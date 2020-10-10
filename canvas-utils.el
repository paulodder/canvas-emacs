(require 'request)
(require 'json)

(defcustom canvas-baseurl nil "Base url of canvas environment")
(defcustom canvas-token nil "Canvas token")


(defvar canvas--courses nil "List of your courses.")
(defvar canvas--courseid2modules '()
  "List with cons cells of form (courseid . <seq of module jsons (incl items)>)")
(defvar canvas--userid nil "User id")



(defun canvas--list-courses (&optional force-reload)
  (if (and canvas--courses
           (not force-reload))
      canvas--courses
    (canvas--reload-courses)))

(defun canvas--reload-courses ()
  (setq canvas--courses (cl-sort (canvas--request (format "/api/v1/users/%s/courses"
                                                          (canvas--get-user-id))
                                                  "GET"
                                                  '((per_page . 100)))
                                 (lambda (a b)
                                   (string-greaterp (canvas--json-find '(start_at)
                                                                       a)
                                                    (canvas--json-find '(start_at)
                                                                       b))))))

(defun canvas--list-modules (courseid &optional force-reload)
  "lists module jsons for given courseid"
  (let ((course-modules (canvas--json-find `(,courseid)
                                           canvas--courseid2modules)))
    (if (and course-modules
             (not force-reload))
        course-modules
      (canvas--reload-modules courseid))))

(defun canvas--reload-modules (courseid)
  "reloads modules for given courseid and sets them accordingly in canvas--courseid2modules"
  (let ((courseid2modules-without-current (seq-filter (lambda (courseid-module)
                                                        (not (equal courseid (car courseid-module))))
                                                      canvas--courseid2modules))
        (modules (canvas--request (format "/api/v1/courses/%s/modules" courseid)
                                  "GET"
                                  '((per_page . 100)
                                    ("include[]" . "items")))))
    (setq canvas--courseid2modules (append courseid2modules-without-current
                                           `((,courseid . ,modules))))
    modules))



(defun canvas--flatten (seq)
  (cond
   ((null seq) nil)
   ((atom seq)
    (list seq))
   (t (append (car seq)
              (canvas--flatten (cdr seq))))))





(defun canvas--get-user-id (&optional force-reload)
  (if (and canvas--userid
           (not force-reload))
      canvas--userid
    (canvas--reload-userid)))

(defun canvas--reload-userid ()
  (setq canvas--userid (canvas--json-find '(id)
                                          (canvas--request "/api/v1/users/self"))))

(defun canvas--encode-params (params)
  "encodes params in GET format"
  (concat "?"
          (string-join (seq-map (lambda (keyval)
                                  (format "%s=%s"
                                          (car keyval)
                                          (cdr keyval)))
                                params)
                       "&")))

;; adapted from https://github.com/titaniumbones/org-lms
(defun canvas--request (query &optional request-type request-params
                              file)
  "Send QUERY to canvas-base-url with http request type REQUEST-TYPE.
  Optionally send REQUEST-PARAMS as JSON data, and write results to FILE, which should be a full path.
    "
  (let ((json-params (json-encode request-params))
        (target (concat canvas-baseurl
                        query
                        (if (string= request-type "GET")
                            (canvas--encode-params request-params)))))
    (message (format "%s" target))
    (if canvas-token
        (request-response-data (request target
                                 :type (if request-type request-type "GET"):headers`(("Authorization" . ,(concat "Bearer " canvas-token))
                                                                                     ("Content-Type" . "application/json"))
                                 :sync t
                                 :data (if json-params json-params nil):encoding'no-conversion
                                 :parser (lambda ()
                                           (if (and (boundp 'file)
                                                    file)
                                               (write-region (buffer-string)
                                                             nil
                                                             file))
                                           (json-read)):success
                                 (cl-function (lambda (&key data &allow-other-keys)
                                                data))
                                 :error (cl-function (lambda (&key error-thrown data status &allow-other-keys)
                                                       (message "NO PAYLOAD: %s" error-thrown)))))
      (user-error "Please set a value for for `canvas-token' in order to complete API calls"))))




(cl-defun canvas--json-find
    (keys json
          &key
          (ignore-if-missing t))
  "Given key sequence and json, return corresponding element from json"
  (cond
   ((= (length keys) 0) json)
   ((= (length json) 0)
    (unless ignore-if-missing
      (error (format "Key: %s not found"
                     (first keys)))))
   (t (if (eq (first keys) (car (first json)))
          (canvas--json-find (rest keys)
                             (rest (first json)))
        (canvas--json-find keys
                           (rest json))))))

(cl-defmacro canvas--with-json-bind
    ((&rest defs) json
     &body
     body)
  (declare (indent 2))
  (let ((json-sym (gensym "json")))
    `(let ((,json-sym ,json))
       (let ,(loop for
                   (sym path)
                   in
                   defs
                   collect
                   `(,sym
                     (canvas--json-find ',path ,json-sym)))
         ,@body))))

(defun canvas--get-by-key (key-path value jsons)
  "given key-path, its desired value, and list of jsons to choose
from, returns the json such that its key-path value equals value"
  (cl-find value
           jsons
           :key #'(lambda (j)
                    (canvas--json-find key-path j)):test#'equal))


(defun canvas--choose-course ()
  "prompts user to select a course, returns course id"
  (canvas--choose-from-jsons "course"
                             '(name)
                             (canvas--list-courses)))

(defun canvas--choose-assignment (courseid)
  "for given course id prompts user to select course"
  (canvas--choose-from-jsons "assignment"
                             '(name)
                             (canvas--request (format "/api/v1/courses/%s/assignments"
                                                      courseid
                                                      '((per_page . 100))))))


(cl-defun canvas--choose-from-jsons
    (name path-to-show
          jsons
          &key
          (path-to-return '(id)))
  "Given list of jsons, name of what is going to be picked, and
the path-to-show determining which value of each json to show in
the minibuffer, returns the id of the chosen object
e.g. given a list of course jsons, \"course\", and '(name), this function will
prompt the user to select a course based on a list of course names"
  (let* ((show-values (seq-map (lambda (j)
                                 (canvas--json-find path-to-show j))
                               jsons))
         (show-value2return-value (seq-map (lambda (j)
                                             `(,(canvas--json-find path-to-show j)
                                               . ,(canvas--json-find path-to-return j)))
                                           jsons)))
    (let ((chosen-json (completing-read (concat (capitalize name)
                                                ": ")
                                        show-values
                                        nil
                                        t)))
      (cdr (cl-find chosen-json show-value2return-value
                    :key #'first
                    :test #'equal)))))

(defun canvas--choose-announcement (courseid)
  "Given courseid, prompts user to select announcement, returns corresponding json"
  (let* ((announcement-jsons (canvas--request "/api/v1/announcements/"
                                              "GET"
                                              `(("context_codes[]" . ,(concat "course_"
                                                                              (int-to-string courseid))))))
         (announcement-id (canvas--choose-from-jsons "announcement"
                                                     '(title)
                                                     announcement-jsons)))
    (canvas--get-by-key '(id)
                        announcement-id
                        announcement-jsons)))

(defun canvas--choose-module-item (courseid)
  "returns url for selected module item"
  (let ((module-items (canvas--flatten (seq-map (lambda (moduleid2items)
                                                  (let ((module-name (canvas--json-find '(name)
                                                                                        moduleid2items))
                                                        ;; (module-id (canvas--json-find '(id)
                                                        ;;                               moduleid2items))
                                                        (module-items (canvas--json-find '(items)
                                                                                         moduleid2items)))
                                                    (seq-map (lambda (module-item)
                                                               (let ((module-item-id (canvas--json-find '(id)
                                                                                                        module-item)))
                                                                 `((url . ,(canvas--json-find '(html_url)
                                                                                              module-item))
                                                                   (fancy-name . ,(string-join (list module-name
                                                                                                     (canvas--json-find '(title)
                                                                                                                        module-item))
                                                                                               "/")))))
                                                             module-items)))
                                                (canvas--list-modules courseid)))))
    (canvas--choose-from-jsons "module"
                               '(fancy-name)
                               module-items
                               :path-to-return '(url))))


(defun canvas--render-json (path-to-name path-to-content json)
  "given json object renders it in a separate buffer named according to path-to-name and"
  (let* ((buffer-name (canvas--json-find path-to-name json))
         (buffer-content-raw (canvas--json-find path-to-content json))
         (buffer-content (replace-in-string "\\" "" buffer-content-raw))
         (out-buf (get-buffer-create buffer-name)))
    (progn
      (set-buffer out-buf)
      (insert buffer-content)
      (shr-render-region (point-min)
                         (point-max))
      (select-window (display-buffer out-buf))
      (goto-char (point-min)))))

;;; exposed functions

(defun canvas-view-course ()
  (interactive)
  (let* ((courseid (canvas--choose-course))
         (front-page-json (canvas--request (format "/api/v1/courses/%s/front_page" courseid))))
    (canvas--render-json '(url)
                         '(body)
                         front-page-json)))

(defun canvas-view-assignment ()
  (interactive)
  (let* ((courseid (canvas--choose-course))
         (ass-id (canvas--choose-assignment courseid)))
    (canvas--render-json '(name)
                         '(description)
                         (canvas--request (format "/api/v1/courses/%s/assignments/%s"
                                                  courseid ass-id)))))

(defun canvas-view-announcement ()
  (interactive)
  (let* ((courseid (canvas--choose-course))
         (announcement-json (canvas--choose-announcement courseid)))
    (canvas--render-json '(title)
                         '(message)
                         announcement-json)))


(defun canvas-view-module ()
  (interactive)
  (let* ((courseid (canvas--choose-course))
         (url (canvas--choose-module-item courseid)))
    (funcall shr-external-browser url)))



(provide 'canvas-utils)
