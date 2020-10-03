(require 'request)
(require 'json)

(defvar canvas-baseurl nil "Base url of canvas environment")
(defvar canvas-token nil "Canvas token")
(defvar canvas--courses nil "List of courses.")
(defvar canvas--userid nil "User id")



(defun canvas--list-courses (&optional force-reload)
  (if (and canvas--courses
           (not force-reload))
      canvas--courses
    ;; (if active-only
    (canvas--reload-courses)))

(defun canvas--reload-courses ()
  (let ((courses (canvas--request "/api/v1/courses")))
    (setq canvas--courses courses)
    courses))

(defun canvas--get-user-id (&optional force-reload)
  (if (and canvas--userid
           (not force-reload))
      canvas--userid
    (canvas--reload-userid)))

(defun canvas--reload-userid ()
  (setq canvas--userid (canvas--json-find '(id)
                                          (canvas--request "/api/v1/users/self"))))


;; adapted from https://github.com/titaniumbones/org-lms
(defun canvas--request (query &optional request-type request-params
                              file)
  "Send QUERY to canvas-base-url with http request type REQUEST-TYPE.
  Optionally send REQUEST-PARAMS as JSON data, and write results to FILE, which should be a full path.
    "
  (let ((json-params (json-encode request-params))
        (target (concat canvas-baseurl query)))
    (if canvas-token
        (request-response-data (request target
                                 :type "GET"
                                 :headers `(("Authorization" . ,(concat "Bearer " canvas-token))
                                            ("Content-Type" . "application/json")):sync
                                 t
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



(defun canvas--json-find (keys json)
  "Given key sequence and json, return corresponding element from json"
  (cond
   ((= (length keys) 0) json)
   ((= (length json) 0)
    (error (format "Key: %s not found"
                   (first keys))))
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

;; (map 'vector (lambda (j) (canvas--json-find '(end_at) j)) (canvas--list-courses t))
