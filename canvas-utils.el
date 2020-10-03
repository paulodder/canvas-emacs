(require 'request)
(require 'json)

(defvar canvas-baseurl nil "Base url of canvas environment")
(defvar canvas-token nil "Canvas token")
(defvar canvas--courses nil "List of courses.")



(defun canvas--list-courses (&optional force-reload)
  (if (and canvas--courses
           (not force-reload))
      canvas--courses
    (canvas--reload-courses)))

(defun canvas--reload-courses ()
  (let ((courses (canvas--request "/api/v1/courses")))
    (setq canvas--courses courses)
    courses))

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
