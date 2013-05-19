(defvar pt-templates-directory "~/.emacs.d/templates"
  "Where templates are stored.")

(defvar pt-template-var-delimiter "$!"
  "Strings used to delimit variable names.")

(defun pt-template-var-regexp ()
  "Create the regexp which identifies a variable in a template."
  (concat pt-template-var-delimiter
          "\\([^" pt-template-var-delimiter " \t\n]+\\)"
          pt-template-var-delimiter))

(defvar pt-value-alist nil
  "A placeholder where replacement values will be kept. This is
  let-bound when `pt-create-project-from-template' is called and
  should not be edited directly.")

(defun pt-replace-all (from to str)
  "Simply replace all occurrences of FROM with TO in STR."
  (while (string-match from str)
    (set 'str (replace-match to t t str)))
  str)

(defun pt-get-replacement (key)
  "Find, or ask for and save, the replacement value for KEY."
  (let ((replacement (assoc key pt-value-alist)))
    (when (eq replacement nil)
      (set 'replacement
           `(,key . ,(read-from-minibuffer (concat key ": "))))
      (add-to-list 'pt-value-alist replacement))
    replacement))

(defun pt-parse-file-name (filename)
  "Parse FILENAME and replace all __variables__ with values
provided by the user."
  (while (string-match (pt-template-var-regexp) filename)
    (let* ((tpl-var (match-string 1 filename))
           (replacement-value (pt-get-replacement tpl-var)))
      (set 'filename (replace-match (cdr replacement-value) t t
                                    filename))))

  (let ((noext (file-name-sans-extension filename))
        (ext (file-name-extension filename t)))
    (set 'noext (pt-replace-all "\\." "/" noext))
    (concat noext ext)))

(defun pt-parse-file (file)
  "Parse FILE and replace all __variables__ with values provided by
the user."
  (insert-file-contents file)

  (while (re-search-forward (pt-template-var-regexp) nil t)
    (let* ((tpl-var (match-string 1))
           (replacement-value (pt-get-replacement tpl-var)))
      (replace-match (cdr replacement-value) t t))))

(defun pt-parse-and-copy-file (src dst)
  "Copy SRC to DST, but, if necessary, parse the file and filename
first."
  (let* ((parsed-dst (pt-parse-file-name dst))
         (parsed-dst-dir (file-name-directory parsed-dst)))
    (when (not (file-exists-p parsed-dst-dir))
      (make-directory parsed-dst-dir t))

    (with-temp-file parsed-dst
        (pt-parse-file src))))

(defun pt-copy-directory (directory to)
  "Copy template directory DIRECTORY to the location indicated by
TO and parse both paths and files in the process."
  (let ((files (directory-files directory t "[^.]\\{1,2\\}$" t)))
    (while files
      (let* ((src-filename (car files))
             (dst-filename
              (concat to "/" (file-name-nondirectory src-filename))))
        (if (file-directory-p src-filename)
            (pt-copy-directory src-filename dst-filename)
          (if (string-equal (file-name-extension src-filename) "etpl")
              (pt-parse-and-copy-file
               src-filename (file-name-sans-extension dst-filename))
            (if (not (file-exists-p to))
                (make-directory to t)
              (unless (file-directory-p to)
                (error
                 (concat "Cannot create project at %s, file "
                         "already exists and is not a directory.") to)))

            (copy-file src-filename dst-filename))))
      (set 'files (cdr files)))))

;;;###autoload
(defun pt-create-project-from-template (template destination)
  "Take TEMPLATE, copy it to DESTINATION and replace any
occurrences of __variables__ with user-povided values."
  (interactive "MTemplate: \nGDestination: ")
  (let ((pt-value-alist))
    (pt-copy-directory
     (concat pt-templates-directory "/" template) destination)))

(provide 'project-template)
