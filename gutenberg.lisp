;;;; Downloading and processing text from Project Gutenberg
(uiop:define-package :fare-scripts/gutenberg
  (:use :cl :uiop :fare-utils
	:drakma :inferior-shell :cl-scripting :cl-launch/dispatch)
  (:export
   #:get-gutenberg-urls))
   #:download-gutenberg-files))

(in-package :fare-scripts/gutenberg)

#| ;;; Downloading files from Project Gutenberg
https://www.gutenberg.org/wiki/Gutenberg:Information_About_Robot_Access_to_our_Pages
https://www.exratione.com/2014/11/how-to-politely-download-all-english-language-text-format-files-from-project-gutenberg/
http://webapps.stackexchange.com/questions/12311/how-to-download-all-english-books-from-gutenberg
|#
(exporting-definitions

(defun get-gutenberg-urls
    (&key
       (directory (subpathname (user-homedir-pathname) "gutenberg/"))
       (language "en")
       (format "txt"))
  (let ((catdir (subpathname directory "catalog/")))
    ;; Get a list of all the files we want
    (with-current-directory (catdir)
      (run/i
       `(wget
	 "-w" 2 "-m"
	 ("--directory-prefix=" ,catdir)
	 ("http://www.gutenberg.org/robot/harvest?"
	  ,@(when format `("filetypes[]=" ,format "&"))
	  ,@(when language `("langs[]=" ,language)))))
      ;; Process the downloaded HTML link lists into
      ;; a single sorted file of zipfile URLs, one per line.
      (prog1
	  (run
	   `(pipe
	     (grep "-oh" "http://[a-zA-Z0-9./]*.zip"
		   ,@(directory (subpathname catdir "www.gutenberg.org/robot/")))
	     (sort -u))
	   :output :lines)
	;; Get rid of the downloaded harvest files now that we have what we want.
	#+(or)
	(uiop:delete-directory-tree
	 (subpathname catdir "www.gutenberg.org/")
	 :validate (lambda (x) (search "/catalog/www.gutenberg.org/" (namestring x))))))))

(defun download-all-urls (urls &key directory)
  (when directory (ensure-directory-exists directory))
  (with-current-directory (directory)
    ;; XXXXXX
    ))

(defun download-gutenberg-files
    (&key
       (directory (subpathname (user-homedir-pathname) "gutenberg/"))
       (language "en")
       (format "txt"))
  (let ((catdir (subpathname directory "catalog/")))
    ))

);exporting-definitions

(register-commands :fare-scripts/gutenberg)
