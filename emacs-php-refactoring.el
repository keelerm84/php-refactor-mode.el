;;; package -- summary
;;; Commentary:

;;; Code:
(defvar php-refactoring-command "refactor.phar")
(defvar php-refactoring-patch-command "patch -p1")

(defun php-refactoring--convert-local-to-instance-variable ()
  "Convert a local variable into an instance variable of the class."
  (interactive)
  (php-refactoring--run-command
   "convert-local-to-instance-variable"
   (list (buffer-file-name) (number-to-string (locate-current-line-number)) (concat "'" (word-at-point) "'"))))

(defun php-refactoring--optimize-use ()
  "Optimizes the use of Fully qualified names in a file."
  (interactive)
  (php-refactoring--run-command "optimize-use" (list (buffer-file-name))))

(defun php-refactoring--extract-method (begin end)
  "Extract the selected region into a separate method.

BEGIN is the starting position of the selected region.
END is the ending position of the selected region."
  (interactive "r")
  (let ((region-start (number-to-string (line-number-at-pos begin)))
        (region-end (number-to-string (line-number-at-pos end))))
    (php-refactoring--run-command
     "extract-method"
     (list (buffer-file-name) (concat region-start "-" region-end) "newMethomdName"))))

(defun php-refactoring--rename-local-variable ()
  "Rename an existing local variable to the specified new name."
  (interactive)
  (php-refactoring--run-command
   "rename-local-variable"
   (list (buffer-file-name) (number-to-string (locate-current-line-number)) (concat "'" (word-at-point) "'") "renamed")))

(defun php-refactoring--run-command (method args)
  "Execute the given refactoring command and apply the resulting patch.

METHOD specifies the refactoring method to run.
ARGS contains a list of flags required for the specific method to run."

  ;; TODO We need to make sure the file is saved first.  Otherwise, we can't do this.
  (shell-command
   (mapconcat 'identity
              (list php-refactoring-command method
                    (mapconcat 'identity args " ") "| tee ~/patch |" php-refactoring-patch-command) " ")
   nil)
  (revert-buffer nil t))

(provide 'emacs-php-refactoring)

;;; emacs-php-refactoring ends here
