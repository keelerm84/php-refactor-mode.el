;;; package -- summary
;;; Commentary:

;;; Code:

(require 'thingatpt)
(require 'locate)

;; (defvar php-refactor-command "refactor.phar")
;; (defvar php-refactor-patch-command "patch -p1")

(defgroup php-refactor nil
  "Quickly and safely perform common refactorings."
  :group 'tools
  :group 'convenience)

(defcustom php-refactor-command "refactor.phar"
  "Define the command used for executing the refactoring."
  :group 'php-refactor
  :type 'symbol)

(defcustom php-refactor-patch-command "patch -p1"
  "Define the command used for applying the patch."
  :group 'php-refactor
  :type 'symbol)

(defcustom php-refactor-keymap-prefix (kbd "C-c r")
  "php-refactor keymap prefix."
  :group 'php-refactor
  :type 'string)

(defvar php-refactor-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "lv") 'php-refactor--convert-local-to-instance-variable)
      (define-key prefix-map (kbd "rv") 'php-refactor--rename-local-variable)
      (define-key prefix-map (kbd "em") 'php-refactor--extract-method)
      (define-key prefix-map (kbd "ou") 'php-refactor--optimize-use)
      (define-key map php-refactor-keymap-prefix prefix-map))
    map)
  "Keymap for php-refactor mode.")

(define-minor-mode php-refactor-mode
  "Minor more to quickly and safely perform common refactorings."
  nil " Refactor" php-refactor-mode-map)

(defun php-refactor--convert-local-to-instance-variable ()
  "Convert a local variable into an instance variable of the class."
  (interactive)
  (php-refactor--run-command
   "convert-local-to-instance-variable"
   (list (buffer-file-name) (number-to-string (locate-current-line-number)) (concat "'" (word-at-point) "'"))))

(defun php-refactor--optimize-use ()
  "Optimizes the use of Fully qualified names in a file."
  (interactive)
  (php-refactor--run-command "optimize-use" (list (buffer-file-name))))

(defun php-refactor--extract-method (begin end)
  "Extract the selected region into a separate method.

BEGIN is the starting position of the selected region.
END is the ending position of the selected region."
  (interactive "r")
  (let ((region-start (number-to-string (line-number-at-pos begin)))
        (region-end (number-to-string (line-number-at-pos end))))
    (php-refactor--run-command
     "extract-method"
     (list (buffer-file-name) (concat region-start "-" region-end) "newMethomdName"))))

(defun php-refactor--rename-local-variable ()
  "Rename an existing local variable to the specified new name."
  (interactive)
  (php-refactor--run-command
   "rename-local-variable"
   (list (buffer-file-name) (number-to-string (locate-current-line-number)) (concat "'" (word-at-point) "'") "renamed")))

(defun php-refactor--run-command (method args)
  "Execute the given refactoring command and apply the resulting patch.

METHOD specifies the refactoring method to run.
ARGS contains a list of flags required for the specific method to run."

  ;; TODO We need to make sure the file is saved first.  Otherwise, we can't do this.
  (shell-command
   (mapconcat 'identity
              (list php-refactor-command method
                    (mapconcat 'identity args " ") "| tee ~/patch |" php-refactor-patch-command) " ")
   nil)
  (revert-buffer nil t))

(provide 'php-refactor-mode)

;;; emacs-php-refactor ends here
