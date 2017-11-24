(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq php-refactor-root-path project-directory))

(add-to-list 'load-path php-refactor-root-path)

(require 'php-mode)
(require 'php-refactor-mode)
(require 'espuds)
(require 'ert)

(Before
 (switch-to-buffer
  (get-buffer-create "*php-refactor*"))
 (erase-buffer)
 (transient-mark-mode 1)
 (cua-mode 0)
 (php-refactor-mode 0)
 (setq set-mark-default-inactive nil)
 (deactivate-mark))

(After)
