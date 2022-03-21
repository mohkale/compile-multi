;;; compile-multi-make.el --- `compile-multi' task generator for Makefiles -*- lexical-binding: t; -*-

;; Copyright (C) 2022  mohsin kaleem

;; Author: mohsin kaleem <mohkale@kisara.moe>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)

(defgroup compile-multi-make nil
  "Helpers for `compile-multi' and Makefiles."
  :group 'compile-multi)

(defcustom compile-multi-make-cache-targets t
  "When true cache the targets of the current project."
  :type 'boolean)

(defvar compile-multi-make--targets-cache (make-hash-table))

(defun compile-multi-make--targets (makefile)
  "Parse out all the Makefile targets in MAKEFILE."
  (let (make-targets
        (modtime (and compile-multi-make-cache-targets
                      (file-attribute-modification-time (file-attributes makefile 'integer)))))
    (setq make-targets
          (or (and compile-multi-make-cache-targets
                   (when-let ((entry (gethash default-directory compile-multi-make--targets-cache)))
                     (when (time-less-p modtime (car entry))
                       (cdr entry))))
              ;; Taken from [[https://github.com/abo-abo/helm-make/blob/ebd71e85046d59b37f6a96535e01993b6962c559/helm-make.el#L284][helm-make/helm--make-target-list-default]].
              (let (targets)
                (with-temp-buffer
                  (insert-file-contents makefile)
                  (goto-char (point-min))
                  (while (re-search-forward "^\\([^: \n]+\\) *:\\(?: \\|$\\)" nil t)
                    (let ((str (match-string 1)))
                      (unless (string-match "^\\." str)
                        (push str targets)))))
                (nreverse targets))))
    (when compile-multi-make-cache-targets
      (puthash default-directory (cons modtime make-targets) compile-multi-make--targets-cache))
    make-targets))

;;;###autoload
(defun compile-multi-make-targets (&optional file-name)
  "Retun a list of `compile-multi' targets for the Makefile FILE-NAME."
  (cl-loop for it in (compile-multi-make--targets (or file-name "Makefile"))
           collect (cons (concat "make:" it)
                         (concat "make " (shell-quote-argument it)))))

(provide 'compile-multi-make)
;;; compile-multi-make.el ends here
