;;; compile-multi-embark.el --- Integration for `compile-multi' and `embark' -*- lexical-binding: t; -*-

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: project, convenience
;; Package-Requires: ((emacs "29.1") (compile-multi "0.4") (embark "0.22.1"))
;; Version: 0.5
;; Homepage: https://github.com/mohkale/compile-multi

;; Copyright (C) 2023  Mohsin Kaleem

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package defines a keymap and embark transformer helper to integrate
;; compile-multi into embark. This lets you set command actions that will
;; run on the underlying command shown in a `compile-multi' session.

;;; Code:

(require 'embark)

(defgroup compile-multi-embark nil
  "Integration between `compile-multi' and `embark'."
  :group 'compile-multi)

(defvar compile-multi-embark-command-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-general-map)
    map)
  "Keymap active in `compile-multi' `embark' sessions.")



(defun compile-multi-embark-transformer (type target)
  "Transformer for `embark' to convert a `compile-multi' TARGET into a command.
TYPE should always be `compile-multi'."
  (cl-assert (eq type 'compile-multi) 'show-args
             "This transformer should only be used with `compile-multi'.")
  (let ((command (get-text-property 0 'compile-multi--task target)))
    (cl-assert command 'show-args "Encountered compile-multi candidate with no command")
    (cons type command)))

;;;###autoload
(define-minor-mode compile-multi-embark-mode
  "Minor mode enabling `embark' actions for `compile-multi' minibuffer sessions."
  :global t :group 'compile-multi-embark
  (if compile-multi-embark-mode
      (progn
        (push '(compile-multi compile-multi-embark-command-map) embark-keymap-alist)
        (push `(compile-multi . ,#'compile-multi-embark-transformer)
              embark-transformer-alist))
    (setq embark-keymap-alist (assq-delete-all 'compile-multi embark-keymap-alist)
          embark-transformer-alist (assq-delete-all 'compile-multi embark-transformer-alist))))

(provide 'compile-multi-embark)
;;; compile-multi-embark.el ends here
