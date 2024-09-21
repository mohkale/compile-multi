;;; compile-multi-nerd-icons.el --- Affixate `compile-multi' with nerd icons -*- lexical-binding: t; -*-

;; Copyright (C) 2022  mohsin kaleem

;; Author: mohsin kaleem <mohkale@kisara.moe>
;; Keywords: tools, compile, build
;; Package-Requires: ((emacs "28.0") (nerd-icons-completion "0.0.1"))
;; Version: 0.6
;; Homepage: https://github.com/mohkale/compile-multi

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

;;; Commentary:

;; This extension package adds nerd icons to `compile-multi'. This relies on the
;; external `nerd-icons-completion' package to associate the `compile-multi'
;; completion kind with an icon generation function.

;;; Code:

(require 'nerd-icons)
(require 'nerd-icons-completion)

(defgroup compile-multi-nerd-icons nil
  "Icon affixation for `compile-multi' completions."
  :group 'compile-multi)

(defcustom compile-multi-nerd-icons-alist
  '(;; projection-multi target generators.
    (cmake        nerd-icons-sucicon "nf-seti-makefile"            :face nerd-icons-red)
    (ctest        nerd-icons-sucicon "nf-seti-makefile"            :face nerd-icons-lorange)
    (gradle       nerd-icons-sucicon "nf-seti-gradle"              :face nerd-icons-blue)
    (make         nerd-icons-devicon "nf-dev-gnu"                  :face nerd-icons-red-alt)
    (npm          nerd-icons-sucicon "nf-seti-npm"                 :face nerd-icons-red)
    (project      nerd-icons-sucicon "nf-seti-project"             :face nerd-icons-red)
    (yarn         nerd-icons-sucicon "nf-seti-yarn"                :face nerd-icons-yellow)
    (tox          nerd-icons-sucicon "nf-seti-python"              :face nerd-icons-dblue)
    (poetry       nerd-icons-sucicon "nf-seti-python"              :face nerd-icons-dblue)
    ;; Types we recommend users override.
    (emacs        nerd-icons-sucicon "nf-custom-emacs"             :face nerd-icons-purple)
    (local        nerd-icons-mdicon  "nf-md-shield"                :face nerd-icons-blue)
    ;; The default. Omit this to not have any icons when none are configured.
    (t            nerd-icons-sucicon "nf-seti-powershell"          :face nerd-icons-blue))
  "Icon configuration for `compile-multi' completions."
  :group 'compile-multi-nerd-icons
  :type '(list (cons (choice (symbol :tag "Type")
                             (const :tag "Default" t))
                     sexp)))

(cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql compile-multi)))
  "Return the icon for the candidate CAND of completion category `compile-multi'."
  (let* ((type (get-text-property 0 'consult--type cand))
         (icon
          (or (when type
                (alist-get type compile-multi-nerd-icons-alist))
              (alist-get t compile-multi-nerd-icons-alist))))
    (when icon
      (concat
       (apply (car icon) (cdr icon))
       " "))))

(provide 'compile-multi-nerd-icons)
;;; compile-multi-nerd-icons.el ends here

