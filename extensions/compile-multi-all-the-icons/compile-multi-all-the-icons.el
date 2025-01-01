;;; compile-multi-all-the-icons.el --- Affixate `compile-multi' with icons -*- lexical-binding: t; -*-

;; Copyright (C) 2022  mohsin kaleem

;; Author: mohsin kaleem <mohkale@kisara.moe>
;; Keywords: tools, compile, build
;; Package-Requires: ((emacs "28.0") (all-the-icons-completion "0.0.1"))
;; Version: 0.7
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

;; This extension package adds icons to `compile-multi'. This relies on the
;; external `all-the-icons-completion' package to associate the `compile-multi'
;; completion kind with an icon generation function.

;;; Code:

(require 'all-the-icons)
(require 'all-the-icons-completion)

(defgroup compile-multi-all-the-icons nil
  "Icon affixation for `compile-multi' completions."
  :group 'compile-multi)

(defcustom compile-multi-all-the-icons-alist
 '(;; projection-multi target generators.
   (cmake        all-the-icons-fileicon "cmake"            :face all-the-icons-red)
   (ctest        all-the-icons-fileicon "cmake"            :face all-the-icons-lorange)
   (gradle       all-the-icons-fileicon "gradle"           :face all-the-icons-blue)
   (make         all-the-icons-fileicon "gnu"              :face all-the-icons-red-alt)
   (npm          all-the-icons-fileicon "npm"              :face all-the-icons-red)
   (project      all-the-icons-fileicon "npm"              :face all-the-icons-red)
   (yarn         all-the-icons-octicon "repo"              :face all-the-icons-yellow)
   (tox          all-the-icons-alltheicon "python"         :height 1.0 :face all-the-icons-dblue)
   (poetry       all-the-icons-alltheicon "python"         :height 1.0 :face all-the-icons-dblue)
   ;; Types we recommend users override.
   (emacs        all-the-icons-fileicon "elisp"            :height 1.0 :v-adjust -0.1 :face all-the-icons-purple)
   (local        all-the-icons-faicon   "shield"           :face all-the-icons-lblue)
   ;; The default. Omit this to not have any icons when none are configured.
   (t            all-the-icons-fileicon "powershell"       :face all-the-icons-blue))
 "Icon configuration for `compile-multi' completions."
 :group 'compile-multi-all-the-icons
 :type '(list (cons (choice (symbol :tag "Type")
                            (const :tag "Default" t))
                    sexp)))

(cl-defmethod all-the-icons-completion-get-icon (cand (_cat (eql compile-multi)))
  "Return the icon for the candidate CAND of completion category `compile-multi'."
  (let* ((type (get-text-property 0 'consult--type cand))
         (icon
          (or (when type
                (alist-get type compile-multi-all-the-icons-alist))
              (alist-get t compile-multi-all-the-icons-alist))))
    (when icon
      (concat
       (apply (car icon) (cdr icon))
       " "))))

(provide 'compile-multi-all-the-icons)
;;; compile-multi-all-the-icons.el ends here

