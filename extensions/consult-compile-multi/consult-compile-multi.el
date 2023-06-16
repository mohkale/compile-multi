;;; consult-compile-multi.el --- `consult'-ing read support for `compile-multi' -*- lexical-binding: t; -*-

;; Copyright (C) 2022  mohsin kaleem

;; Author: mohsin kaleem <mohkale@kisara.moe>
;; Keywords: tools, compile, build
;; Package-Requires: ((emacs "28.0") ("compile-multi" "0.3") (consult "0.34"))
;; Version: 0.1
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

;; This extension package allows you to run `compile-multi' with `consult--read'.
;; This provides extra functionality atop the base completing read interface such
;; as narrowing or having the ability to further customize the completion session.

;;; Code:

(require 'consult)
(require 'compile-multi)

(defgroup consult-compile-multi nil
  "Run `compile-multi' through `consult'ing-read."
  :group 'compile-multi)

(defcustom consult-compile-multi-narrow
  '((?p "Project" project)
    (?c "CMake" cmake ctest)
    (?t "Test" ctest))
  "Narrowing configuration for `compile-multi'.
This configuration lets you filter down completion candidates exposed by
`compile-multi'. Each entry should be of the form (char name types...)."
  :type '(alist
          :key-type (character :tag "Character to narrow on")
          :value-type
          (cons (string :tag "Title of narrow")
                (repeat (list (symbol :tag "compile-multi types matching narrowing config")))))
  :group 'consult-compile-multi)

(defun consult-compile-multi--narrow ()
  "Calculate a `compile-multi' narrowing configuration."
  (list :predicate
        (lambda (cand)
          (when-let ((type-list
                      (cdr (alist-get consult--narrow consult-compile-multi-narrow))))
            (member (get-text-property 0 'consult--type (car cand)) type-list)))
        :keys
        (mapcar (pcase-lambda (`(,x ,y ,_)) (cons x y))
                consult-compile-multi-narrow)))

(cl-defmethod compile-multi-read-actions ((_interface (eql consult)) tasks)
  "Interactively select a `compile-multi' action from TASKS using consult."
  (consult--read
   tasks
   :prompt "Compile: "
   :category 'compile-multi
   :lookup #'consult--lookup-cons
   :history 'compile-multi-history
   :group #'compile-multi--group-function
   :narrow (consult-compile-multi--narrow)
   :annotate
   (when compile-multi-annotate-cmds
     (lambda (task)
       (compile-multi--annotation-function
        (consult--lookup-cons task tasks))))))



;;; Minor mode to enable consult.

;;;###autoload
(define-minor-mode consult-compile-multi-mode
  "Run `compile-multi' with `consult'."
  :global t :group 'consult-compile-multi
  (setq compile-multi-interface
        (if consult-compile-multi-mode
            'consult
          nil)))

(provide 'consult-compile-multi)
;;; consult-compile-multi.el ends here
