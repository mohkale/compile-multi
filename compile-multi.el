;;; compile-multi.el --- A multi target interface to compile -*- lexical-binding: t; -*-

;; Copyright (C) 2022  mohsin kaleem

;; Author: mohsin kaleem <mohkale@kisara.moe>
;; Keywords: tools, compile, build
;; Package-Requires: ((emacs "28.1"))
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

;; Multi target interface to `compile'.
;;
;; This package exposes facilities for generating a collection of compilation
;; commands for the current buffer or project and interactively select one to
;; run. This can be plugged into various build frameworks such as Make or CMake
;; to automatically determine the list of available targets and let you
;; interactively select one to run.

;;; Code:

(require 'seq)
(require 'subr-x)

(defgroup compile-multi nil
  "A multi target interface to compile."
  :group 'compilation)



;;; Completion interface

(defcustom compile-multi-interface nil
  "Set the default interface for `compile-multi'.
To override the interface you must define a variant of
`compile-multi-read-actions' that accepts an interface argument matching the
value set here."
  :type '(optional symbol))

(defvar compile-multi-history nil
  "History of completions chosen with `compile-multi'.")

(defcustom compile-multi-annotate-cmds t
  "Affixate `compile-multi' to show the command being compiled."
  :type 'boolean)

(defcustom compile-multi-annotate-string-cmds t
  "Auto annotate string based commands in `compile-multi-config'.
When true if a command in `compile-multi-config' is a string or produces a
string then generate an annotation for it automatically. This option only
has significance when `compile-multi-annotate-cmds' is true."
  :type 'boolean)

(defcustom compile-multi-annotate-limit 48
  "Truncate any annotations longer than this limit.
Set to nil to disable truncation."
  :type '(optional integer))

(defcustom compile-multi-group-cmds 'group-and-replace
  "Group commands with the same `compile-multi' root prefix."
  :type '(choice
          (const :tag "Group and remove group prefix." group-and-replace)
          (const :tag "Group candidates." t)
          (const :tag "Never group candidates." nil)))

(defun compile-multi--annotation-function (task)
  "Annotator for TASK."
  (when-let ((cmd (plist-get (cdr task) :annotation)))
    (when (and compile-multi-annotate-limit
               (>= (length cmd) compile-multi-annotate-limit))
      (setq cmd
            (concat
             (string-remove-suffix
              " "
              (substring cmd 0 compile-multi-annotate-limit))
             "…")))
    (concat " "
            (propertize " " 'display `(space :align-to (- right ,(+ 1 (length cmd)))))
            (propertize cmd 'face 'completions-annotations))))

(defun compile-multi--group-function (cand transform)
  "Group function for `compile-multi' on CAND and TRANSFORM."
  (when compile-multi-group-cmds
    (when-let ((type (get-text-property 0 'consult--type cand))
               (type (symbol-name type)))
      (if transform
          (when (eq compile-multi-group-cmds 'group-and-replace)
            (substring cand (1+ (length type))))
        type))))

(cl-defgeneric compile-multi-read-actions (_interface tasks)
  "Interactively select a `compile-multi' action from TASKS."
  (assoc (completing-read
          "Compile: "
          (lambda (string predicate action)
            (if (eq action 'metadata)
                `(metadata
                  (annotation-function
                   . ,(when compile-multi-annotate-cmds
                        (lambda (task)
                          (compile-multi--annotation-function
                           (assoc task tasks)))))
                  (group-function . ,#'compile-multi--group-function)
                  (category . compile-multi))
              (complete-with-action action tasks string predicate)))
          nil t nil 'compile-multi-history)
         tasks))



;;; Task generation

(defcustom compile-multi-forms
  '((file-name . (buffer-file-name))
    (file-dir . (file-name-directory (buffer-file-name)))
    (file-base . (file-name-nondirectory (buffer-file-name)))
    (file-base-no-ext . (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
    (file-ext . (file-name-extension (file-name-nondirectory (buffer-file-name)))))
  "Alist of special let-forms to replace in `compile-multi-config'.
The key is a symbol that may occur in one of the actions of
`compile-multi-config'. The value is a Lisp form that will be evaluated and
then replace key."
  :type '(alist :key-type symbol :value-type (sexp :tag "Expression")))

(defconst compile-multi-config-type
  '(alist :key-type
          (choice (const t :tag "Default")
                  (symbol :tag "Major-mode")
                  (regexp :tag "File/buffer regexp")
                  (sexp :tag "Expression"))
          :value-type
          (repeat
           (choice (string :tag "Shell command")
                   (function :tag "Shell command generator")
                   (repeat (choice string
                                   (sexp :tag "Expression")))
                   ;; (plist :value-type (sexp :tag "Any of the above value types"))
                   (cons (string :tag "Command Name")
                         (sexp :tag "Any of the above value types"))))
          ))

(defcustom compile-multi-config nil
  "Alist of triggers and actions for those triggers."
  :type compile-multi-config-type)

(defcustom compile-multi-dir-local-config nil
  "Variant of `compile-multi-config' to be set in .dir-locals.el ."
  :type compile-multi-config-type)

(defun compile-multi--tasks ()
  "Select the tasks from `compile-multi-config' whose triggers are fired."
  (apply #'append
         (mapcar #'cdr
                 (seq-filter
                  (lambda (it)
                    (let ((trigger (car it)))
                      (cond
                       ((eq trigger t) t)
                       ((symbolp trigger)
                        (derived-mode-p trigger))
                       ((listp trigger)
                        (eval trigger))
                       ((stringp trigger)
                        (string-match-p trigger (buffer-name)))
                       ((functionp trigger)
                        (funcall trigger))
                       (t (error "Unknown trigger type: %s" trigger)))))
                  (append compile-multi-config
                          compile-multi-dir-local-config)))))

(defun compile-multi--fill-tasks (tasks)
  "Convert TASKS values into shell commands.
Returns a cons cell of the completion target and a plist of task properties.
The plist will contain a command and an optional annotation property for task."
  (let (res)
    (dolist (task tasks)
      (cond
       ;; Generate one-or more tasks to replace this task.
       ((functionp task)
        (setq res (append (nreverse
                           (compile-multi--fill-tasks (funcall task)))
                          res)))
       ;; A full command to be called as a standalone task.
       ((stringp (cdr task))
        (push `(,(car task)
                :command ,(cdr task)
                ,@(when compile-multi-annotate-string-cmds
                    (list :annotation (cdr task))))
              res))
       ;; Defer to a lisp-command to compile, not a shell command.
       ;; KLUDGE: I'm not sure about this, it might be a little too hacky.
       ((functionp (cdr task))
        (push `(,(car task)
                :command ,(cdr task)
                :annotation ,(if (eq (car-safe (cdr task)) 'lambda)
                                 "lambda"
                               (format "%s" (cdr task))))
              res))
       ;; Command is a pre-formatted plist, nothing to be done.
       ((and (listp (cdr task))
             (keywordp (cadr task)))
        ;; (cl-assert (plist-get (cdr task) :command) nil
        ;;            "Task %S has a missing command value" task)
        (when-let ((annotation (plist-get (cdr task) :annotation)))
          (plist-put (cdr task) :annotation
                     (when compile-multi-annotate-cmds
                       annotation)))
        (push `(,(car task) ,@(cdr task)) res))
       ;; It's a list that we need to format into a shell-command to run.
       ((listp (cdr task))
        (let ((cmd
               (mapconcat (lambda (it)
                            (or
                             (and (symbolp it)
                                  (let* ((val (or (alist-get it compile-multi-forms)
                                                  it))
                                         (evaluated-value (eval val)))
                                    (unless (stringp evaluated-value)
                                      (error "Failed to stringify task argument %s" val))
                                    evaluated-value))
                             (let ((evaluated-value (eval it)))
                               (unless (stringp evaluated-value)
                                 (error "Failed to stringify task argument %s" it))
                               evaluated-value)))
                          (cdr task) " ")))
          (push `(,(car task)
                  :command ,cmd
                  ,@(when compile-multi-annotate-string-cmds
                      (list :annotation cmd)))
                res)))
       (t (error "Unknown task type: %s" task))))
    (nreverse res)))

(defun compile-multi--add-properties (tasks)
  "Attach a type property to `compile-multi' TASKS."
  (dolist (task tasks)
    (when-let* ((group-pos (string-match-p ":" (car task)))
                (type (intern (substring (car task) 0 group-pos))))
      (add-text-properties 0 1 (list 'consult--type type) (car task)))
    (when-let ((command (plist-get (cdr task) :command)))
      (add-text-properties 0 1 (list 'compile-multi--task command) (car task))))
  tasks)



;;; Main entrypoint

(defcustom compile-multi-default-directory nil
  "Assign `default-directory' of a compilation in `compile-multi'.
If set this function will be called prior to determining compilation triggers
and actions and `default-directory' will be set to the result. If the result
is nil then `default-directory' will not be changed."
  :type '(optional function))

;;;###autoload
(defun compile-multi (&optional query)
  "Multi-target interface to compile.
With optional argument QUERY allow user to modify compilation command before
running."
  (interactive "P")
  (let* ((default-directory (or (and compile-multi-default-directory
                                     (funcall compile-multi-default-directory))
                                default-directory))
         (tasks (thread-first
                  (compile-multi--tasks)
                  (compile-multi--fill-tasks)
                  (compile-multi--add-properties)))
         (compile-cmd (if tasks
                          (plist-get
                           (cdr (compile-multi-read-actions
                                 compile-multi-interface tasks))
                           :command)
                        (read-shell-command "No tasks for compile-multi, run command: "))))
    (cond
     ((stringp compile-cmd)
      (when query
        (setq compile-cmd
              (read-shell-command "Compile command: " compile-cmd)))
      (compile compile-cmd (consp query)))
     ((functionp compile-cmd)
      (if query
          (eval-expression
           (read--expression "Eval: " (format "(%s)" compile-cmd)))
        (funcall compile-cmd)))
     (t (error "Don't know how to run the command %s" compile-cmd)))))

(provide 'compile-multi)
;;; compile-multi.el ends here
