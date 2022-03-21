;;; compile-multi-ruby-thor.el --- `compile-multi' task generator for Ruby Thor projects -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mohsin Kaleem

;; Author: Mohsin Kaleem <mohkale@gmail.com>

;; Copyright (C) 2021  Mohsin Kaleem

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;;; Code:

;;;###autoload
(defun compile-multi-ruby-thor-targets ()
  (let ((bundler
         (when (locate-dominating-file default-directory "Gemfile")
           "bundler exec ")))
    (with-temp-buffer
      (insert
       (shell-command-to-string (concat bundler "thor list")))
      (goto-char (point-min))
      (let (res)
        (save-match-data
          (while (re-search-forward
                  (rx bol
                      "thor " (group (+ (not space)))
                      (* " " (or "default" (+ (any upper))))
                      (one-or-more " ") "#" (one-or-more any)
                      eol)
                  nil t)
            (let ((target (match-string 1)))
              (push (cons (concat "thor:" target)
                          (concat bundler "thor " (shell-quote-argument target)))
                    res))))
        (nreverse res)))))

(provide 'compile-multi-ruby-thor)
;;; compile-multi-ruby-thor.el ends here
