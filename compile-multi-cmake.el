;;; compile-multi-cmake.el --- `compile-multi' task generator for CMakeLists.txt projects -*- lexical-binding: t; -*-

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

(defgroup compile-multi-cmake nil
  "Helpers for `compile-multi' and CMakeLists projects."
  :group 'compile-multi)

(defcustom compile-multi-cmake-command "cmake --build build"
  "Command for running the cmake executable."
  :type 'boolean)

;;;###autoload
(defun compile-multi-cmake-targets ()
  "Retun a list of `compile-multi' targets for the current cmake project."
  (with-temp-buffer
    (insert
     (shell-command-to-string
      (concat compile-multi-cmake-command " --target help")))
    (goto-char (point-min))
    (let (res)
      (save-match-data
        (while (re-search-forward (rx
                                   bol
                                   (or
                                    (and
                                     (group-n 1 (minimal-match (one-or-more any)))
                                     ": " (one-or-more any))
                                    (and
                                     (one-or-more ".") " "
                                     (group-n 1 (minimal-match (one-or-more any)))
                                     (optional " (the default if no target is provided)")))
                                   eol)
                                  nil t)
          (let ((target (match-string 1)))
            (push (cons (concat "cmake:" target)
                        (concat compile-multi-cmake-command
                                " --target "
                                (shell-quote-argument target)))
                  res))))
      (nreverse res))))

(provide 'compile-multi-cmake)
;;; compile-multi-cmake.el ends here
