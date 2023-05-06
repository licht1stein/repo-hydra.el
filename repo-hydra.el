;;; repo-hydra.el --- git repo specific hydras -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023, Mykhaylo Bilyanskyy <mb@m1k.pw>
;;
;; Author: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Maintainer: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Version: 1.0.0
;; Package-Requires: ((hydra "0.15.0"))
;;
;; Created: 06 May 2023
;;
;; URL: https://github.com/licht1stein/repo-hydra.el
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Easily define and call repo-specific hydra menus.
;;
;; Uses break versioning scheme: https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md
;;
;;; Code:
(require 'hydra)

(defvar repo-hydra--hydras-map (make-hash-table :test #'equal) "Hash-map with all repo-hydras.")

;;;###autoload
(defmacro repo-hydra (repo-name &rest menu-entries)
  "Create a repository specific hydra menu.

REPO-NAME - must be the same as repository root directory name
MENU-ENTRIES - hydra menu entries"
  (let* ((hy-cmd-name (read (concat (downcase repo-name) "-repo-hydra")))
         (commands-defs (mapcar (lambda (entry)
								                  (cl-destructuring-bind (k ecmd des ccmd) entry
									                  `(defun ,ecmd () (interactive) (cider-interactive-eval ,ccmd))))
								                menu-entries))
		     (hy-triplets (mapcar (lambda (entry)
								                (cl-destructuring-bind (k ecmd des ccmd) entry
								                  `(,k ,ecmd ,des)))
							                menu-entries)))
    (puthash repo-name (format "%s/body" hy-cmd-name) repo-hydra--hydras-map)
	  `(progn
	     ,@commands-defs
	     (defhydra ,hy-cmd-name ()
		     ,repo-name
		     ,@hy-triplets))))

(defun repo-hydra--current-git-repo ()
  "Return current repository root folder name as a string."
  (file-name-nondirectory
   (substring
    (shell-command-to-string "git rev-parse --show-toplevel")
    0 -1)))

;;;###autoload
(defun repo-hydra-show ()
  "Show interactive Clojure dev menu."
  (interactive)
  (let ((menu-name (gethash (repo-hydra--current-git-repo) repo-hydra--hydras-map)))
	  (if menu-name
        (funcall (read menu-name))
      (message "No dev menu defined for this git repo."))))

(provide 'repo-hydra)
;;; repo-hydra.el ends here
