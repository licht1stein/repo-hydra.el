;;; example.el --- repo-hydra example -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023, Mykhaylo Bilyanskyy <mb@m1k.pw>
;;
;; Author: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Maintainer: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; Created: 06 May 2023
;;
;; URL: https://github.com/licht1stein
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
;; This is an example file for repo-hydra
;;
;;; Code:
(require 'repo-hydra)

;; This is a standard hydra, can be used with any repo
(repo-hydra-define
 "repo-hydra.el"
 ("i" text-scale-increase "In")
 ("o" text-scale-decrease "Out"))


;; This is original @jpmonettas hydra for clojure
(repo-hydra-define-clj
 "project1"
 ("p" project1-portal "Portal" "(user/portal)")
 ("P" project1-clear-portal "Clear Portal" "(user/clear-portal)"))

(provide 'example)
;;; example.el ends here
