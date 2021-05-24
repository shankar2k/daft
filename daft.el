;;; daft.el --- "Minimal" example of difference in behavior between widget-button-click and widget-button-press 

;; Copyright (C) 2021 Shankar Rao

;; Author: Shankar Rao <shankar.rao@gmail.com>
;; URL: https://github.com/~shankar2k/daft
;; Version: 0.1
;; Keywords: widget, deft

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is a "minimal" test case that, to some extent, mimics the behavior of
;; Deft to show the difference in behavior between the functions
;; `widget-button-click' and `widget-button-press'.
;;
;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; History:

;; Version 0.1 (2021-05-23):

;; - Initial version

;;; Installation:

;; To install, first clone the Git repository
;; [on GitHub] (https://github.com/shankar2k/daft):

;;     git clone https://github.com/shankar2k/daft.git

;; Then make sure that Emacs can find it by adding the following line to your
;; startup file:

;;     (add-to-list 'load-path "/path/to/daft/repository")

;;; Usage:

;; After installing Daft, you can simply run `M-x daft` to start Daft. It will
;; list all of the text files (i.e., files with extension `.txt`) in the daft
;; repository directory as widgets. One can open any of the files by either
;; hitting [RET] when the point is on the file's widget, or by clicking on the
;; widget with the mouse. However the behaviors for these two options are
;; different:

;;   - If the file is opened by hitting [RET] (invoking
;;     `widget-button-press'), the file is opened in the side window below and
;;     the file's window now has focus. This is the desired behavior.
;;
;;   - If the file is opened by clicking with the mouse (invoking
;;     `widget-button-click'), the file is opened in the side window below,
;;     but the focus remains with the Daft buffer. This is not the desired
;;     behavior.

;;; Code:


(defconst daft-buffer "*Daft*")

(defvar daft-all-files nil)

(defvar daft-open-file-hook nil)

(defun daft-buffer-setup ()
  (let ((inhibit-read-only t))
    (erase-buffer))
  (widget-insert "Daft\n\n")
  (mapc 'daft-file-widget daft-all-files)
  (widget-setup)
  (goto-char (point-min))
  (forward-line 2))

(defun daft-file-widget (file)
  (when file
    (let ((title (file-name-sans-extension (file-name-nondirectory file))))
      (widget-create 'link
                     :format "%[%v%]"
                     :tag file
                     :notify (lambda (widget &rest ignore)
                               (daft-open-file (widget-get widget :tag)))
                     title)
      (widget-insert "\n"))))

(defun daft-open-file (file)
  (let ((buffer (find-file-noselect (file-truename file))))
    (with-current-buffer buffer
      (run-hooks 'daft-open-file-hook))
    (switch-to-buffer buffer)))
 
(put 'daft-mode 'mode-class 'special)

(defun daft-mode ()
  (setq buffer-read-only t)
  (setq major-mode 'daft-mode)
  (setq daft-all-files
        (directory-files (file-name-directory (locate-library "daft"))
                         t "\\.txt"))
  (daft-buffer-setup))

(defun daft ()
  (interactive)
  (switch-to-buffer daft-buffer)
  (if (not (eq major-mode 'daft-mode))
      (daft-mode)))


;; "extension" for opening files in a side window

(defvar daft-buffer-alist '((display-buffer-reuse-window
                             display-buffer-in-side-window)                            
                            (side            . bottom)
                            (window-height . 0.5)))

(defvar daft-open-in-side-window? t)

(defun daft-open-file-side ()
  (when daft-open-in-side-window?
    (pop-to-buffer (get-file-buffer file) daft-buffer-alist)))

(add-hook 'daft-open-file-hook 'daft-open-file-side)

(provide 'daft)
