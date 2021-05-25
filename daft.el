;;; daft.el --- "Minimal" example of difference in behavior between widgets and buttons 

;; Copyright (C) 2021 Shankar Rao

;; Author: Shankar Rao <shankar.rao@gmail.com>
;; URL: https://github.com/~shankar2k/daft
;; Version: 0.2
;; Keywords: widget, button, deft

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is a "minimal" test case that, to some extent, mimics the behavior of
;; Deft to highlight the difference in mouse click behavior between widgets
;; and buttons.
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

;; Version 0.2 (2021-05-25):

;; - Added test functions for comparing behavior of widgets and buttons 

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
;; repository directory as widgets/buttons. One can open any of the files by
;; either hitting [RET] when the point is on the file's widget/button, or by
;; clicking on the widget/button with the mouse. If ``daft-use-buttons?'' is
;; set to ``t'', then either method for opening the file will operate the same
;; way. However if ``daft-use-buttons'' is ``nil'', then the behaviors for
;; these two options are different:

;;   - If the file is opened by hitting [RET] (invoking
;;     `widget-button-press'), the file is opened in the side window below and
;;     the file's window now has focus. This is the desired behavior.
;;
;;   - If the file is opened by clicking with the mouse (invoking
;;     `widget-button-click'), the file is opened in the side window below,
;;     but the focus remains with the Daft buffer. This is not the desired
;;     behavior.

;; One can use the test functions ``test-daft-button'' and
;; ``test-daft-widget'' to test and compare the functionality of Daft using
;; widgets and buttons, respectively. These functions set ``daft-use-button?''
;; to either t or nil, kill the daft-buffer if it exists, and then run
;; ``daft''.

;;; Code:

;;;; Requirements:

(require 'button)

;;;; Variables

(defconst daft-buffer "*Daft*")

(defvar daft-all-files nil)

(defvar daft-open-file-hook nil)

(defvar daft-use-button? t)

(defvar daft-buffer-alist '((display-buffer-reuse-window
                             display-buffer-in-side-window)                            
                            (side            . bottom)
                            (window-height . 0.5)))

(defvar daft-open-in-side-window? t)

;;;; Functions

(defun daft-buffer-setup ()
  (let ((inhibit-read-only t)
        (daft-file (if daft-use-button?
                       'daft-file-button
                     'daft-file-widget)))
    (erase-buffer)
    (insert "Daft\n\n")
    (mapc daft-file daft-all-files))
  (unless daft-use-button?
    (widget-setup))
  (goto-char (point-min))
  (forward-line 2))

(defun daft-button-open (button)
  (daft-open-file (button-get button 'tag)))

(define-button-type 'daft-button
  'action 'daft-button-open
  'follow-link t
  'help-echo "Edit this file")

(defun daft-file-button (file)
  (when file
    (let ((title (file-name-sans-extension (file-name-nondirectory file))))
      (insert-text-button title 'type 'daft-button 'tag file)
      (insert "\n"))))

(defun daft-file-widget (file)
  (when file
    (let ((title (file-name-sans-extension (file-name-nondirectory file))))
      (widget-create 'link
                     :format "%[%v%]"
                     :tag file
                     :help-echo "Edit this file"
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
(defun daft-open-file-side ()
  (when daft-open-in-side-window?
    (pop-to-buffer (get-file-buffer file) daft-buffer-alist)))


(defun test-daft-widget ()
  (interactive)
  (setq daft-use-button? nil)
  (let ((buffer (get-buffer daft-buffer)))
    (when buffer
      (kill-buffer buffer)))
  (daft))

(defun test-daft-button ()
  (interactive)
  (setq daft-use-button? t)
  (let ((buffer (get-buffer daft-buffer)))
    (when buffer
      (kill-buffer buffer)))
  (daft))        


(add-hook 'daft-open-file-hook 'daft-open-file-side)

;;;; Footer

(provide 'daft)

;;; daft.el ends here
