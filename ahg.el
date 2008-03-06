;;; ahg.el --- Alberto's Emacs interface for Mercurial (Hg)

;; Copyright (C) 2008 Alberto Griggio

;; Author: Alberto Griggio <agriggio@users.sourceforge.net>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; A simple Emacs interface for the Mercurial (Hg) Distributed SCM.
;;; Installation: put this file where Emacs can find it, and then add the line
;;; (require 'ahg)
;;; to your .emacs

(require 'diff-mode)
(require 'easymenu)
(require 'log-edit)
(require 'cl)

;;-----------------------------------------------------------------------------
;; the global aHg menu and keymap
;;-----------------------------------------------------------------------------

(easy-menu-add-item nil '("tools")
                    '("aHg"
                      ["Status" ahg-status t]
                      ["Log Summary" ahg-short-log t]
                      ["Detailed Log" ahg-log t]
                      ["Commit Current File" ahg-commit-cur-file t]
                      ["View Changes of Current File" ahg-diff-cur-file t]
                      ["Execute Hg Command" ahg-do-command t]
                      ["Help on Hg Command" ahg-command-help t])
                    "PCL-CVS")

(defvar ahg-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'ahg-status)
    (define-key map "l" 'ahg-short-log)
    (define-key map "L" 'ahg-log)
    (define-key map "!" 'ahg-do-command)
    (define-key map "h" 'ahg-command-help)
    (define-key map "c" 'ahg-commit-cur-file)
    (define-key map "=" 'ahg-diff-cur-file)
    map))

;;-----------------------------------------------------------------------------
;; Customization
;;-----------------------------------------------------------------------------

(defgroup ahg nil "aHg Mercurial Frontend" :group 'tools)

(defcustom ahg-global-key-prefix "hg"
  "Prefix of globally-available aHg commands."
  :group 'ahg :type 'string
  :set (function (lambda (symbol value)
                   (when (boundp symbol) (global-unset-key (eval symbol)))
                   (global-set-key (set symbol value) ahg-global-map))))

(defcustom ahg-do-command-insert-header t
  "If non-nil, `ahg-do-command' will insert a header line in the
command output." :group 'ahg :type 'boolean)

(defcustom ahg-restore-window-configuration-on-quit t
  "If non-nil, when `ahg-buffer-quit' will restore the window configuration."
  :group 'ahg :type 'boolean)

(defcustom ahg-diff-use-git-format t
  "If non-nil, aHg commands that output a diff will use the git format."
  :group 'ahg :type 'boolean)

(defface ahg-status-marked-face
  '((default (:inherit font-lock-preprocessor-face)))
  "Face for marked files in aHg status buffers." :group 'ahg)

(defface ahg-status-modified-face
  '((default (:inherit font-lock-function-name-face)))
  "Face for modified files in aHg status buffers." :group 'ahg)

(defface ahg-status-added-face
  '((default (:inherit font-lock-type-face)))
  "Face for added files in aHg status buffers." :group 'ahg)

(defface ahg-status-removed-face
  '((default (:inherit font-lock-constant-face)))
  "Face for removed files in aHg status buffers." :group 'ahg)

(defface ahg-status-clean-face
  '((default (:inherit default)))
  "Face for clean files in aHg status buffers." :group 'ahg)

(defface ahg-status-deleted-face
  '((default (:inherit font-lock-string-face)))
  "Face for deleted files in aHg status buffers." :group 'ahg)

(defface ahg-status-ignored-face
  '((default (:inherit font-lock-comment-face)))
  "Face for ignored files in aHg status buffers." :group 'ahg)

(defface ahg-status-unknown-face
  '((default (:inherit font-lock-variable-name-face)))
  "Face for unknown files in aHg status buffers." :group 'ahg)

(defface ahg-short-log-revision-face
  '((default (:inherit font-lock-function-name-face)))
  "Face for revision field in aHg short log buffers." :group 'ahg)

(defface ahg-short-log-date-face
  '((default (:inherit font-lock-string-face)))
  "Face for date field in aHg short log buffers." :group 'ahg)

(defface ahg-log-revision-face
  '((default (:inherit font-lock-variable-name-face)))
  "Face for revision field in aHg log buffers." :group 'ahg)

(defface ahg-log-field-face
  '((default (:inherit font-lock-function-name-face)))
  "Face for fields in aHg log buffers." :group 'ahg)

(defface ahg-log-branch-face
  '((default (:inherit font-lock-keyword-face)))
  "Face for tags and branches in aHg log buffers." :group 'ahg)

(defface ahg-short-log-user-face
  '((default (:inherit font-lock-type-face)))
  "Face for user field in aHg short log buffers." :group 'ahg)

(defface ahg-header-line-face
  '((default (:inherit font-lock-comment-face)))
  "Face for header lines in aHg buffers." :group 'ahg)

(defface ahg-header-line-root-face
  '((default (:inherit font-lock-constant-face)))
  "Face for repository path in header lines of aHg buffers." :group 'ahg)

;;-----------------------------------------------------------------------------
;; Variable definitions for faces
;;-----------------------------------------------------------------------------

(defvar ahg-status-marked-face 'ahg-status-marked-face)
(defvar ahg-status-modified-face 'ahg-status-modified-face)
(defvar ahg-status-added-face 'ahg-status-added-face)
(defvar ahg-status-removed-face 'ahg-status-removed-face)
(defvar ahg-status-clean-face 'ahg-status-clean-face)
(defvar ahg-status-deleted-face 'ahg-status-deleted-face)
(defvar ahg-status-ignored-face 'ahg-status-ignored-face)
(defvar ahg-status-unknown-face 'ahg-status-unknown-face)
(defvar ahg-short-log-revision-face 'ahg-short-log-revision-face)
(defvar ahg-short-log-date-face 'ahg-short-log-date-face)
(defvar ahg-log-field-face 'ahg-log-field-face)
(defvar ahg-log-branch-face 'ahg-log-branch-face)
(defvar ahg-log-revision-face 'ahg-log-revision-face)
(defvar ahg-short-log-user-face 'ahg-short-log-user-face)
(defvar ahg-header-line-face 'ahg-header-line-face)
(defvar ahg-header-line-root-face 'ahg-header-line-root-face)

;;-----------------------------------------------------------------------------
;; hg root
;;-----------------------------------------------------------------------------

(defun ahg-root ()
  "Returns the root of the tree handled by Mercurial, or nil if
the current dir is not under hg."
  (with-temp-buffer
    (when (= (call-process "hg" nil t nil "root") 0)
      (buffer-substring-no-properties (point-min) (1- (point-max))))))

;;-----------------------------------------------------------------------------
;; hg identify
;;-----------------------------------------------------------------------------

(defun ahg-identify (&optional root)
  (interactive)
  (with-temp-buffer
    (let ((status
           (if root
               (let ((default-directory (file-name-as-directory root)))
                 (call-process "hg" nil t nil "identify"))
             (call-process "hg" nil t nil "identify"))))
      (when (= status 0)
        (buffer-substring-no-properties (point-min) (1- (point-max)))))))

;;-----------------------------------------------------------------------------
;; hg status
;;-----------------------------------------------------------------------------

(defvar ahg-face-status-hash
  (let* ((test (define-hash-table-test 'ahg-str-hash 'string= 'sxhash))
         (h (make-hash-table :test 'ahg-str-hash)))
    (puthash "M" ahg-status-modified-face h)
    (puthash "A" ahg-status-added-face h)
    (puthash "R" ahg-status-removed-face h)
    (puthash "C" ahg-status-clean-face h)
    (puthash "!" ahg-status-deleted-face h)
    (puthash "I" ahg-status-ignored-face h)
    (puthash "?" ahg-status-unknown-face h)
    h))

(defvar ahg-status-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'ahg-status-visit-file-other-window)
    map))

(defun ahg-face-from-status (status-code)
  (gethash status-code ahg-face-status-hash 'default))


(define-derived-mode ahg-status-mode nil "aHg-status"
  "Major mode for *hg status* buffers.

Commands:
\\{ahg-status-mode-map}
"
  (toggle-read-only t)
  (font-lock-mode nil)
  (define-key ahg-status-mode-map (kbd "C-h") 'describe-mode)
  (define-key ahg-status-mode-map " " 'ahg-status-toggle-mark)
  (define-key ahg-status-mode-map "m" 'ahg-status-mark)
  (define-key ahg-status-mode-map "u" 'ahg-status-unmark)
  (define-key ahg-status-mode-map (kbd "M-DEL") 'ahg-status-unmark-all)
  (define-key ahg-status-mode-map "c" 'ahg-status-commit)
  (define-key ahg-status-mode-map "a" 'ahg-status-add)
  (define-key ahg-status-mode-map "=" 'ahg-status-diff)
  (define-key ahg-status-mode-map "D" 'ahg-status-diff-all)
  (define-key ahg-status-mode-map "r" 'ahg-status-remove)
  (define-key ahg-status-mode-map "g" 'ahg-status-refresh)
  (define-key ahg-status-mode-map "q" 'ahg-buffer-quit)
  (define-key ahg-status-mode-map "U" 'ahg-status-undo)
  (define-key ahg-status-mode-map "!" 'ahg-status-do-command)
  (define-key ahg-status-mode-map "l" 'ahg-status-short-log)
  (define-key ahg-status-mode-map "L" 'ahg-status-log)
  (define-key ahg-status-mode-map "f" 'ahg-status-visit-file)
  (define-key ahg-status-mode-map "\r" 'ahg-status-visit-file)
  (define-key ahg-status-mode-map "o" 'ahg-status-visit-file-other-window)
  (define-key ahg-status-mode-map "h" 'ahg-command-help)
  (let ((showmap (make-sparse-keymap)))
    (define-key showmap "A" 'ahg-status-show-all)
    (define-key showmap "m" 'ahg-status-show-modified)
    (define-key showmap "a" 'ahg-status-show-added)
    (define-key showmap "r" 'ahg-status-show-removed)
    (define-key showmap "d" 'ahg-status-show-deleted)
    (define-key showmap "c" 'ahg-status-show-clean)
    (define-key showmap "u" 'ahg-status-show-unknown)
    (define-key showmap "i" 'ahg-status-show-ignored)
    (define-key ahg-status-mode-map "s" showmap))
  (easy-menu-add ahg-status-mode-menu ahg-status-mode-map))

(easy-menu-define ahg-status-mode-menu ahg-status-mode-map "aHg Status"
  '("aHg Status"
    ["Visit File" ahg-status-visit-file [:keys "f" :active t]]
    ["Visit File (Other Window)" ahg-status-visit-file-other-window
     [:keys "o" :active t]]
    ["--" nil nil]
    ["Commit" ahg-status-commit [:keys "c" :active t]]
    ["Add" ahg-status-add [:keys "a" :active t]]
    ["Remove" ahg-status-remove [:keys "r" :active t]]
    ["Undo" ahg-status-undo [:keys "U" :active t]]
    ["Hg Command" ahg-status-do-command [:keys "!" :active t]]
    ["--" nil nil]
    ["Toggle Mark" ahg-status-toggle-mark [:keys " " :active t]]
    ["Mark" ahg-status-mark [:keys "m" :active t]]
    ["Unmark" ahg-status-unmark [:keys "u" :active t]]
    ["Unmark All" ahg-status-unmark-all [:keys (kbd "M-DEL") :active t]]
    ["--" nil nil]
    ["Short Log" ahg-short-log [:keys "l" :active t]]
    ["Detailed Log" ahg-log [:keys "L" :active t]]
    ["Diff" ahg-status-diff [:keys "=" :active t]]
    ["Diff All" ahg-status-diff-all [:keys "D" :active t]]
    ["--" nil nil]
    ("Show"
     ["All" ahg-status-show-all [:keys "sA" :active t]]
     ["Modified" ahg-status-show-modified [:keys "sm" :active t]]
     ["Added" ahg-status-show-added [:keys "sa" :active t]]
     ["Removed" ahg-status-show-removed [:keys "sr" :active t]]
     ["Deleted" ahg-status-show-deleted [:keys "sd" :active t]]
     ["Clean" ahg-status-show-clean [:keys "sc" :active t]]
     ["Unknown" ahg-status-show-unknown [:keys "su" :active t]]
     ["Ignored" ahg-status-show-ignored [:keys "si" :active t]]
     )
    ["--" nil nil]
    ["Help on Hg Command" ahg-command-help [:keys "h" :active t]]
    ["--" nil nil]
    ["Refresh" ahg-status-refresh [:keys "g" :active t]]
    ["Quit" ahg-buffer-quit [:keys "q" :active t]]
    ))


(defun ahg-status (&rest extra-switches)
  "Run hg status. When called non-interactively, it is possible
to pass extra switches to hg status."
  (interactive)
  (let* ((buf (get-buffer-create "*aHg-status*"))
         (process (apply 'start-process-shell-command
                         "aHg-status" buf "hg status" extra-switches)))
    (set-process-sentinel process 'ahg-status-sentinel)
    (with-current-buffer buf
      (set (make-local-variable 'ahg-root) (ahg-root))
      (ahg-push-window-configuration))))

(defun ahg-status-pp (data)
  "Pretty-printer for data elements in a *hg status* buffer."
  ;; data is a cons cell: (marked flag (hg status . filename))
  (let ((marked (car data))
        (status-code (cadr data))
        (filename (cddr data)))
    (if marked
        (progn
          (insert (propertize "*" 'face (cons 'foreground-color "#00CC00")))
          (insert (propertize (concat status-code " " filename)
                              'face ahg-status-marked-face)))
      (insert (propertize (concat " " status-code " " filename)
                          'face (ahg-face-from-status status-code)
                          'mouse-face 'highlight
                          'keymap ahg-status-line-map)))))


(defun ahg-status-toggle-mark ()
  (interactive)
  (let* ((node (ewoc-locate ewoc))
         (data (and node (ewoc-data node)))
         (inhibit-read-only t))
    (when data
      (if (car data) (ewoc-set-data node (cons nil (cdr data)))
        (ewoc-set-data node (cons t (cdr data))))
      (ewoc-invalidate ewoc node)
      (setq node (ewoc-next ewoc node))
      (when node (goto-char (ewoc-location node))))))

(defun ahg-status-do-mark (yes)
  (let* ((node (ewoc-locate ewoc))
         (data (and node (ewoc-data node)))
         (inhibit-read-only t))
    (when data
      (if yes (ewoc-set-data node (cons t (cdr data)))
        (ewoc-set-data node (cons nil (cdr data))))
      (ewoc-invalidate ewoc node)
      (setq node (ewoc-next ewoc node))
      (when node (goto-char (ewoc-location node))))))

(defun ahg-status-mark () (interactive) (ahg-status-do-mark t))
(defun ahg-status-unmark () (interactive) (ahg-status-do-mark nil))

(defun ahg-status-unmark-all ()
  (interactive)
  (ewoc-map (lambda (d) (when (car d) (setcar d nil) t)) ewoc))

(defun ahg-status-show-all () (interactive) (ahg-status "-A"))
(defun ahg-status-show-modified () (interactive) (ahg-status "-m"))
(defun ahg-status-show-added () (interactive) (ahg-status "-a"))
(defun ahg-status-show-removed () (interactive) (ahg-status "-r"))
(defun ahg-status-show-deleted () (interactive) (ahg-status "-d"))
(defun ahg-status-show-clean () (interactive) (ahg-status "-c"))
(defun ahg-status-show-unknown () (interactive) (ahg-status "-u"))
(defun ahg-status-show-ignored () (interactive) (ahg-status "-i"))

(defun ahg-status-get-marked (action-if-empty &optional filter)
  "Returns the list of marked nodes. If such list is empty, behave according to
ACTION-IF-EMPTY: if nil, do nothing. If 'all, return all nodes, if 'cur return
the singleton list with the node at point."
  (let ((marked (ewoc-collect ewoc
                              (if filter (lambda (d)
                                           (and (car d) (funcall filter d)))
                                'car))))
    (if (null marked)
      (cond ((eq action-if-empty nil) nil)
            ((eq action-if-empty 'all)
             (ewoc-collect ewoc (if filter filter 'identity)))
            ((eq action-if-empty 'cur)
             (let ((n (ewoc-locate ewoc)))
               (when (and n (or (null filter) (funcall filter (ewoc-data n))))
                 (list (ewoc-data n))))))
      marked)))

(defun ahg-status-commit ()
  (interactive)
  (let ((files (ahg-status-get-marked nil)))
    (ahg-commit (mapcar 'cddr files))))

(defun ahg-status-add ()
  (interactive)
  (let ((files (ahg-status-get-marked
                'all (lambda (data) (string= (cadr data) "?")))))
    (if (yes-or-no-p (format "Add %d files to hg? " (length files)))
        (ahg-generic-command
         "add" (mapcar 'cddr files)
         (lexical-let ((howmany (length files)))
           (lambda (process status)
             (if (string= status "finished\n")
                 (with-current-buffer
                     (process-buffer process)
                   (ahg-status-maybe-refresh)
                   (message "Added %d files" howmany))
               (ahg-show-error process)))))
      (message "hg add aborted"))))

(defun ahg-status-remove ()
  (interactive)
  (let ((files (ahg-status-get-marked
                 'all (lambda (data)
                        (let ((f (cadr data)))
                          (or (string= f "A") (string= f "C")))))))
    (if (yes-or-no-p (format "Remove %d files from hg? " (length files)))
        (ahg-generic-command
         "remove" (mapcar 'cddr files)
         (lexical-let ((howmany (length files)))
           (lambda (process status)
             (if (string= status "finished\n")
                 (with-current-buffer (process-buffer process)
                   (ahg-status-maybe-refresh)
                   (message "Removed %d files" howmany))
               (ahg-show-error process)))))
      (message "hg remove aborted"))))

(defun ahg-status-refresh ()
  (interactive)
  (ahg-status))


(defun ahg-status-maybe-refresh ()
  (let ((buf (ahg-get-status-buffer (ahg-root))))
    (when buf (ahg-status))))


(defun ahg-status-diff (&optional all)
  (interactive)
  (let ((files (ahg-status-get-marked (if all 'all 'cur)))
        (buf (get-buffer-create "*aHg diff*"))
        (inhibit-read-only t)
        (args (when ahg-diff-use-git-format '("--git"))))
    (with-current-buffer buf
      (erase-buffer)
      (ahg-push-window-configuration))
    (ahg-generic-command "diff" (append args (mapcar 'cddr files))
                         (lambda (process status)
                            (if (string= status "finished\n")
                                (progn
                                  (pop-to-buffer (process-buffer process))
                                  (ahg-diff-mode)
                                  (set-buffer-modified-p nil)
                                  (beginning-of-buffer))
                              (ahg-show-error process)))
                         buf)))

(defun ahg-status-diff-all ()
  (interactive)
  (ahg-status-diff t))

(defun ahg-status-undo ()
  (interactive)
  (let ((files (ahg-status-get-marked nil)))
    (if (yes-or-no-p
         (if files (format "Undo changes on %d files? " (length files))
           "Undo all changes? "))
        (ahg-generic-command "revert" (if files (mapcar 'cddr files) '("--all"))
                             (lambda (process status)
                                (if (string= status "finished\n")
                                    (with-current-buffer
                                        (process-buffer process)
                                      (ahg-status-maybe-refresh))
                                  (ahg-show-error process))))
      (message "hg revert aborted"))))                         


(defun ahg-get-status-ewoc (root)
  "Returns an *hg status* buffer for ROOT. The buffer's major mode is
ahg-status, and it has an ewoc associated with it."
  (let ((buf (get-buffer-create (concat "*hg status: " root "*")))
        (inhibit-read-only t)
        (header (concat
                 (propertize "hg status for " 'face ahg-header-line-face)
                 (propertize root 'face ahg-header-line-root-face) "\n"))
        (footer (concat "\n"
                        (make-string (1- (window-width (selected-window))) ?-)
                        "\nId: " (ahg-identify root))))
    (with-current-buffer buf
      (erase-buffer)
      (let ((ew (ewoc-create 'ahg-status-pp
                             header footer)))
        (ahg-status-mode)
        (set (make-local-variable 'ewoc) ew)
        (setq default-directory (file-name-as-directory root))
        ew))))

(defun ahg-get-status-buffer (&optional root)
  (unless root (setq root (ahg-root)))
  (get-buffer (concat "*hg status: " root "*")))

(defun ahg-status-sentinel (process status)
  (if (string= status "finished\n")
      ;; everything was ok, we can show the status buffer
      (let* ((buf (process-buffer process))
             (root (with-current-buffer buf ahg-root))
             (ew (ahg-get-status-ewoc root))
             (outbuf (ewoc-buffer ew))
             (cfg (with-current-buffer buf ahg-window-configuration)))
        (with-current-buffer buf
          (beginning-of-buffer)
          (while (not (eobp))
            (ewoc-enter-last
             ew
             (cons nil
                   (cons (buffer-substring (point) (1+ (point)))
                         (buffer-substring (+ (point) 2) (point-at-eol)))))
            (forward-line 1)))
        (kill-buffer buf)
        (pop-to-buffer outbuf)
        (set (make-local-variable 'ahg-window-configuration) cfg)
        (let ((inhibit-read-only t)
              (node (ewoc-nth ew 0)))
          (ewoc-refresh ew)
          (when node (goto-char (ewoc-location node)))))
    ;; error, we signal it and pop to the buffer
    (ahg-show-error process)))


(defun ahg-status-visit-file (&optional other-window)
  (interactive)
  (let* ((node (ewoc-locate ewoc))
         (data (and node (ewoc-data node))))
    (when data
      (if other-window
          (find-file-other-window (cddr data))
        (find-file (cddr data))))))

(defun ahg-status-visit-file-other-window ()
  (interactive)
  (ahg-status-visit-file t))


(defun ahg-status-do-command ()
  (interactive)
  (let ((files (ahg-status-get-marked nil)))
    (if files
        (let ((ahg-do-command-prompt "Hg command on selected files: ")
              (ahg-do-command-extra-args (mapcar 'cddr files)))
          (call-interactively 'ahg-do-command))
      (call-interactively 'ahg-do-command))))

(defun ahg-status-short-log ()
  (interactive)
  (let* ((files (ahg-status-get-marked nil))
         (ahg-file-list-for-log-command (if files (mapcar 'cddr files) nil)))
    (call-interactively 'ahg-short-log)))

(defun ahg-status-log ()
  (interactive)
  (let* ((files (ahg-status-get-marked nil))
         (ahg-file-list-for-log-command (if files (mapcar 'cddr files) nil)))
    (call-interactively 'ahg-log)))

;;-----------------------------------------------------------------------------
;; hg commit
;;-----------------------------------------------------------------------------

(defun ahg-commit-callback ()
  (interactive)
  (let ((msg (buffer-string)))
    (let ((args (append (list "-m" msg)
                        (log-edit-files))))
      (ahg-generic-command
       "commit" args
       (lexical-let ((aroot (ahg-root)))
         (lambda (process status)
           (if (string= status "finished\n")
               (let ((buf (ahg-get-status-buffer aroot)))
                 (when buf (ahg-status)))
             (ahg-show-error process)))))))
  (kill-buffer (current-buffer)))

(defun ahg-commit (files)
  "Run hg commit. Pops up a buffer for editing the log file."
  (let ((buf (generate-new-buffer "*aHg-log*")))
    (log-edit
     'ahg-commit-callback
     nil
     (list (cons 'log-edit-listfun
                 (lexical-let ((flist files)) (lambda () flist))))
     buf)))

(defun ahg-commit-cur-file ()
  "Run hg commit on the current file only."
  (interactive)
  (cond ((eq major-mode 'ahg-status-mode)
         (call-interactively 'ahg-status-commit))
        ((buffer-file-name) (ahg-commit (list (buffer-file-name))))
        (t (message "hg commit: no file found, aborting."))))

;;-----------------------------------------------------------------------------
;; hg log
;;-----------------------------------------------------------------------------

(defvar ahg-short-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h") 'describe-mode)
    (define-key map [?g] 'ahg-short-log)
    (define-key map [?s] 'ahg-status)
    (define-key map [?=] 'ahg-short-log-view-diff)
    (define-key map [? ] 'ahg-short-log-view-details)
    (define-key map [?\r] 'ahg-short-log-view-details)
    (define-key map [?r] 'ahg-short-log-goto-revision)
    (define-key map [?n] 'ahg-short-log-next)
    (define-key map [?p] 'ahg-short-log-previous)
    (define-key map [?q] 'ahg-buffer-quit)
    (define-key map [?!] 'ahg-do-command)
    (define-key map [?h] 'ahg-command-help)
    map)
  "Keymap used in `ahg-short-log-mode'.")

(defvar ahg-short-log-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ahg-short-log-view-details-mouse)
    (define-key map [mouse-2] 'ahg-short-log-view-diff-mouse)
    map))

;; (defvar ahg-short-log-revision-face font-lock-function-name-face)
;; (defvar ahg-short-log-date-face font-lock-string-face)
;; (defvar ahg-short-log-user-face font-lock-type-face)

(defconst ahg-short-log-start-regexp "^ +\\([0-9]+\\) |")

(define-derived-mode ahg-short-log-mode fundamental-mode "ahg-short-log"
  "Major mode to display hg shortlog output.

Commands:
\\{ahg-short-log-mode-map}
"
  (use-local-map ahg-short-log-mode-map)
  (font-lock-mode nil)
  (easy-menu-add ahg-short-log-mode-menu ahg-short-log-mode-map))

(easy-menu-define ahg-short-log-mode-menu ahg-short-log-mode-map "aHg Short Log"
  '("aHg Short Log"
    ["View Revision Diff" ahg-short-log-view-diff [:keys "=" :active t]]
    ["View Revision Details" ahg-short-log-view-details [:keys " " :active t]]
    ["--" nil nil]
    ["Status" ahg-status [:keys "s" :active t]]
    ["Hg Command" ahg-do-command [:keys "!" :active t]]
    ["--" nil nil]
    ["Next Revision" ahg-short-log-next [:keys "n" :active t]]
    ["Previous Revision" ahg-short-log-previous [:keys "p" :active t]]
    ["Go To Revision..." ahg-short-log-goto-revision [:keys "r" :active t]]
    ["--" nil nil]
    ["Help on Hg Command" ahg-command-help [:keys "h" :active t]]
    ["--" nil nil]
    ["Refresh" ahg-short-log [:keys "g" :active t]]
    ["Quit" ahg-buffer-quit [:keys "q" :active t]]
    ))


(defun ahg-short-log-pp (data)
  "Pretty-printer for short log revisions."
  ;; data is a 4-elements list
  (labels ((trim (n s) (if (> (length s) n) (substring s 0 n) s)))
    (let* ((width (window-width (selected-window)))
           (p1 (car data))
           (p2 (cadr data))
           (p3 (caddr data))
           (p4 (cadddr data))
           (s (format "%7s | %s | %8s | %s"
                      (propertize p1 'face ahg-short-log-revision-face)
                      (propertize p2 'face ahg-short-log-date-face)
                      (propertize (trim 8 p3) 'face ahg-short-log-user-face)
                      p4))
           (pad (if (< (length s) width)
                    (make-string (- width (length s)) ? )
                  "")))
      (insert (propertize (concat s pad) 'mouse-face 'highlight
                          'keymap ahg-short-log-line-map)))))
        
(defun ahg-short-log-insert-contents (ewoc contents)
  (let ((lines (split-string contents "\n")))
    (labels ((format-line (line)
               (if (and line (> (length line) 0))
                    (let* ((p1 (string-match " " line))
                           (p2 (string-match " " line (1+ p1)))
                           (p3 (string-match " " line (1+ p2)))
                           (data (list
                                  (substring line 0 p1)
                                  (substring line (1+ p1) p2)
                                  (substring line (1+ p2) p3)
                                  (substring line (1+ p3)))))
                      (ewoc-enter-last ewoc data)))))
      (mapcar 'format-line lines))))


(defun ahg-short-log-next (n)
  "Move to the next changeset line"
  (interactive "p")
  (ewoc-goto-next ewoc n))

(defun ahg-short-log-previous (n)
  "Move to the previous changeset line"
  (interactive "p")
  (ewoc-goto-prev ewoc n))

(defun ahg-short-log-revision-at-point ()
  (let ((node (ewoc-locate ewoc)))
    (and node (car (ewoc-data node)))))

(defun ahg-short-log-view-diff ()
  (interactive)
  (let ((r1 (ahg-short-log-revision-at-point))
        (r2 "tip"))
    (when (string-to-number r1)
      (setq r2 (number-to-string (1- (string-to-number r1)))))
    (ahg-diff r2 r1)))

(defun ahg-short-log-view-details ()
  "View details of the given revision."
  (interactive)
  (let ((rev (ahg-short-log-revision-at-point)))
    (ahg-log rev nil)))


(defun ahg-short-log-view-details-mouse (event)
  (interactive "e")
  (save-window-excursion
    (select-window (posn-window (event-end event)) t)
    (goto-char (posn-point (event-end event)))
    (ahg-short-log-view-details)))


(defun ahg-short-log-view-diff-mouse (event)
  (interactive "e")
  (save-window-excursion
    (select-window (posn-window (event-end event)) t)
    (goto-char (posn-point (event-end event)))
    (ahg-short-log-view-diff)))


(defun ahg-short-log-goto-revision (rev)
  "Move point to the revision REV. If REV is not found in the log buffer,
do nothing."
  (interactive "P")
  (when (interactive-p)
    (unless rev
      (setq rev (read-string "Goto revision: "))))
  (let ((n (ewoc-nth ewoc 0)))
    (while (and n (not (string= rev (car (ewoc-data n)))))
      (setq n (ewoc-next ewoc n)))
    (when n (ewoc-goto-node ewoc n))))


(defun ahg-args-add-revs (r1 r2 &optional disjoint)
  (let (command-list)
    (when r1
      (when (numberp r1)
        (setq r1 (number-to-string r1))))
    (when r2
      (when (numberp r2)
        (setq r2 (number-to-string r2))))
    (cond ((and r1 r2 (> (length r1) 0) (> (length r2) 0))
           (setq command-list
                 (if disjoint
                     (append command-list (list "-r" r1 "-r" r2))
                   (append command-list (list "-r" (format "%s:%s" r1 r2))))))
          ((and r1 (> (length r1) 0)) (setq command-list
                                 (append command-list (list "-r" r1))))
          ((and r2 (> (length r2) 0)) (setq command-list
                                            (append command-list
                                                    (list "-r" r2)))))
    command-list))


(defun ahg-short-log-create-ewoc ()
  (let* ((width (window-width (selected-window)))
         (header (concat
                  (propertize "hg log for " 'face ahg-header-line-face)
                  (propertize default-directory 'face ahg-header-line-root-face)
                  "\n\n" (propertize (make-string width ?-) 'face 'bold) "\n"
                  (propertize "    Rev |    Date    |  Author  | Summary\n"
                              'face 'bold)
                  (propertize (make-string width ?-) 'face 'bold)))
         (footer (propertize (make-string width ?-) 'face 'bold))
         (ew (ewoc-create 'ahg-short-log-pp header footer)))
    ew))

(defvar ahg-file-list-for-log-command nil)

(defun ahg-short-log (r1 r2)
  "Run hg log, in a compressed format.
This displays the log in a tabular view, one line per
changeset. The format of each line is: Revision | Date | User |
Summary.  When run interactively with a positve prefix argument,
don't ask for revisions."
  (interactive
   (list (read-string
          (concat "hg log"
                  (if ahg-file-list-for-log-command " (on selected files)" "")
                  ", R1: ") "tip")
         (read-string
          (concat "hg log"
                  (if ahg-file-list-for-log-command " (on selected files)" "")
                  ", R2: ") "0")))
  (let ((buffer (get-buffer-create
                 (concat "*hg log (summary): " (ahg-root) "*")))
        (command-list (ahg-args-add-revs r1 r2)))  
    (setq command-list
          (append command-list
                  (list "--template"
                        "{rev} {date|shortdate} {author|user} {desc|firstline}\\n")))
    (when ahg-file-list-for-log-command
      (setq command-list (append command-list ahg-file-list-for-log-command)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ahg-push-window-configuration)))
    (ahg-generic-command
     "log" command-list
     (lambda (process status)
       (if (string= status "finished\n")
           (with-current-buffer (process-buffer process)
             (pop-to-buffer (current-buffer))
             (ahg-short-log-mode)
             (let ((contents (buffer-substring-no-properties
                              (point-min) (point-max)))
                   (inhibit-read-only t))
               (erase-buffer)
               (setq truncate-lines t)
               (let ((ew (ahg-short-log-create-ewoc)))
                 (ahg-short-log-insert-contents ew contents)
                 (goto-line 6)
                 (toggle-read-only 1)
                 (set (make-local-variable 'ewoc) ew))))
         (ahg-show-error process)))
     buffer)))


(defvar ahg-log-font-lock-keywords
  '(("^hg \\<[a-z]+\\> for" . ahg-header-line-face)
    ("^hg \\<[a-z]+\\> for \\(.*\\)" 1 ahg-header-line-root-face)
    ("^changeset:" . ahg-log-field-face)
    ("^tag:" . ahg-log-field-face)
    ("^user:" . ahg-log-field-face)
    ("^date:" . ahg-log-field-face)
    ("^summary:" . ahg-log-field-face)
    ("^files:" . ahg-log-field-face)
    ("^branch:" . ahg-log-field-face)
    ("^parent:" . ahg-log-field-face)
    ("^description:" . ahg-log-field-face)
    ("^\\(changeset\\|parent\\): +\\(.+\\)$" 2 ahg-log-revision-face)
    ("^\\(tag\\|branch\\): +\\(.+\\)$" 2 ahg-log-branch-face)
    ("^user: +\\(.+\\)$" 1 ahg-short-log-user-face)
    ("^date: +\\(.+\\)$" 1 ahg-short-log-date-face)
    )
  "Keywords in `ahg-log-mode' mode.")

(define-derived-mode ahg-log-mode nil "ahg-log"
  "Major mode to display hg log output.

Commands:
\\{ahg-log-mode-map}
"
  (toggle-read-only t)
  (define-key ahg-log-mode-map (kbd "C-h") 'describe-mode)
  (define-key ahg-log-mode-map [?g] 'ahg-log)
  (define-key ahg-log-mode-map [?s] 'ahg-status)
  (define-key ahg-log-mode-map [?=] 'ahg-log-view-diff)
  (define-key ahg-log-mode-map [?n] 'ahg-log-next)
  (define-key ahg-log-mode-map "\t" 'ahg-log-next)
  (define-key ahg-log-mode-map [?p] 'ahg-log-previous)
  (define-key ahg-log-mode-map [?q] 'ahg-buffer-quit)
  (define-key ahg-log-mode-map [?!] 'ahg-do-command)
  (define-key ahg-log-mode-map [?h] 'ahg-command-help)
  (set (make-local-variable 'font-lock-defaults)
       (list 'ahg-log-font-lock-keywords t nil nil))
  (easy-menu-add ahg-log-mode-menu ahg-log-mode-map))

(easy-menu-define ahg-log-mode-menu ahg-log-mode-map "aHg Log"
  '("aHg Log"
    ["View Revision Diff" ahg-log-view-diff [:keys "=" :active t]]
    ["--" nil nil]
    ["Status" ahg-status [:keys "s" :active t]]
    ["Hg Command" ahg-do-command [:keys "!" :active t]]
    ["--" nil nil]
    ["Next Revision" ahg-log-next [:keys "\t" :active t]]
    ["Previous Revision" ahg-log-previous [:keys "p" :active t]]
    ["--" nil nil]
    ["Help on Hg Command" ahg-command-help [:keys "h" :active t]]
    ["--" nil nil]
    ["Refresh" ahg-log [:keys "g" :active t]]
    ["Quit" ahg-buffer-quit [:keys "q" :active t]]
    ))


(defconst ahg-log-start-regexp "^changeset: +\\([0-9]+:[0-9a-f]+\\)")
(defun ahg-log-next (n)
  "Move to the next changeset header of the next diff hunk"
  (interactive "p")
  (end-of-line)
  (re-search-forward ahg-log-start-regexp nil t n)
  (beginning-of-line))

(defun ahg-log-previous (n)
  "Move to the previous changeset header of the previous diff hunk"
  (interactive "p")
  (end-of-line)
  (re-search-backward ahg-log-start-regexp)
  (re-search-backward ahg-log-start-regexp nil t n))

(defun ahg-log-view-diff ()
  (interactive)
  (let ((r1 (ahg-log-revision-at-point t))
        (r2 "tip"))
    (when (string-to-number r1)
      (setq r2 (number-to-string (1- (string-to-number r1)))))
    (ahg-diff r2 r1)))

(defun ahg-log-revision-at-point (&optional short-id)
  (save-excursion
    (end-of-line)
    (re-search-backward ahg-log-start-regexp)
    (let ((rev (match-string-no-properties 1)))
      (when rev
        (if short-id (car (split-string rev ":"))
          (cadr (split-string rev ":")))))))


(defun ahg-log-goto-revision (rev)
  "Move point to the revision REV. If REV is not found in the log
buffer, do nothing."
  (let ((rev-pos))
    (save-excursion
      (when
          (re-search-forward (concat "^changeset: +" rev) nil t)
        (setq rev-pos (point))))
    (when rev-pos
      (goto-char rev-pos))))

(defun ahg-log (r1 r2)
  "Run hg log. When run interactively with a positve prefix
argument, don't ask for revisions."
  (interactive
   (list (read-string
          (concat "hg log"
                  (if ahg-file-list-for-log-command " (on selected files)" "")
                  ", R1: ") "tip")
         (read-string
          (concat "hg log"
                  (if ahg-file-list-for-log-command " (on selected files)" "")
                  ", R2: ") "0")))
  (let ((buffer (get-buffer-create
                 (concat "*hg log (details): " (ahg-root) "*")))
        (command-list (ahg-args-add-revs r1 r2)))  
    (setq command-list (append command-list (list "-v")))
    (when ahg-file-list-for-log-command
      (setq command-list (append command-list ahg-file-list-for-log-command)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ahg-push-window-configuration)))
    (ahg-generic-command
     "log" command-list
     (lambda (process status)
       (if (string= status "finished\n")
           (progn
             (pop-to-buffer (process-buffer process))
             (ahg-log-mode)
             (beginning-of-buffer)
             (let ((inhibit-read-only t))
               (insert
                (propertize "hg log for " 'face ahg-header-line-face)
                (propertize default-directory 'face ahg-header-line-root-face)
                "\n\n")))
         (ahg-show-error process)))
     buffer)))

;;-----------------------------------------------------------------------------
;; hg diff
;;-----------------------------------------------------------------------------

(define-derived-mode ahg-diff-mode diff-mode "aHg Diff"
  "Special Diff mode for aHg buffers.

Commands:
\\{ahg-diff-mode-map}
"
  (toggle-read-only t)
  (define-key ahg-diff-mode-map "q" 'ahg-buffer-quit)
  (easy-menu-add-item nil '("Diff") '["--" nil nil])
  (easy-menu-add-item nil '("Diff") '["Quit" ahg-buffer-quit
                                      [:keys "q" :active t]]))


(defun ahg-diff (&optional r1 r2 files)
  (interactive "P")
  (when (interactive-p)
    (unless r1
      (setq r1 (read-string "hg diff, R1: " "tip"))
      (setq r2 (read-string "hg diff, R2: " ""))))
  (let ((buffer (get-buffer-create "*hg diff*"))
        (command-list (ahg-args-add-revs r1 r2 t)))
    (when ahg-diff-use-git-format
      (setq command-list (cons "--git" command-list)))
    (when files
      (setq command-list (append command-list files)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ahg-push-window-configuration)))
    (ahg-generic-command "diff" command-list
                         (lambda (process status)
                           (if (string= status "finished\n")
                               (progn
                                 (pop-to-buffer (process-buffer process))
                                 (ahg-diff-mode)
                                 (beginning-of-buffer))
                             (ahg-show-error process)))
                         buffer)))

(defun ahg-diff-cur-file ()
  (interactive)
  (cond ((eq major-mode 'ahg-status-mode) (call-interactively 'ahg-status-diff))
        ((eq major-mode 'ahg-short-log-mode)
         (call-interactively 'ahg-short-log-view-diff))
        ((eq major-mode 'ahg-log-mode) (call-interactively 'ahg-log-view-diff))
        ((buffer-file-name) (ahg-diff nil nil (list (buffer-file-name))))
        (t (message "hg diff: no file found, aborting."))))

;;-----------------------------------------------------------------------------
;; hg command
;;-----------------------------------------------------------------------------

(defun ahg-complete-command-name (command)
  (with-temp-buffer
    (let ((process-environment (cons "LANG=" process-environment))) 
      (if (= (call-process "hg" nil t nil "help") 0)
          (let (out)
            (beginning-of-buffer)
            (search-forward "list of commands:")
            (beginning-of-line)
            (forward-line 2)
            (while (not (or (looking-at "^$") (eobp)))
              (forward-char 1)
              (let* ((pt (point))
                     (cmd (buffer-substring-no-properties
                           pt (progn (forward-word) (point))))
                     (ok (compare-strings command 0 nil cmd 0 nil)))
                (when (or (eq ok t) (= (- ok) (1+ (length command))))
                  (setq out (cons cmd out)))
                (beginning-of-line)
                (forward-line 1)))
            (if out (nreverse out) (list command)))
        ;; here, we silently ignore errors, and return the command itself as a
        ;; match
        (list command)))))

(defun ahg-complete-command (command)
  ;; we split the string, and treat the last word as a filename
  (let* ((idx (string-match "\\([^ ]+\\)$" command))
         (matches
          (cond ((= idx 0) (ahg-complete-command-name command))
                ((and (= idx 5) (string= (substring command 0 idx) "help "))
                 (ahg-complete-command-name (substring command idx)))
                (t (file-expand-wildcards
                    (concat (substring command idx) "*")))))
         (prev (substring command 0 idx)))
    (mapcar (function (lambda (a) (concat prev a))) matches)))


(defvar ahg-do-command-prompt "Hg command: ")
(defvar ahg-do-command-extra-args nil)

(defun ahg-do-command (cmdstring)
  (interactive (list
                (let ((minibuffer-local-completion-map
                       (copy-keymap minibuffer-local-map)))
                  (define-key minibuffer-local-completion-map "\t"
                    'minibuffer-complete)
                  (completing-read ahg-do-command-prompt
                                   (dynamic-completion-table
                                    ahg-complete-command)))))
  (let* ((args (split-string cmdstring))
         (cmdname (car args))
         (buffer (get-buffer-create (concat "*hg command: "
                                            (ahg-root) "*"))))
    (when ahg-do-command-extra-args
      (setq args (append args ahg-do-command-extra-args)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ahg-push-window-configuration)))
    (ahg-generic-command
     cmdname (cdr args)
     (lambda (process status)
       (if (string= status "finished\n")
           (with-current-buffer (process-buffer process)
             (ahg-command-mode)
             (pop-to-buffer (current-buffer))
             (when ahg-do-command-insert-header
               (let ((inhibit-read-only t))
                 (beginning-of-buffer)
                 (insert
                  (propertize
                   (concat "output of '"
                           (mapconcat 'identity (process-command process) " ")
                           "' on ")
                   'face ahg-header-line-face)
                  (propertize default-directory 'face ahg-header-line-root-face)
                  "\n" (make-string (1- (window-width (selected-window))) ?-)
                  "\n\n"))))
         (ahg-show-error process)))
     buffer)))

;;-----------------------------------------------------------------------------
;; hg help
;;-----------------------------------------------------------------------------

(defun ahg-command-help (command)
  (interactive
   (list (completing-read "Help on hg command: "
                          (dynamic-completion-table
                           ahg-complete-command-name))))
  (let ((buffer (get-buffer-create "*hg help*")))
    (with-current-buffer buffer (let ((inhibit-read-only t)) (erase-buffer)))
    (ahg-generic-command
     "help" (list command)
     (lambda (process status)
       (if (string= status "finished\n")
           (progn
             (pop-to-buffer (process-buffer process))
             (help-mode)
             (beginning-of-buffer))
         (ahg-show-error process)))
     buffer)))

;;-----------------------------------------------------------------------------
;; Various helper functions
;;-----------------------------------------------------------------------------

(defun ahg-buffer-quit ()
  (interactive)
  (let ((buf (current-buffer)))
    (ahg-pop-window-configuration)
    (kill-buffer buf)))

(defun ahg-generic-command (command args sentinel &optional buffer)
  "Executes then given hg command, with the given
arguments. SENTINEL is a sentinel function. BUFFER is the
destination buffer. If nil, a new buffer will be used."
  (unless buffer (setq buffer (generate-new-buffer "*ahg-command*")))
  (let ((process
         (apply 'start-process
                (concat "*ahg-command-" command "*") buffer
                "hg" command args)))
    (set-process-sentinel process (indirect-function sentinel))))

(defun ahg-show-error (process)
  "Displays an error message for the given process."
  (let ((buf (process-buffer process)))
    (pop-to-buffer buf)
    (beginning-of-buffer)
    (ahg-command-mode)
    (message "aHg error executing: %s"
             (mapconcat 'identity (process-command process) " "))))

(define-derived-mode ahg-command-mode nil "aHg command"
  "Major mode for aHg commands.

Commands:
\\{ahg-command-mode-map}
"
  (toggle-read-only t)
  (font-lock-mode nil)
  (define-key ahg-command-mode-map "h" 'ahg-command-help)
  (define-key ahg-command-mode-map "q" 'ahg-buffer-quit)
  (define-key ahg-command-mode-map "!" 'ahg-do-command)
  (easy-menu-add ahg-command-mode-menu ahg-command-mode-map))

(easy-menu-define ahg-command-mode-menu ahg-command-mode-map "aHg Command"
  '("aHg Command"
    ["Execute Hg Command" ahg-do-command [:keys "!" :active t]]
    ["Help on Hg Command" ahg-command-help [:keys "h" :active t]]
    ["Quit" ahg-buffer-quit [:keys "q" :active t]]))


(defun ahg-push-window-configuration ()
  (set (make-local-variable 'ahg-window-configuration)
       (current-window-configuration))
  (put 'ahg-window-configuration 'permanent-local t))

(defun ahg-pop-window-configuration ()
  (when (and ahg-restore-window-configuration-on-quit
             (boundp 'ahg-window-configuration))
    (set-window-configuration ahg-window-configuration)))


(provide 'ahg)
