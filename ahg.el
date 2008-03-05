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


;;-----------------------------------------------------------------------------
;; hg root
;;-----------------------------------------------------------------------------

(defun ahg-root ()
  "Returns the root of the tree handled by Mercurial, or nil if
the current dir is not under hg."
  (let ((r (shell-command-to-string "hg root")))
    (if (string= (substring r 0 7) "abort: ") nil (substring r 0 -1))))

;;-----------------------------------------------------------------------------
;; hg identify
;;-----------------------------------------------------------------------------

(defun ahg-identify (&optional root)
  (interactive)
  (unless root (setq root (ahg-root)))
  (let* ((default-directory (file-name-as-directory root))
         (r (shell-command-to-string (concat "hg identify"))))
    (if (string= (substring r 0 7) "abort: ") nil (substring r 0 -1))))

;;-----------------------------------------------------------------------------
;; hg status
;;-----------------------------------------------------------------------------

(defvar ahg-status-marked-face font-lock-preprocessor-face)
(defvar ahg-status-modified-face font-lock-function-name-face)
(defvar ahg-status-added-face font-lock-type-face)
(defvar ahg-status-removed-face font-lock-constant-face)
(defvar ahg-status-clean-face 'default)
(defvar ahg-status-deleted-face font-lock-string-face)
(defvar ahg-status-ignored-face font-lock-comment-face)
(defvar ahg-status-unknown-face font-lock-variable-name-face)

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

(defun ahg-face-from-status (status-code)
  (gethash status-code ahg-face-status-hash 'default))

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
                          'face (ahg-face-from-status status-code))))))


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

(defun ahg-status-mark (yes)
  (let* ((node (ewoc-locate ewoc))
         (data (and node (ewoc-data node)))
         (inhibit-read-only t))
    (when data
      (if yes (ewoc-set-data node (cons t (cdr data)))
        (ewoc-set-data node (cons nil (cdr data))))
      (ewoc-invalidate ewoc node)
      (setq node (ewoc-next ewoc node))
      (when node (goto-char (ewoc-location node))))))


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
  (let* ((files (ahg-status-get-marked
                 'all (lambda (data) (string= (cadr data) "?"))))
         (howmany (length files)))
    (if (yes-or-no-p (format "Add %d files to hg? " (length files)))
        (ahg-generic-command "add" (mapcar 'cddr files)
                             (ahg-capturing-lambda (process status)
                                (if (string= status "finished\n")
                                    (with-current-buffer
                                        (process-buffer process)
                                      (ahg-status-maybe-refresh)
                                      (message "Added %d files"
                                               (capture howmany)))
                                  (ahg-show-error process))))
      (message "hg add aborted"))))

(defun ahg-status-remove ()
  (interactive)
  (let* ((files (ahg-status-get-marked
                 'all (lambda (data)
                        (let ((f (cadr data)))
                          (or (string= f "A") (string= f "C"))))))
         (howmany (length files)))
    (if (yes-or-no-p (format "Remove %d files from hg? " (length files)))
        (ahg-generic-command "remove" (mapcar 'cddr files)
                             (ahg-capturing-lambda (process status)
                                (if (string= status "finished\n")
                                    (with-current-buffer
                                        (process-buffer process)
                                      (ahg-status-maybe-refresh)
                                      (message "Removed %d files"
                                               (capture howmany)))
                                  (ahg-show-error process))))
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
        (inhibit-read-only t))
    (with-current-buffer buf (erase-buffer))
    (ahg-generic-command "diff" (mapcar 'cddr files)
                         (lambda (process status)
                            (if (string= status "finished\n")
                                (progn
                                  (pop-to-buffer (process-buffer process))
                                  (diff-mode)
                                  (set-buffer-modified-p nil)
                                  (view-mode)
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
                         

(define-derived-mode ahg-status-mode nil "aHg-status"
  "Major mode for *hg status* buffers."
  (toggle-read-only t)
  (font-lock-mode nil)
  (define-key ahg-status-mode-map "C-h" 'describe-mode)
  (define-key ahg-status-mode-map " " 'ahg-status-toggle-mark)
  (define-key ahg-status-mode-map "m" (lambda () (interactive)
                                        (ahg-status-mark t)))
  (define-key ahg-status-mode-map "u" (lambda () (interactive)
                                        (ahg-status-mark nil)))
  (define-key ahg-status-mode-map "c" 'ahg-status-commit)
  (define-key ahg-status-mode-map "a" 'ahg-status-add)
  (define-key ahg-status-mode-map "=" 'ahg-status-diff)
  (define-key ahg-status-mode-map "D" 'ahg-status-diff-all)
  (define-key ahg-status-mode-map "r" 'ahg-status-remove)
  (define-key ahg-status-mode-map "g" 'ahg-status-refresh)
  (define-key ahg-status-mode-map "q" 'ahg-buffer-quit)
  (define-key ahg-status-mode-map "U" 'ahg-status-undo)
  (define-key ahg-status-mode-map "!" 'ahg-command)
  (define-key ahg-status-mode-map "l" 'ahg-short-log)
  (let ((showmap (make-sparse-keymap)))
    (define-key showmap "A" (lambda () (interactive) (ahg-status "-A")))
    (define-key showmap "m" (lambda () (interactive) (ahg-status "-m")))
    (define-key showmap "a" (lambda () (interactive) (ahg-status "-a")))
    (define-key showmap "r" (lambda () (interactive) (ahg-status "-r")))
    (define-key showmap "d" (lambda () (interactive) (ahg-status "-d")))
    (define-key showmap "c" (lambda () (interactive) (ahg-status "-c")))
    (define-key showmap "u" (lambda () (interactive) (ahg-status "-u")))
    (define-key showmap "i" (lambda () (interactive) (ahg-status "-I")))
    (define-key ahg-status-mode-map "s" showmap)))


(defun ahg-get-status-ewoc (root)
  "Returns an *hg status* buffer for ROOT. The buffer's major mode is
ahg-status, and it has an ewoc associated with it."
  (let ((buf (get-buffer-create (concat "*hg status: " root "*")))
        (inhibit-read-only t)
        (header (concat
                 (propertize "hg status for " 'face font-lock-comment-face)
                 (propertize root 'face font-lock-constant-face) "\n"))
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
             (outbuf (ewoc-buffer ew)))
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
        (let ((inhibit-read-only t)
              (node (ewoc-nth ew 0)))
          (ewoc-refresh ew)
          (when node (goto-char (ewoc-location node)))))
    ;; error, we signal it and pop to the buffer
    (let ((buf (process-buffer process)))
      (with-current-buffer buf
        (goto-char (point-min))
        (view-mode))
      (pop-to-buffer buf)
      (message "Error in hg status: %s" (substring status 0 -1)))))

(defun ahg-status (&rest extra-switches)
  "Run hg status. When called non-interactively, it is possible
to pass extra switches to hg status."
  (interactive)
  (let* ((buf (get-buffer-create "*aHg-status*"))
         (process (apply 'start-process-shell-command
                         "aHg-status" buf "hg status" extra-switches)))
    (set-process-sentinel process 'ahg-status-sentinel)
    (with-current-buffer buf
      (set (make-local-variable 'ahg-root) (ahg-root)))))

;;-----------------------------------------------------------------------------
;; hg commit
;;-----------------------------------------------------------------------------

(defun ahg-commit-callback ()
  (interactive)
  (let ((msg (buffer-string)))
    (let ((args (append (list "-m" msg)
                        (log-edit-files))))
      (ahg-generic-command "commit" args
                           (ahg-capturing-lambda (process status)
                              (if (string= status "finished\n")
                                  (let ((buf (ahg-get-status-buffer
                                              (capture (ahg-root)))))
                                    (when buf (ahg-status)))
                                (ahg-show-error process))))))
  (kill-buffer (current-buffer)))

(defun ahg-commit (files)
  "Run hg commit. Pops up a buffer for editing the log file."
  (interactive (list (buffer-file-name)))
  (let ((buf (generate-new-buffer "*aHg-log*")))
    (log-edit
     'ahg-commit-callback
     nil
     (list (cons 'log-edit-listfun
                 (ahg-capturing-lambda () (capture files))))
     buf)))

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
    (define-key map [?!] 'ahg-command)
    map)
  "Keymap used in `ahg-short-log-mode'.")

(defvar ahg-short-log-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ahg-short-log-view-details-mouse)
    (define-key map [mouse-2] 'ahg-short-log-view-details-mouse)
    map))

(defvar ahg-short-log-font-lock-keywords
  '(("^hg log for " . font-lock-comment-face)
    ("^-+" . 'bold)
    ("^ +Rev | .*" . 'bold)
    ("^hg log for \\(.*\\)$" 1 font-lock-constant-face)
    ("^ +\\([0-9]+\\)" 1 font-lock-function-name-face)
    ("^[^|]+| \\([0-9][^|]+\\)|" 1 font-lock-string-face)
    ("^\\([^|]+| \\)\\{2\\}\\([^|]+\\)|" 2 font-lock-type-face)
    )
  "Keywords in `ahg-short-log-mode' mode.")

(defconst ahg-short-log-start-regexp "^ +\\([0-9]+\\) |")

(define-derived-mode ahg-short-log-mode fundamental-mode "ahg-short-log"
  "Major mode to display hg shortlog output.

Commands:
\\{ahg-short-log-mode-map}
"
  (use-local-map ahg-short-log-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       (list 'ahg-short-log-font-lock-keywords t nil nil)))


(defun ahg-short-log-format (output &optional width)
  "Formats the output returned by hg log for displaying it in a short-log
buffer (see `ahg-short-log' and `ahg-short-log-mode')."
  (let ((lines (split-string output "\n")))
    (unless width
      (setq width (window-width (get-buffer-window (current-buffer)))))
    (labels ((trim (n s) (if (> (length s) n) (substring s 0 n) s)))
    (labels ((format-line (line)
                (if (and line (> (length line) 0))
                    (let* ((p1 (string-match " " line))
                           (p2 (string-match " " line (1+ p1)))
                           (p3 (string-match " " line (1+ p2)))
                           (s (format "%7s | %s | %8s | %s"
                                      (substring line 0 p1)
                                      (substring line (1+ p1) p2)
                                      (trim 8 (substring line (1+ p2) p3))
                                      (substring line (1+ p3))))
                           (pad (if (< (length s) width)
                                    (make-string (- width (length s)) ? )
                                  "")))
                      (concat s pad "\n"))
                  "")))
      (apply 'concat (mapcar 'format-line lines))))))


(defun ahg-short-log-mode-setup-lines ()
  "Sets up mouse navigation on short-log lines."
  ;; setup the lines
  (beginning-of-buffer)
  (while (not (eobp))
    (beginning-of-line)
    (when (string-match ahg-short-log-start-regexp
                        (buffer-substring (point-at-bol) (point-at-eol)))
      (add-text-properties (point-at-bol) (point-at-eol)
                           (list 'mouse-face 'highlight
                                 'keymap ahg-short-log-line-map)))
    (forward-line))
  (beginning-of-buffer)
  (ahg-short-log-next 1))


(defun ahg-short-log-next (n)
  "Move to the next changeset line"
  (interactive "p")
  (end-of-line)
  (re-search-forward ahg-short-log-start-regexp nil t n)
  (beginning-of-line))

(defun ahg-short-log-previous (n)
  "Move to the previous changeset line"
  (interactive "p")
  (end-of-line)
  (re-search-backward ahg-short-log-start-regexp)
  (re-search-backward ahg-short-log-start-regexp nil t n))

(defun ahg-short-log-revision-at-point ()
  (save-excursion
    (end-of-line)
    (re-search-backward ahg-short-log-start-regexp)
    (match-string-no-properties 1)))

(defun ahg-short-log-view-diff ()
  (interactive)
  (let ((r1 (ahg-short-log-revision-at-point))
        (r2 "tip"))
    (when (string-to-number r1)
      (setq r2 (number-to-string (1- (string-to-number r1)))))
    (ahg-diff r1 r2)))

(defun ahg-short-log-view-details (&optional rev)
  "View details of the given revision."
  (interactive)
  (when (interactive-p)
    (setq rev (ahg-short-log-revision-at-point)))
  (ahg-log rev nil))


(defun ahg-short-log-view-details-mouse (event)
  (interactive "e")
  (save-excursion
    (goto-char (posn-point (event-end event)))
    (ahg-short-log-view-details (ahg-short-log-revision-at-point))))


(defun ahg-short-log-goto-revision (rev)
  "Move point to the revision REV. If REV is not found in the log buffer,
do nothing."
  (interactive "P")
  (when (interactive-p)
    (unless rev
      (setq rev (read-string "Goto revision: "))))
  (let ((rev-pos))
    (save-excursion
      (when
          (re-search-forward (concat "^ +" rev " |") nil t)
        (setq rev-pos (point))))
    (when rev-pos
      (goto-char rev-pos) (beginning-of-line))))

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
                                            (append command-list (list "-r" r2)))))
    command-list))

(defun ahg-short-log (&optional r1 r2)
  "Run hg log, in a compressed format.
This displays the log in a tabular view, one line per changeset. The format of
each line is: Revision | Date | User | Summary.
When run interactively with a positve prefix argument, don't ask for revisions."
  (interactive "P")
  (when (interactive-p)
    (unless r1
      (setq r1 (read-string "hg log, R1: " "tip"))
      (setq r2 (read-string "hg log, R2: " "0"))))
  (let ((buffer (get-buffer-create (concat "*hg log (summary): " (ahg-root) "*")))
        (command-list (ahg-args-add-revs r1 r2)))  
    (setq command-list
          (append command-list
                  (list "--template"
                        "{rev} {date|shortdate} {author|user} {desc|firstline}\\n")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (ahg-generic-command "log" command-list
                         (lambda (process status)
                            (if (string= status "finished\n")
                                (with-current-buffer (process-buffer process)
                                  (ahg-short-log-mode)
                                  (let ((inhibit-read-only t)
                                        (contents (buffer-string)))
                                    (erase-buffer)
                                    (setq truncate-lines t)
                                    (insert (ahg-short-log-format contents))
                                    (goto-char (point-min))
                                    (insert (format "hg log for %s\n\n" default-directory))
                                    (let ((l (concat (make-string
                                                      (window-width (get-buffer-window
                                                                     (current-buffer))) ?-) "\n")))
                                      (insert l)
                                      (insert "    Rev |    Date    |  Author  | Summary\n")
                                      (insert l)
                                      (goto-char (point-max))
                                      (insert l))
                                    (goto-line 6)
                                    (ahg-short-log-mode-setup-lines)
                                    (toggle-read-only 1))
                                  (pop-to-buffer (current-buffer)))
                              (ahg-show-error process)))
                         buffer)))


(defvar ahg-log-mode-map
  (let ((map (copy-keymap diff-mode-shared-map)))

    ;; the merge group
    (define-key map (dvc-prefix-merge ?u) 'dvc-update)
    (define-key map (dvc-prefix-merge ?f) 'dvc-pull) ;; hint: fetch, p is reserved for push
    (define-key map (dvc-prefix-merge ?m) 'dvc-missing)
    map)
  "Keymap used in `ahg-log-mode'.")


(defvar ahg-log-font-lock-keywords
  '(("^hg \\<[a-z]+\\> for" . font-lock-comment-face)
    ("^hg \\<[a-z]+\\> for \\(.*\\)" 1 font-lock-constant-face)
    ("^changeset:" . font-lock-function-name-face)
    ("^tag:" . font-lock-function-name-face)
    ("^user:" . font-lock-function-name-face)
    ("^date:" . font-lock-function-name-face)
    ("^summary:" . font-lock-function-name-face)
    ("^files:" . font-lock-function-name-face)
    ("^branch:" . font-lock-function-name-face)
    ("^parent:" . font-lock-function-name-face)
    ("^description:" . font-lock-function-name-face)
    ("^\\(changeset\\|parent\\): +\\(.+\\)$" 2 font-lock-variable-name-face)
    ("^\\(tag\\|branch\\): +\\(.+\\)$" 2 font-lock-keyword-face)
    ("^user: +\\(.+\\)$" 1 font-lock-type-face)
    ("^date: +\\(.+\\)$" 1 font-lock-string-face)
    )
  "Keywords in `ahg-log-mode' mode.")

(define-derived-mode ahg-log-mode nil "ahg-log"
  "Major mode to display hg log output with embedded diffs. Derives from `diff-mode'.

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
  (define-key ahg-log-mode-map [?!] 'ahg-command)
  (set (make-local-variable 'font-lock-defaults)
       (list 'ahg-log-font-lock-keywords t nil nil)))

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
    (ahg-diff r1 r2)))

(defun ahg-log-revision-at-point (&optional short-id)
  (save-excursion
    (end-of-line)
    (re-search-backward ahg-log-start-regexp)
    (let ((rev (match-string-no-properties 1)))
      (when rev
        (if short-id (car (split-string rev ":"))
          (cadr (split-string rev ":")))))))


(defun ahg-log-goto-revision (rev)
  "Move point to the revision REV. If REV is not found in the log buffer, do nothing."
  (let ((rev-pos))
    (save-excursion
      (when
          (re-search-forward (concat "^changeset: +" rev) nil t)
        (setq rev-pos (point))))
    (when rev-pos
      (goto-char rev-pos))))

(defun ahg-log (&optional r1 r2)
  "Run hg log. When run interactively with a positve prefix
argument, don't ask for revisions."
  (interactive "P")
  (when (interactive-p)
    (unless r1
      (setq r1 (read-string "hg log, R1: " "tip"))
      (setq r2 (read-string "hg log, R2: " "0"))))
  (let ((buffer (get-buffer-create (concat "*hg log (details): " (ahg-root) "*")))
        (command-list (ahg-args-add-revs r1 r2)))  
    (setq command-list (append command-list (list "-v")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (ahg-generic-command "log" command-list
                         (lambda (process status)
                            (if (string= status "finished\n")
                                (with-current-buffer (process-buffer process)
                                  (ahg-log-mode)
                                  (beginning-of-buffer)
                                  (pop-to-buffer (current-buffer)))
                              (ahg-show-error process)))
                         buffer)))

;;-----------------------------------------------------------------------------
;; hg diff
;;-----------------------------------------------------------------------------

(defun ahg-diff (&optional r1 r2)
  (interactive "P")
  (when (interactive-p)
    (unless r1
      (setq r1 (read-string "hg diff, R1: " "tip"))
      (setq r2 (read-string "hg diff, R2: " ""))))
  (let ((buffer (get-buffer-create "*hg diff*"))
        (command-list (ahg-args-add-revs r1 r2 t)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (ahg-generic-command "diff" command-list
                         (lambda (process status)
                           (if (string= status "finished\n")
                               (with-current-buffer (process-buffer process)
                                 (diff-mode)
                                 (view-mode)
                                 (beginning-of-buffer)
                                 (pop-to-buffer (current-buffer)))
                             (ahg-show-error process)))
                         buffer)))

;;-----------------------------------------------------------------------------
;; hg command
;;-----------------------------------------------------------------------------

(defun ahg-complete-command (command)
  ;; we split the string, and treat the last word as a filename
  (let* ((idx (string-match "\\([^ \\t]+\\)$" command))
         (matches (file-expand-wildcards (concat (substring command idx) "*")))
         (prev (substring command 0 idx)))
    (mapcar (function (lambda (a) (concat prev a))) matches)))

(defun ahg-command (cmdstring)
  (interactive (list
                (let ((minibuffer-local-completion-map
                       (copy-keymap minibuffer-local-map)))
                  (define-key minibuffer-local-completion-map "\t"
                    'minibuffer-complete)
                  (completing-read "Hg command: "
                                   (dynamic-completion-table
                                    ahg-complete-command)))))
  (let* ((args (split-string cmdstring))
         (cmdname (car args))
         (buffer (get-buffer-create (concat "*hg " cmdname ": "
                                            (ahg-root) "*"))))
    (ahg-generic-command cmdname (cdr args)
                         (lambda (process status)
                           (if (string= status "finished\n")
                               (with-current-buffer (process-buffer process)
                                 (beginning-of-buffer)
                                 (ahg-command-mode)
                                 (pop-to-buffer (current-buffer)))
                             (ahg-show-error process)))
                         buffer)))

;;-----------------------------------------------------------------------------
;; Various helper functions
;;-----------------------------------------------------------------------------

(defun ahg-buffer-quit ()
  (interactive)
  (kill-buffer (current-buffer))
  (or (one-window-p) (delete-window)))

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
  "Major mode for aHg commands"
  (toggle-read-only t)
  (define-key ahg-command-mode-map "q" 'ahg-buffer-quit))

;; This is a cut&paste from dvc-capturing-lambda from the DVC package, and the
;; code is Copyright (C) by Matthieu Moy

(eval-and-compile
  (defvar ahg-gensym-counter 0)

  (defun ahg-gensym (&optional arg)
    "Generate a new uninterned symbol.
    The name is made by appending a number to PREFIX, default
\"dvc\"."
    (let* ((prefix (if (stringp arg) arg "ahg-gensym-uniq-"))
           (num (if (integerp arg) arg
                  (prog1
                      ahg-gensym-counter
                    (setq ahg-gensym-counter (1+
                                               ahg-gensym-counter)))))
           (symbol (make-symbol (format "%s%d" prefix num))))
      (eval `(defvar ,symbol nil "lint trap"))
      symbol))


  (defun ahg-capturing-lambda-helper (l)
    (cond ((atom l) l)
          ((eq (car l) 'capture)
           (let ((g (ahg-gensym)))
             (push (list g (cadr l)) captured-values)
             g))
          (t (mapcar 'ahg-capturing-lambda-helper l))))

  (defmacro ahg-capturing-lambda (args &rest body)
    "A `lambda' capable of capturing values from its defining
environment.
    Values to be captured should be surrounded by (capture ...).
    For example:

      (let* ((x 'lexical-x)
             (y 'lexical-y)
             (l (ahg-capturing-lambda (arg)
                  (list x (capture y) arg))))
        (let ((y 'dynamic-y)
              (x 'dynamic-x))
          (funcall l 'arg)))

    => (dynamic-x lexical-y 'arg)
    "
    (let ((captured-values '()))
      (let ((body (ahg-capturing-lambda-helper body)))
        (` (` (lambda (, (quote (, args)))
                (let ( (, (,@ (mapcar (lambda (var)
                                        (` (list '(, (car var))
                                                 (list 'quote (, (cadr var))))))
                                      captured-values))))
                  (funcall (, (lambda () . (, body)))))))))))
  )
