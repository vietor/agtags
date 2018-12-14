;;; agtags.el --- emacs frontend to GNU Global -*- lexical-binding: t; -*-

;;; Commentary:
;; A package to integrate GNU Global source code tagging system
;; (http://www.gnu.org/software/global) with Emacs.

;;; Code:
(require 'grep)

(defvar agtags-mode)

(defgroup agtags nil
  "GNU Global source code tagging system."
  :group 'tools)

(defcustom agtags-key-prefix "C-x t"
  "It is used for the prefix key of agtags's command."
  :safe 'stringp
  :type 'string
  :group 'agtags)

(defcustom agtags-global-ignore-case nil
  "Non-nil if Global should ignore case in the search pattern."
  :safe 'booleanp
  :type 'boolean
  :group 'agtags)

(defcustom agtags-global-treat-text nil
  "Non-nil if Global should include matches from text files.
This affects 'agtags/find-file' and 'agtags/find-grep'."
  :safe 'booleanp
  :type 'boolean
  :group 'agtags)

(defvar agtags/history-list nil
  "Gtags history list.")

(defconst agtags/display-buffer-dwim '((display-buffer-reuse-window
                                        display-buffer-same-window)
                                       (inhibit-same-window . nil)))

;;
;; The private functions
;;

(defun agtags/quote (string)
  "Return a regular expression whose only exact match is STRING."
  (let ((s string))
    (when (not (string-match-p "\\\\" s))
      (setq s (regexp-quote s)))
    (when (string-match-p "^-" s)
      (setq s (concat "\\" s)))
    s))

(defun agtags/shell-quote (string)
  "Return a regular expression whose only exact match is STRING for shell."
  (shell-quote-argument (agtags/quote string)))

(defun agtags/get-root ()
  "Get and validate env  `GTAGSROOT`."
  (let ((dir (getenv "GTAGSROOT")))
    (when (zerop (length dir))
      (error "No env `GTAGSROOT` provided"))
    dir))

(defun agtags/is-active ()
  "Test global was created."
  (let ((dir (getenv "GTAGSROOT")))
    (and (> (length dir) 0)
         (file-regular-p (expand-file-name "GTAGS" dir)))))

(defun agtags/run-global-to-list (arguments)
  "Execute the global command to list, use ARGUMENTS; Return nil if an error occured."
  (let ((default-directory (agtags/get-root)))
    (condition-case nil
        (apply #'process-lines "global" arguments)
      (error nil))))

(defun agtags/run-global-to-mode (arguments &optional result)
  "Execute the global command to agtags-*-mode, use ARGUMENTS; output format use RESULT."
  (let* ((xr (or result "grep"))
         (xs (append (list "global"
                           (format "--result=%s" xr)
                           (and agtags-global-ignore-case "--ignore-case")
                           (and agtags-global-treat-text "--other"))
                     arguments))
         (default-directory (agtags/get-root))
         (display-buffer-overriding-action agtags/display-buffer-dwim))
    (compilation-start (mapconcat #'identity (delq nil xs) " ")
                       (if (string= xr "path") 'agtags-path-mode 'agtags-grep-mode))))

(defun agtags/run-global-completing (flag string predicate code)
  "Completion Function with FLAG for `completing-read'. Require: STRING PREDICATE CODE."
  (let* ((xs (append (list "-c"
                           (and (eq flag 'files) "--path")
                           (and (eq flag 'rtags) "--reference")
                           (and agtags-global-ignore-case "--ignore-case")
                           (and agtags-global-treat-text "--other"))))
         (complete-list (agtags/run-global-to-list (delq nil xs))))
    (cond ((eq code nil)
           (try-completion string complete-list predicate))
          ((eq code t)
           (all-completions string complete-list predicate))
          ((eq code 'lambda)
           (if (intern-soft string complete-list) t nil)))))

(defun agtags/read-dwim ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

(defun agtags/read-input (prompt)
  "Read a value from the minibuffer with PROMPT."
  (let* ((final-prompt (format "%s: " prompt))
         (user-input (read-from-minibuffer final-prompt nil nil nil agtags/history-list)))
    user-input))

(defun agtags/read-input-dwim (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, offer that as a default."
  (let* ((suggested (agtags/read-dwim))
         (final-prompt (if suggested
                           (format "%s (default %s): " prompt suggested)
                         (format "%s: " prompt)))
         (user-input (read-from-minibuffer
                      final-prompt
                      nil nil nil agtags/history-list suggested)))
    (if (> (length user-input) 0) user-input suggested)))

(defun agtags/read-completing (flag prompt)
  "Read a value from the Completion by FLAG with PROMPT."
  (let* ((final-prompt (format "%s: " prompt))
         (user-input (completing-read
                      final-prompt
                      (lambda (string predicate code)
                        (agtags/run-global-completing flag string predicate code))
                      nil nil nil agtags/history-list)))
    user-input))

(defun agtags/read-completing-dwim (flag prompt)
  "Read a value from the Completion by FLAG with PROMPT.
If there's a string at point, offer that as a default."
  (let* ((suggested (agtags/read-dwim))
         (final-prompt (if suggested
                           (format "%s (default %s): " prompt suggested)
                         (format "%s: " prompt)))
         (user-input (completing-read
                      final-prompt
                      (lambda (string predicate code)
                        (agtags/run-global-completing flag string predicate code))
                      nil nil nil agtags/history-list suggested)))
    (if (> (length user-input) 0) user-input suggested)))

;;
;; The agtags-*-mode support
;;

(defun agtags/auto-update()
  "Auto update tags file, when buffer was save."
  (when (and agtags-mode
             buffer-file-name
             (agtags/is-active)
             (string-prefix-p (agtags/get-root) buffer-file-name))
    (call-process "global" nil nil nil "-u" (concat "--single-update=" buffer-file-name))))

(defun agtags/kill-window ()
  "Quit agtags-*-mode window and kill its buffer."
  (interactive)
  (quit-window t))

(defadvice compile-goto-error (around agtags activate)
  "Use same window when goto selected."
  (let ((display-buffer-overriding-action agtags/display-buffer-dwim))
    ad-do-it))

(defconst agtags/global-mode-font-lock-keywords
  '(("^Global \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (1 'compilation-error)
     (2 'compilation-error nil t))
    ("^Global found \\([0-9]+\\)" (1 compilation-info-face))))

(defconst agtags/button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'agtags/goto-selected)
    map))

(defconst agtags/global-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'compile-goto-error)
    (define-key map "\r" 'compile-goto-error)
    (define-key map "\C-m" 'compile-goto-error)
    (define-key map "q" 'agtags/kill-window)
    (define-key map "g" 'recompile)
    (define-key map "n" 'compilation-next-error)
    (define-key map "p" 'compilation-previous-error)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    map))

(defconst agtags/path-regexp-alist
  `((,"^\\(?:[^\"'\n]*/\\)?[^ )\t\n]+$" 0)))

(defconst agtags/grep-regexp-alist
  `((,"^\\(.+?\\):\\([0-9]+\\):\\(?:$\\|[^0-9\n]\\|[0-9][^0-9\n]\\|[0-9][0-9].\\)"
     1 2
     (,(lambda ()
         (let* ((start (1+ (match-end 2)))
                (mbeg (text-property-any start (line-end-position) 'global-color t)))
           (and mbeg (- mbeg start)))))
     nil 1)))

(defun agtags/global-mode-finished (buffer status)
  "Function to call when a gun global process finishes.
BUFFER is the global's mode buffer, STATUS was the finish status."
  (let* ((name (buffer-name buffer))
         (dname (if (string= name "*agtags-grep*")
                    "*agtags-path*"
                  "*agtags-grep*"))
         (dbuffer (get-buffer dname)))
    (when dbuffer
      (delete-windows-on dbuffer)
      (kill-buffer dbuffer))))

(defvar agtags-grep-mode-map agtags/global-mode-map)
(defvar agtags-grep-mode-font-lock-keywords agtags/global-mode-font-lock-keywords)

;;;###autoload
(define-derived-mode agtags-grep-mode grep-mode "Global Grep"
  "A mode for showing outputs from gnu global."
  (setq-local grep-scroll-output nil)
  (setq-local grep-highlight-matches nil)
  (setq-local compilation-always-kill t)
  (setq-local compilation-disable-input t)
  (setq-local compilation-error-screen-columns nil)
  (setq-local compilation-scroll-output 'first-error)
  (setq-local compilation-error-regexp-alist agtags/grep-regexp-alist)
  (setq-local compilation-finish-functions #'agtags/global-mode-finished))

(defvar agtags-path-mode-map agtags/global-mode-map)
(defvar agtags-path-mode-font-lock-keywords agtags/global-mode-font-lock-keywords)

;;;###autoload
(define-compilation-mode agtags-path-mode "Global Files"
  "A mode for showing files from gnu global."
  (setq-local compilation-error-face grep-hit-face)
  (setq-local compilation-always-kill t)
  (setq-local compilation-disable-input t)
  (setq-local compilation-error-screen-columns nil)
  (setq-local compilation-scroll-output 'first-error)
  (setq-local compilation-error-regexp-alist agtags/path-regexp-alist)
  (setq-local compilation-finish-functions #'agtags/global-mode-finished))

;;;###autoload
(define-minor-mode agtags-mode nil
  :lighter " G"
  (if agtags-mode
      (add-hook 'before-save-hook 'agtags/auto-update nil 'local)
    (remove-hook 'before-save-hook 'agtags/auto-update 'local)))

;;
;; The interactive functions
;;

(defun agtags/update-tags ()
  "Create or Update tag files (e.g. GTAGS) in directory `GTAGSROOT`."
  (interactive)
  (let ((rootpath (agtags/get-root)))
    (dolist (file (list "GRTAGS" "GPATH" "GTAGS"))
      (ignore-errors
        (delete-file (expand-file-name file rootpath))))
    (with-temp-buffer
      (cd rootpath)
      (when (zerop (call-process (executable-find "gtags") nil t nil "-i"))
        (message "Tags create or update by GTAGS")))))

(defun agtags/open-file ()
  "Input pattern and move to the top of the file."
  (interactive)
  (let ((user-input (agtags/read-completing 'files "Open file")))
    (when (> (length user-input) 0)
      (find-file (expand-file-name user-input (agtags/get-root))))))

(defun agtags/find-file ()
  "Input pattern, search file and move to the top of the file."
  (interactive)
  (let ((user-input (agtags/read-input "Find files")))
    (when (> (length user-input) 0)
      (agtags/run-global-to-mode (list "--path" (agtags/shell-quote user-input)) "path"))))

(defun agtags/find-tag ()
  "Input tag and move to the locations."
  (interactive)
  (let ((user-input (agtags/read-completing-dwim 'tags "Find tag")))
    (when (> (length user-input) 0)
      (agtags/run-global-to-mode (list (agtags/shell-quote user-input))))))

(defun agtags/find-rtag ()
  "Input rtags and move to the locations."
  (interactive)
  (let ((user-input (agtags/read-completing-dwim 'rtags "Find rtag")))
    (when (> (length user-input) 0)
      (agtags/run-global-to-mode (list "--reference" (agtags/shell-quote user-input))))))

(defun agtags/find-with-grep ()
  "Input pattern, search with grep(1) and move to the locations."
  (interactive)
  (let ((user-input (agtags/read-input-dwim "Search string")))
    (when (> (length user-input) 0)
      (agtags/run-global-to-mode (list "--grep" (agtags/shell-quote user-input))))))

(defun agtags/switch-dwim ()
  "Switch to last agtags-*-mode buffer."
  (interactive)
  (let ((buffer (or (get-buffer "*agtags-grep*")
                    (get-buffer "*agtags-path*")
                    (other-buffer (current-buffer) 1))))
    (when buffer
      (switch-to-buffer buffer))))

;;
;; The public functions
;;

;;;###autoload
(defun agtags-bind-keys()
  "Set global key bindings for agtags."
  (dolist (pair '(("q" . agtags/switch-dwim)
                  ("b" . agtags/update-tags)
                  ("f" . agtags/open-file)
                  ("F" . agtags/find-file)
                  ("t" . agtags/find-tag)
                  ("r" . agtags/find-rtag)
                  ("p" . agtags/find-with-grep)))
    (global-set-key (kbd (concat agtags-key-prefix " " (car pair))) (cdr pair))))

;;;###autoload
(defun agtags-update-root (root)
  "Set ROOT directory of the project for agtags."
  (setenv "GTAGSROOT" root))

;;;###autoload
(defun agtags-update-parser (&optional parser)
  "Set parser to PARSER for agtags or smart parser it."
  (if (and (stringp parser)(> (length parser) 0))
      (setenv "GTAGSLABEL" parser)
    (let* ((exe-ctags (executable-find "ctags"))
           (exe-uctags (and exe-ctags
                            (with-temp-buffer
                              (call-process "ctags" nil t nil "--version")
                              (goto-char (point-min))
                              (looking-at "Universal Ctags"))))
           (label (cond (exe-uctags "new-ctags")
                        ((and exe-ctags (executable-find "pygmentize")) "pygments")
                        (exe-ctags "ctags")
                        (t "default"))))
      (setenv "GTAGSLABEL" label))))

(provide 'agtags)
;;; agtags.el ends here