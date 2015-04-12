;;; package --- summary
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(company-backends (quote (company-anaconda company-c-headers company-elisp company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake company-capf company-anaconda (company-dabbrev-code company-gtags company-etags company-keywords) company-oddmuse company-files company-dabbrev)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 1)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes (quote ("c40361c0bbeb6ad640e66234c7f903c84cf667e8a0162630040866b1f9275a18" default)))
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:background "DarkOrange3" :foreground "gainsboro"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "white"))))
 '(company-scrollbar-bg ((t (:inherit company-tooltip :background "gainsboro"))))
 '(company-scrollbar-fg ((t (:background "slategray"))))
 '(company-template-field ((t (:background "gray32" :foreground "white"))))
 '(company-tooltip ((t (:background "gray32" :foreground "white"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :foreground "lightskyblue"))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "lightskyblue"))))
 '(company-tooltip-mouse ((t (:background "darkorange2" :foreground "white"))))
 '(company-tooltip-selection ((t (:inherit company-tooltip :background "darkorange2"))))
 '(cursor ((t (:background "gainsboro"))))
 '(font-lock-builtin-face ((t (:foreground "#e6a8df"))))
 '(font-lock-comment-face ((t (:foreground "dark gray"))))
 '(font-lock-string-face ((t (:foreground "light goldenrod"))))
 '(font-lock-variable-name-face ((t (:foreground "#fdbd47"))))
 '(mode-line ((t (:background "#494F51" :foreground "#d5d9d1" :box (:line-width -1 :style released-button))))))

;; Paren matching display
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(show-paren-mode t)
(set-face-background 'show-paren-match-face "#444444")

;; Setting background color in terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "#212526" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)


;;bracket autocomplete

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(global-linum-mode t)
(tool-bar-mode -1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Add .pixie to css mode
(add-to-list 'auto-mode-alist '("\\.pixie\\'" . css-mode))

;; Flycheck mode: syntax error highlightig (practically an IDE)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Make scrolling better
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(3))


;; Text completion - company package
(add-to-list 'load-path "/home/ngraves3/.emacs.d/elpa/company-0.8.9/")
(autoload 'company-mode "company" nil t)
(add-hook 'after-init-hook 'global-company-mode)

;; Map C-q to autocomplete
(global-set-key "\C-q" 'company-complete)
(global-set-key "\M-q" 'company-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here be some extra package magic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


(defun replace-all (old new)
  "Function to replace all instances of a string with another"
  (interactive "sString to replace: \nsReplace %s with: ")
  (let ((number-replaced 1))
    (goto-char(point-min))
    (while (search-forward old nil t)
      (replace-match new)
      (setq number-replaced (+ number-replaced 1)))
    (message "%d instances of %s replaced with %s"
	     number-replaced
	     old
	     new)))

(defun exec-file ()
  "Function to execute the current file, however is necessary"
  (interactive)
  (let ((relative-name (concat "./" (buffer-name))))
    (set-file-modes relative-name #o755)
    (when (eq 126 (shell-command relative-name))
      ;; value of 126 == permission denied
      (let ((home-name (concat "~/" (buffer-name))))
	(let ((nix-ending (string-match "unix" (symbol-name buffer-file-coding-system))))
	  (set-buffer-file-coding-system 'undecided-unix t)
	  (save-buffer)
	  (copy-file relative-name home-name)
	  (set-file-modes home-name #o755)

	  (shell-command home-name)
	  (delete-file home-name)
	  (when (eq nix-ending nil)
	    (set-buffer-file-coding-system 'undecided-dos t)
	    (save-buffer)))))))


(setq decl-stack '())
;; Jump to function declaration feature
(defun jump-to-function-declaration ()
  "Feature to highlight a function name and jump to its declaration, similar to in an IDE"
  (interactive)
  ;; Consider adding a single paren to the beginning of the function finding regex
  (let ((function-regex (concat "^[#\\(]?[ ]*[A-Za-z][^ ]+[ ]+" (buffer-substring-no-properties
						   (+ (point) (skip-chars-backward "A-Za-z0-9_\\-"))
						   (+ (point) (skip-chars-forward "A-Za-z0-9_\\-")))))) ;; "^;^)^(^[^]^,^\\.^\"^ "
    (push (buffer-name) decl-stack)
    ;;(message "Regex is: %s" function-regex)
    ;; try executing the command:
    ;;     grep -r -E function-regex ../
    (let ((containing-file (shell-command-to-string
			    (concat "grep -r -l -E '" function-regex "' --exclude=*.gz ../"))))
      ;; need to return early if nil
      (setq containing-file (split-string containing-file "\n"))

      (when (not (equal (car containing-file) ""))
	 (switch-to-buffer (set-buffer
			 (find-file-noselect
			  (expand-file-name (car containing-file)))))

	 (when (re-search-forward function-regex nil t 1)
	    (goto-char (match-beginning 0))))))) ;; go to start of match


(defun unjump-to-function-declaration ()
  (interactive)
  (let ((next-buffer (pop decl-stack)))
    (when (and (not (equal next-buffer nil))
	       (not (equal (buffer-name) next-buffer)))
      (kill-buffer)
      (switch-to-buffer next-buffer))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text-Jump
;;;
;;; Functions for setting jump points for quick navigation
;;; through pages. they allow  quick navigation to the last
;;; point of inserted text. currently only works backwards,
;;; so after jumping to a point of text, you cannot jump
;;; forward.
;;;
;;; Created by Nick Graves 3/27/15
;;;
(setq jump-stack '())
(defun set-jump-point ()
  "Sets a jump point to return to."
  (interactive)
  (push (list (current-buffer) (point)) jump-stack))

(defun goto-jump-point ()
  "Returns to the most recently placed jump point."
  (interactive)
  (when (not (equal jump-stack nil))
    (let ((buffer-point (pop jump-stack)))
      (switch-to-buffer (car buffer-point))
      (goto-char (car (cdr buffer-point))))))

;; advise set-jump-point on the self-insert-command function so it automatically tracks text insertion. this command is used everytime a key is pressed.
;; it will jump to the beginning of the last text burst (where a text burst is defined as a series of keystrokes uninterrupted by non-deletion movement)
(defadvice self-insert-command (before track-text-insertion)
  "Everytime text is entered in the buffer, a jump is added to the jump-stack. This allows for jumping to last inserted text"
  (when (and (not (eq last-command 'self-insert-command))
	   (not (eq last-command 'autopair-backspace))) ; always in autopair mode. use delete-backward-char when not.
      (set-jump-point)))

;; Activate text jump advice
(ad-activate 'self-insert-command)

;;; End jump point nav
;;;;;;;;;;;;;;;;;;;;;;

;; map f3  to jump-to-function-declaration and f4 to jump back
;; just like in an IDE
(global-set-key (kbd "<f3>") 'jump-to-function-declaration)
(global-set-key (kbd "<f4>") 'unjump-to-function-declaration)

;;Map C-c C-e to exec-file
(global-set-key (kbd "C-c C-e") 'exec-file)

;; Unset lisp completion at point
;;(global-unset-key "\C-\M-i")

;;; HTML / CSS
;; map C-. to close-tag
(add-hook 'html-mode-hook (lambda () (local-set-key [67108910] (quote sgml-close-tag))))
(add-hook 'html-mode-hook (lambda () (local-set-key (kbd "TAB") 'sgml-indent-line)))

;; Add rainbow mode to CSS mode and Javascript mode
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'javascript-mode-hook 'rainbow-mode)

;;; C preferences
(setq c-default-style "k&r")
(setq-default c-basic-offset 4)
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c-mode-hook (lambda () (local-set-key (kbd "<f2>") 'man-follow)))
(add-hook 'c-mode-hook (lambda () (local-set-key (kbd "<f1>") 'delete-other-windows)))

;;; PYTHON
(defun pydoc-under-point ()
  (interactive)
  (pydoc (buffer-substring-no-properties
			     (+ (point) (skip-chars-backward "A-Za-z0-9_\\-"))
			     (+ (point) (skip-chars-forward "A-Za-z0-9_\\-")))))

(add-hook 'python-mode-hook (lambda () (local-set-key (kbd "<f2>") 'pydoc-under-point)))
;; Indentation is frustrating with default python indenting. Map tab to and C-i to shift right
;;     and shift-tab and C-I to shift left
(add-hook 'python-mode-hook (lambda () (progn
					 (local-set-key (kbd "C-i") 'python-indent-shift-right)
					 (local-set-key (kbd "TAB") 'python-indent-shift-right))))
(add-hook 'python-mode-hook (lambda () (progn
					 (local-set-key  "\C-\M-i" 'python-indent-shift-left)
					 (local-set-key (kbd "<S-tab>") 'python-indent-shift-left))))



;; Bind f1 to delete window. F1 was previously a help function I never used
(global-set-key (kbd "<f1>") 'delete-window)

;; Nice dired mode
(require 'dired-details)
(dired-details-install)

;; Window jump nav keys. use C-c + i-j-k-l to move around
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c j") 'windmove-left)
(global-set-key (kbd "C-c k") 'windmove-down)
(global-set-key (kbd "C-c i") 'windmove-up)


;; M-l is already mapped to forward-word, map M-j to backward word
;; Alternate Navigation keys. Most of these weren't mapped to anything
;; Can use either i-j-k-l as a keypad, or use M-p and M-n as training wheels
(global-set-key "\M-j" 'backward-word)
(global-set-key "\M-l" 'forward-word)
(global-set-key "\M-i" 'previous-line)
(global-set-key "\M-k" 'next-line)

;; Jumps to the beginning of the next paragraph
;; or consecutive lines of code.
(global-set-key "\M-n" (lambda ()
			 (interactive)
			 (when (eq mark-active nil)
			   (set-mark-command nil))
			 (search-forward-regexp "^$")
			 (forward-char)))

;; Jumps to the beginning of the previous paragraph
;; or consecutive lines of code.
(global-set-key "\M-p" (lambda ()
			 (interactive)
			 (when (eq mark-active nil)
			   (set-mark-command nil))
			 (backward-char)
			 (backward-char)
			 (search-backward-regexp "^$")
			 (forward-char)))

;; Try mapping u->backward char and o->forward char
(global-set-key "\M-o" 'forward-char)
(global-set-key "\M-u" 'backward-char)

;; Set keys for jump navigation
(global-set-key "\M-[" 'set-jump-point)
(global-set-key "\M-]" 'goto-jump-point)

;; Future development: make a binding for git add/commit/push. bind one chord to git
;; and the others to add/commit/push. Consideration: \M-# for git


(global-set-key (kbd "C-(") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-)") 'kmacro-end-or-call-macro)

(provide '.emacs)
;;; .emacs ends here
