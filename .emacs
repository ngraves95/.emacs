;;; package --- summary
;;; Commentary:
;;; Code:
(setq inhibit-startup-screen t)
(tool-bar-mode nil)

;; Mac specific things
(setq mac-option-key-is-meta nil)
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Deployment
(defun load-or-install-package (package-name)
  "Load PACKAGE-NAME, and if that fails, install it."
  (when (not (require package-name nil 'return-nil-instead-of-error))
    (package-install package-name)))

(mapc 'load-or-install-package '(autopair
                                 company
                                 dired-details
                                 dired-subtree
                                 flycheck
                                 go-mode
                                 god-mode
                                 god-mode-isearch
                                 google-this
                                 paredit
                                 shell-toggle
                                 xcscope))

;; Customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(company-backends
   (quote
    (company-elisp company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake company-capf
                   (company-dabbrev-code company-gtags company-etags company-keywords)
                   company-oddmuse company-files company-dabbrev)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 1)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("c40361c0bbeb6ad640e66234c7f903c84cf667e8a0162630040866b1f9275a18" default)))
 '(package-selected-packages
   (quote
    (go-mode xcscope shell-toggle paredit google-this god-mode flycheck dired-subtree dired-details company autopair))))

(add-to-list 'company-backends '(company-capf company-dabbrev-code))

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
  "Set terminal settings."
  (if (display-graphic-p (selected-frame))
      (global-linum-mode t)
    (set-face-background 'default "#212526" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook 'global-flycheck-mode)



;; Make scrolling better
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(3))

;; Text completion - company package
(add-to-list 'load-path "/home/ngraves3/.emacs.d/elpa/company-0.8.12/")
(add-hook 'after-init-hook 'global-company-mode)

(defun replace-all (old new)
  "Replace all instances of OLD with NEW."
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
  "Set a jump point to return to."
  (interactive)
  (push (list (current-buffer) (point)) jump-stack))

(defun goto-jump-point ()
  "Return to the most recently placed jump point."
  (interactive)
  (when (not (equal jump-stack nil))
    (let ((buffer-point (pop jump-stack)))
      (switch-to-buffer (car buffer-point))
      (goto-char (car (cdr buffer-point))))))

;; advise set-jump-point on the self-insert-command function so it automatically tracks text insertion. this command is used everytime a key is pressed.
;; it will jump to the beginning of the last text burst (where a text burst is defined as a series of keystrokes uninterrupted by non-deletion movement)
(defadvice self-insert-command (before track-text-insertion)
  "Everytime text is entered in the buffer, a jump is added to the jump-stack. This allows for jumping to last inserted text"
  (when (and
	 (not (eq last-command 'self-insert-command))
	 ;(not (eq last-command 'autopair-backspace))
	 (not (string-prefix-p "*" (string-trim (buffer-name)))))
    (set-jump-point)))

;; Activate text jump advice
(ad-activate 'self-insert-command)

;;; End jump point nav
;;;;;;;;;;;;;;;;;;;;;;

;;; HTML / CSS
;; map C-. to close-tag
(add-hook 'html-mode-hook (lambda () (local-set-key [67108910] (quote sgml-close-tag))))
(add-hook 'html-mode-hook (lambda () (local-set-key (kbd "TAB") 'sgml-indent-line)))

(add-to-list 'auto-mode-alist '("\\.view\\'" . html-mode))

;; Add rainbow mode to CSS mode and Javascript mode
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'javascript-mode-hook 'rainbow-mode)

;;; C preferences
;;(setq c-default-style "linux")
(setq c-default-style "gnu")
(setq-default c-basic-offset 2)
(add-hook 'c-mode-hook (lambda () (local-set-key (kbd "<f2>") 'man-follow)))
(add-hook 'c-mode-hook (lambda () (local-set-key (kbd "<f1>") 'delete-other-windows)))

;;; PYTHON
;; Indentation is frustrating with default python indenting. Map tab to and C-i to shift right
;;     and shift-tab and C-I to shift left
;; (add-hook 'python-mode-hook (lambda () (progn
;; 					 (local-set-key (kbd "C-i") 'python-indent-shift-right)
;; 					 (local-set-key (kbd "TAB") 'python-indent-shift-right))))
;; (add-hook 'python-mode-hook (lambda () (progn
;; 					 (local-set-key  "\C-\M-i" 'python-indent-shift-left)
;; 					 (local-set-key (kbd "<S-tab>") 'python-indent-shift-left))))

;;; Window Functions ;;;
;; Window jump nav keys. use C-c + i-j-k-l to move around
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c j") 'windmove-left)
(global-set-key (kbd "C-c k") 'windmove-down)
(global-set-key (kbd "C-c i") 'windmove-up)

;; This is a convenience for God mode so cl moves right, e.g.
(global-set-key (kbd "C-c C-l") 'windmove-right)
(global-set-key (kbd "C-c C-j") 'windmove-left)
(global-set-key (kbd "C-c C-k") 'windmove-down)
(global-set-key (kbd "C-c C-i") 'windmove-up)

;; More conveniences for God mode.
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-0") 'delete-window)

;; M-l is already mapped to forward-word, map M-j to backward word
;; Alternate Navigation keys. Most of these weren't mapped to anything
;; Can use either i-j-k-l as a keypad, or use M-p and M-n as training wheels

;; (global-set-key "\M-j" 'backward-word)
;; (global-set-key "\M-l" 'forward-word)
;; (global-set-key "\M-i" 'previous-line)
;; (global-set-key "\M-k" 'next-line)

(global-set-key "\M-j" (lambda ()
			 (interactive)
			 (next-logical-line)
			 (recenter)))

(global-set-key "\M-k" (lambda ()
			 (interactive)
			 (previous-logical-line)
			 (recenter)))

(global-set-key (kbd "C-'") 'backward-paragraph)
(global-set-key (kbd "C-;") 'forward-paragraph)

;; Jumps to the beginning of the next paragraph
;; or consecutive lines of code.
(global-set-key "\M-n" (lambda ()
			 (interactive)
			 (when (eq mark-active nil)
			   (set-mark-command nil))
			 (forward-paragraph)))

;; Jumps to the beginning of the previous paragraph
;; or consecutive lines of code.
(global-set-key "\M-p" (lambda ()
			 (interactive)
			 (when (eq mark-active nil)
			   (set-mark-command nil))
			 (backward-paragraph)))

;; Try mapping u->backward char and o->forward char
(global-set-key "\M-o" 'forward-char)
(global-set-key "\M-u" 'backward-char)

;; Gobble up all whitespace between "words"
(defun gobble-whitespace ()
  "Delete all whitespace, tabs, and newlines until next non-whitespace character."
  (interactive)
  (while (or (equal (string (char-after)) " ")
	     (equal (string (char-after)) "\t")
	     (equal (string (char-after)) "\n")
	     (equal (string (char-after)) "\r"))
    (delete-char 1)))

(global-set-key (kbd "C-\\") 'gobble-whitespace)

;; Set keys for jump navigation
(global-set-key "\M-[" 'set-jump-point)
(global-set-key "\M-]" 'goto-jump-point)

(global-set-key (kbd "C-(") 'kmacro-start-macro)
(global-set-key (kbd "C-)") 'kmacro-end-or-call-macro)

(autopair-global-mode) ;; enable autopair in all buffers

(global-set-key (kbd "C-q") 'beginning-of-line-text)

(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
(global-set-key (kbd "<escape>") 'god-mode)

(global-set-key (kbd "M-!") 'async-shell-command)
(global-set-key (kbd "M-s M-s") 'shell-toggle)

(define-key god-local-mode-map (kbd "i") 'god-mode)

(defun quick-search (char)
  "Jump to next instance of CHAR."
  (interactive "cChar: \n")
  (forward-char 1)
  (search-forward (string char))
  (backward-char 1))

(defun quick-search-reverse (char)
  "Jump to previous instance of CHAR."
  (interactive "cChar: \n")
  (search-backward (string char)))

(global-set-key (kbd "C-,") 'quick-search-reverse)
(global-set-key (kbd "C-.") 'quick-search)
(define-key god-local-mode-map (kbd "z") (lambda ()
					   (interactive)
					   (run-with-timer 0 nil 'execute-kbd-macro (kbd "RET"))
					   (repeat-complex-command 1)))

(global-set-key (kbd "C-<escape>") 'paredit-mode)
(define-key paredit-mode-map (kbd ",") 'paredit-backward)
(define-key paredit-mode-map (kbd ".") 'paredit-forward)

(add-to-list 'god-exempt-major-modes 'dired-mode)
(add-to-list 'god-exempt-major-modes 'term-mode)
(add-to-list 'god-exempt-major-modes 'shell-mode)

(define-key dired-mode-map (kbd "i") 'dired-subtree-toggle)

(global-set-key (kbd "M-q") 'keyboard-quit)

;; Tags
;; use the command:
;;   find . -name "*.[MY_FILE_ENDINGS]" -print | etags -
;;

(defun generate-tags (file-endings proj-root)
  "Generate tags for all files with FILE-ENDINGS in PROJ-ROOT."
  (interactive "sFile endings: \nsProject root directory: \n")
  (async-shell-command (format "pushd %s && find . -name \"*.[%s]\" -print | etags - && popd"
				 proj-root file-endings)))

(global-set-key (kbd "M-,") 'pop-tag-mark)

(defadvice find-dired-sentinel (after auto-enter-one-file-found)
  (when (eq (count-lines (point-min) (point-max)) 5)
    (autopair-newline)
    (kill-buffer "*Find*")))

(ad-activate 'find-dired-sentinel)

(defun nf (file-name)
  "Find FILE-NAME from the project root."
  (interactive "sFile name: ")
  (let ((starting-dir (cd ".")))
    (while (eq nil (string-match ".*/fw-[a-z]*/?$" (pwd)))
      (cd ".."))
    (find-name-dired "." file-name)
    (cd starting-dir)))

(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

(setq ns-function-modifier 'control)

(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(setq-default c-basic-offset 2)
;(desktop-save-mode)
(global-flycheck-mode t)
(global-company-mode t)
(global-eldoc-mode t)

(setq exec-path (append '("/usr/local/bin") exec-path))

(cscope-setup)

(provide '.emacs)
;;; .emacs ends here.
(put 'downcase-region 'disabled nil)
