(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(company-backends (quote (company-elisp company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake company-capf company-anaconda (company-dabbrev-code company-gtags company-etags company-keywords) company-oddmuse company-files company-dabbrev)))
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
 '(company-preview ((t (:background "dim gray" :foreground "white"))))
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

;; c preferences
(setq c-default-style "k&r")
(setq-default c-basic-offset 4)

;;bracket autocomplete

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(global-linum-mode t)
(tool-bar-mode -1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Add .pixie to css mode
(add-to-list 'auto-mode-alist '("\\.pixie\\'" . css-mode))


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

;;Map C-c C-e to exec-file
(global-set-key (kbd "C-c C-e") 'exec-file)

;; map C-. to close-tag
(add-hook 'html-mode-hook (lambda () (local-set-key [67108910] (quote sgml-close-tag))))



;; M-l is already mapped to forward-word, map M-j to backward word
;; Alternate Navigation keys. Most of these weren't mapped to anything
;; Can use either i-j-k-l as a keypad, or use M-p and M-n as training wheels
(global-set-key "\M-j" 'backward-word)
(global-set-key "\M-l" 'forward-word)
(global-set-key "\M-i" 'previous-line)
(global-set-key "\M-p" 'previous-line)
(global-set-key "\M-k" 'next-line)
(global-set-key "\M-n" 'next-line)
;; Try mapping u->backward char and o->forward char
(global-set-key "\M-o" 'forward-char)
(global-set-key "\M-u" 'backward-char)
