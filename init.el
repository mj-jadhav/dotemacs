(require 'package)

;; BASIC SETUP
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/")
	     t)

(package-initialize)

;; Install my default packages
(defvar my-package-list
  '(better-defaults paredit cider company magit bind-key smex ido-ubiquitous smart-mode-line expand-region highlight-symbol git-gutter))

(dolist (p my-package-list)
  (when (not (package-installed-p p))
    (package-install p)))

(setq user-full-name "Mayur Jadhav")
(setq user-mail-address "mj.jadhav13@gmail.com")


;; Clean GUI
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;; custome theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)
;; (set-face-attribute 'region nil :background "#3d3d3d")


;; Set smart-mode-line
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   "   "
   mode-line-position
   (vc-mode vc-mode)
   "  "
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces))

(setq sml/theme 'automatic)
(sml/setup)


;; set default home directory
;; (setq default-directory (f-full (getenv "HOME")))

;; (defun load-local (file)
;;   (load (f-expand file user-emacs-directory)))


;; load current directory
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; If osx then make changes related to OSX
(when (eq system-type 'darwin)
  (require 'osx))


;; Add paredit minor mode to different mjor modes
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(eval-after-load 'clojure-mode
  '(progn (add-hook 'clojure-mode-hook 'enable-paredit-mode)
          (add-hook 'clojure-mode-hook 'company-mode)
          (add-hook 'clojure-mode-hook 'eldoc-mode)))

(eval-after-load 'cider-repl
  '(progn (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
          (add-hook 'cider-repl-mode-hook 'company-mode)
          (setq nrepl-buffer-name-show-port t)
          (setq cider-prefer-local-resources t)
          (setq cider-prompt-for-symbol nil)
          (setq cider-prompt-save-file-on-load nil)
          (setq cider-repl-history-size 1000)
          (setq cider-repl-history-file "~/.emacs.d/cider-history.dat")
          (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)))

(eval-after-load 'paredit
  '(progn (define-key paredit-mode-map (kbd "C-o") 'paredit-open-round)))


;; Add global company hook for text completion
(add-hook 'after-init-hook 'global-company-mode)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(setq ido-case-fold t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-enable-flex-matching t)


;; This extension allows you to tab-complete words in isearch-mode.
(require 'isearch-dabbrev)
(eval-after-load "isearch"
  '(progn
     (require 'isearch-dabbrev)
     (define-key isearch-mode-map (kbd "<tab>") 'isearch-dabbrev-expand)))


;; add magit hook
(global-set-key (kbd "C-x g") 'magit-status)


;; window-numbering-mode
(require 'window-number)
(window-number-meta-mode 1)
(window-number-mode 1)


;; sudo-edit functionality
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;; Add web search capability for google, wikipedia, emacs-wiki
(global-set-key (kbd "C-x C-a") 'webjump)

;; Add Urban Dictionary to webjump
(eval-after-load "webjump"
'(add-to-list 'webjump-sites
              '("Urban Dictionary" .
                [simple-query
                 "www.urbandictionary.com"
                 "http://www.urbandictionary.com/define.php?term="
                 ""])))


;; just press ~ to go home.
(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))


;; Save buffers before quiting emacs
(bind-key
 "C-x C-c"
 (lambda ()
   (interactive)
   (if (y-or-n-p "Quit Emacs? ")
       (save-buffers-kill-emacs))))


;; --------
;; Bindings
;; --------

;; (bind-key "C-a" 'back-to-indentation-or-beginning-of-line)
;; (bind-key "C-7" 'comment-or-uncomment-current-line-or-region)
;; (bind-key "C-6" 'linum-mode)
;; (bind-key "C-v" 'scroll-up-five)
(bind-key "C-j" 'newline-and-indent)

(bind-key "M-g" 'goto-line)
(bind-key "M-n" 'open-line-below)
(bind-key "M-p" 'open-line-above)
(bind-key "M-+" 'text-scale-increase)
(bind-key "M-_" 'text-scale-decrease)
(bind-key "M-j" 'join-line-or-lines-in-region)
(bind-key "M-v" 'scroll-down-five)
(bind-key "M-k" 'kill-this-buffer)
(bind-key "M-o" 'other-window)
;; (bind-key "M-1" 'delete-other-windows)
;; (bind-key "M-2" 'split-window-below)
;; (bind-key "M-3" 'split-window-right)
;; (bind-key "M-0" 'delete-window)
(bind-key "M-}" 'next-buffer)
(bind-key "M-{" 'previous-buffer)
(bind-key "M-`" 'other-frame)
;; (bind-key "M-w" 'kill-region-or-thing-at-point)

(bind-key "C-c g" 'google)
(bind-key "C-c n" 'clean-up-buffer-or-region)
(bind-key "C-c s" 'swap-windows)
(bind-key "C-c C-r" 'rename-buffer-and-file)
(bind-key "C-c C-k" 'delete-buffer-and-file)

(bind-key "C-M-h" 'backward-kill-word)
(bind-key "M-x" 'smex)


;; start emacs-server
(server-start)


;; Show line number temporarily when M-g (goto-line)
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))


;; Override yes-or-no prompt with y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)


;; Disable visual bell, b/c it breaks emacs GUI
(setq visible-bell nil)


;; Save point position between sessions in a file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;; Expand region
(require 'expand-region)
(bind-key "C-:" 'er/expand-region)


;; Highlight symbol
(require 'highlight-symbol)
(highlight-symbol-mode 1)
(setq highlight-symbol-idle-delay 100)
(bind-key "<f5>" 'highlight-symbol-next)


(set-face-attribute 'default nil :height 130)


;; Add git gutter mode
(require 'git-gutter)
(global-git-gutter-mode t)
(bind-key "C-x q" 'git-gutter:revert-hunk)
(bind-key "C-c C-s" 'git-gutter:stage-hunk)
(bind-key "C-x p" 'git-gutter:previous-hunk)
(bind-key "C-x n" 'git-gutter:next-hunk)
