(require 'package)

;; BASIC SETUP
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/")
	     t)

(package-initialize)

;; Install my default packages
(defvar my-package-list
  '(better-defaults paredit cider company magit bind-key))

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
  '(progn (add-hook 'clojure-mode-hook 'enable-paredit-mode)))

(eval-after-load 'cider-repl
  '(progn (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)))

(eval-after-load 'paredit
  '(progn (define-key paredit-mode-map (kbd "M-s") nil)
          (define-key paredit-mode-map (kbd "C-o") 'paredit-open-round)))


;; Add global company hook for text completion
(add-hook 'after-init-hook 'global-company-mode)


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

;; (bind-key "C-c g" 'google)
(bind-key "C-c n" 'clean-up-buffer-or-region)
(bind-key "C-c s" 'swap-windows)
(bind-key "C-c r" 'rename-buffer-and-file)
(bind-key "C-c k" 'delete-buffer-and-file)

(bind-key "C-M-h" 'backward-kill-word)


;; start emacs-server
(server-start)
