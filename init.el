(require 'package)


;; -----------
;; BASIC SETUP
;; -----------
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

;; load current directory
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; If osx then make changes related to OSX
(when (eq system-type 'darwin)
  (require 'osx))

;; Disable visual bell, b/c it breaks emacs GUI
(setq visible-bell nil)

;; Override yes-or-no prompt with y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; window-numbering-mode
(require 'window-number)
(window-number-meta-mode 1)
(window-number-mode 1)

;; Save buffers before quiting emacs
(bind-key
 "C-x C-c"
 (lambda ()
   (interactive)
   (if (y-or-n-p "Quit Emacs? ")
       (save-buffers-kill-emacs))))


;; --------------------
;; LOAD CUSTOM PACKAGES
;; --------------------
(require 'ui)
(require 'init-parens)
(require 'init-clojure)
(require 'init-cider)
(require 'my-company)
(require 'isearch-dabbrev)
(require 'oh-my-git)
(require 'defuns)


;; ---------------
;; Global Bindings
;; ---------------

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

;; Show line number temporarily when M-g (goto-line)
(global-set-key [remap goto-line] 'goto-line-with-feedback)


;; ------------
;; KEY BINDINGS
;; ------------

(bind-key "C-j" 'newline-and-indent)
(bind-key "M-n" 'open-line-below)
(bind-key "M-p" 'open-line-above)
(bind-key "M-+" 'text-scale-increase)
(bind-key "M-_" 'text-scale-decrease)
(bind-key "M-j" 'join-line-or-lines-in-region)
(bind-key "M-v" 'scroll-down-five)
(bind-key "M-k" 'kill-this-buffer)
(bind-key "M-o" 'other-window)
(bind-key "M-}" 'next-buffer)
(bind-key "M-{" 'previous-buffer)
(bind-key "M-`" 'other-frame)
(bind-key "C-c g" 'google)
(bind-key "C-c n" 'clean-up-buffer-or-region)
(bind-key "C-c s" 'swap-windows)
(bind-key "C-c C-r" 'rename-buffer-and-file)
(bind-key "C-c C-k" 'delete-buffer-and-file)
(bind-key "C-M-h" 'backward-kill-word)
(bind-key "M-x" 'smex)

;; List Keys which are not configured
;; (bind-key "M-1" 'delete-other-windows)
;; (bind-key "M-2" 'split-window-below)
;; (bind-key "M-3" 'split-window-right)
;; (bind-key "M-0" 'delete-window)
;; (bind-key "C-a" 'back-to-indentation-or-beginning-of-line)
;; (bind-key "C-7" 'comment-or-uncomment-current-line-or-region)
;; (bind-key "C-6" 'linum-mode)
;; (bind-key "C-v" 'scroll-up-five)
;; (bind-key "M-w" 'kill-region-or-thing-at-point)


;; Expand region
(require 'expand-region)
(bind-key "C-:" 'er/expand-region)


;; Highlight symbol
(require 'highlight-symbol)
(highlight-symbol-mode 1)
(setq highlight-symbol-idle-delay 100)
(bind-key "<f5>" 'highlight-symbol-next)


;; Save point position between sessions in a file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


;; just press ~ to go home.
(add-hook 'ido-setup-hook 'go-to-home)


;; start emacs-server
(server-start)
