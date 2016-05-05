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

;; set font size 13
(set-face-attribute 'default nil :height 130)

(provide 'ui)

;;; ui.el ends here.
