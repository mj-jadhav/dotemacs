;; Add global company hook for text completion
(add-hook 'after-init-hook 'global-company-mode)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(setq ido-case-fold t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-enable-flex-matching t)

(provide 'my-company)
