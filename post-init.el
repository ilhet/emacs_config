;; (use-package org :load-path "~/.emacs.d/elpa/org-mode/lisp")
;; disable startup message
(add-to-list 'default-frame-alist
             '(font . "Noto Sans Mono CJK SC-11"))
(set-face-attribute 'default nil :font "Noto Sans Mono CJK SC-11")

(setq inhibit-startup-message t)
;; (scroll-bar-mode -1)    ;disable scroll bar
;; (tool-bar-mode -1)      ;disable tool bar
;; (tooltip-mode -1)       ;disable tooltip
;; (menu-bar-mode 1)      ;disable menu bar
(which-key-mode 1)
(load-theme 'leuven)     ;set theme

(transient-mark-mode 1)
(delete-selection-mode 1)

(set-language-environment "utf-8")

;; initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; list of used packages
(use-package auctex)
(use-package xenops
  :hook (LaTeX-mode . xenops-mode)
  :hook (org-mode . xenops-mode)
  :config
  (setq xenops-math-image-scale-factor 0.45)
  (setq xenops-reveal-on-entry t)
  )
;; (use-package org-fragtog) 
(use-package gdscript-mode)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(gdscript-mode . ("127.0.0.1" 6005))
  ))
(use-package corfu
  :init
  (global-corfu-mode)
  )
;; (use-package dap-mode)
(use-package org-roam)
;; CDLatex settings
(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :hook (org-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map 
              ("<tab>" . cdlatex-tab)))
(defun my/super-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'cape-dabbrev #'completion-at-point)))
  )

(use-package cape
  :hook (corfu-mode . my/super-capf)
  )

(use-package exec-path-from-shell)
(when (daemonp)
  (exec-path-from-shell-initialize))

(use-package eglot
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'gdscript-mode 'eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  )

(use-package haskell-mode)
(use-package rust-mode)
;; Yasnippet settings
(use-package yasnippet
  :ensure t
  :hook ((LaTeX-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
  (use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))

  (setq yas-triggers-in-field t)
  
  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))

;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas
;; fields
(use-package cdlatex
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
  :config
  (use-package yasnippet
    :bind (:map yas-keymap
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB" . yas-next-field-or-cdlatex))
    :config
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))
(use-package math-symbol-lists)
(use-package aas)
(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :hook (org-mode . laas-mode)
  :config
  (aas-set-snippets 'laas-mode
		    ;;set condition
		    :cond #'texmathp ;expand while in math
		    ))
		  
(use-package magit)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;;auctex customization
(setq org-latex-create-formula-image-program 'dvisvgm)
(plist-put org-format-latex-options :foreground nil)
(plist-put org-format-latex-options :background nil)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;;org-roam customization
(setq org-roam-directory (file-truename "~/org"))
(org-roam-db-autosync-mode)

;;org-mode customization
(setq org-agenda-files '("~/org"))

;;custom keybinds
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(keymap-global-set "M-z" 'zap-up-to-char)

;;modularize config
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(if (equal system-type 'windows-nt)
    (load-user-file "windows.el"))
(if (equal system-type 'windows-nt)
  (load-user-file "scalingpreviews.el")
  )


(load-user-file "laas.el")


;; (package-vc-install '(org-mode :url "https://code.tecosaur.net/tec/org-mode" :branch "dev"))

(electric-pair-mode 1)

;;mode hooks
(add-hook 'text-mode-hook 'auto-fill-mode)
;; (add-hook 'org-mode-hook 'org-fragtog-mode)
(put 'downcase-region 'disabled nil)

(add-hook 'dired-mode-hook 'auto-revert-mode)
