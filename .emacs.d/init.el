;; global variables
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; modes
(electric-indent-mode 0)

;; global keybindings
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "s-p"))

(delete-selection-mode 1)

;; the package manager

; list the packages you want
(setq package-list
      '(ace-window avy ag s dash avy cyberpunk-theme ensime popup s dash
                   company yasnippet sbt-mode scala-mode2
                   scala-mode2 etags-select find-file-in-repository
                   goto-chg highlight-symbol idea-darkula-theme
                   magit-find-file dash magit magit-popup dash
                   async git-commit with-editor dash async
                   dash with-editor dash async dash async
                   magit-popup dash async monokai-theme
                   play-routes-mode popup-imenu yatemplate
                   flx-ido flx popup dash projectile pkg-info
                   epl dash python-mode s sbt-mode scala-mode2
                   scala-mode2 slack emojify ht seq alert
                   log4e gntp circe oauth2 request websocket
                   smartparens dash  use-package diminish bind-key websocket
                   with-editor dash async yaml-mode yasnippet color-theme-sanityinc-tomorrow))

; list the repositories containing them
(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; custom options

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(define-key input-decode-map "\e[1;2D" [S-left])  
(define-key input-decode-map "\e[1;2C" [S-right])  
(define-key input-decode-map "\e[1;2B" [S-down])  
(define-key input-decode-map "\e[1;2A" [S-up])  
(define-key input-decode-map "\e[1;2F" [S-end])  
(define-key input-decode-map "\e[1;2H" [S-home])

(global-set-key (kbd "s-<up>") 'scroll-down-command)
(global-set-key (kbd "s-<down>") 'scroll-up-command)
(global-set-key (kbd "s-<left>") 'beginning-of-line)
(global-set-key (kbd "s-<right>") 'end-of-line)

(global-set-key (kbd "s-w") 'mac-key-close-window)
(global-set-key (kbd "s-l") 'goto-line)

;; use Shift+arrow_keys to move cursor around split panes
;;(windmove-default-keybindings)

;; when cursor is on edge, move to the other side, as in a torus space
(setq windmove-wrap-around t )

(global-linum-mode t)
(setq-default truncate-lines 1)

(scroll-bar-mode 0)
(fset `yes-or-no-p `y-or-n-p)

;;(load-theme 'idea-darkula t)
(set-default-font "Menlo 18")
(tool-bar-mode 0)

;;(setq mac-command-modifier 'control)

(global-set-key (kbd "<S-tab>") 'un-indent-by-removing-2-spaces)
(defun un-indent-by-removing-2-spaces ()
  "remove 2 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^  ")
        (replace-match "")))))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;(global-set-key [wheel-right] 'scroll-left)
;;(global-set-key [wheel-left] 'scroll-right)

(global-set-key (kbd "s-=") 'toggle-frame-fullscreen)

(global-set-key (kbd "s-p") 'ace-window)
(setq aw-dispatch-always t)

(global-set-key (kbd "s-\\") 'ensime-search)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "s-/") 'other-frame)
;; yaml

(require 'yaml-mode)
   (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;(when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize))

(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


(put 'scroll-left 'disabled nil)

(require 'magit-find-file)
(global-set-key (kbd "C-c p") 'magit-find-file-completing-read)

(global-set-key (kbd "C-x f") 'find-file-in-repository)

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq scala-indent:use-javadoc-style t)

;;(defun ensime-setup ()
;;  "Open sbt project and setup ensime."
;;  (interactive)
;;  (sbt-command "gen-ensime")
;;  (ensime "./.ensime"))

(defun test-only ()
  "Run test with current file."
  (interactive)
  (sbt-command (concat "testOnly " (find-spec-name))))

(defun find-spec-name ()
  "Find spec name of current buffer."
  (concat "*." (file-name-sans-extension (file-name-nondirectory (buffer-name)))))

(defun compile-sbt-project ()
  "Compile the sbt project."
  (sbt-command "test:compile")
  )

;;(add-hook 'scala-mode-hook
;;          (lambda ()
;;            (add-hook 'after-save-hook 'compile-sbt-project)))

(provide 'prelude-scala-sbt)
;;; prelude-scala-sbt.el ends here

(use-package projectile
  :demand
  ;; nice to have it on the modeline
  :init
  (setq projectile-use-git-grep t)
  :config
  (projectile-global-mode)
  (add-hook 'projectile-grep-finished-hook
            ;; not going to the first hit?
            (lambda () (pop-to-buffer next-error-last-buffer)))
  :bind
  (("s-f" . projectile-find-file)
   ("s-F" . projectile-grep)))

(use-package smartparens
  :diminish smartparens-mode
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)

  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")

  ;; WORKAROUND https://github.com/Fuco1/smartparens/issues/543
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))


(sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))


(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hoo  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

;;(bind-key "RET" 'scala-mode-newline-comments scala-mode-map)

(setq comment-start "/* "
	  comment-end " */"
	  comment-style 'multi-line
	  comment-empty-lines t)

(global-set-key (kbd "s-/") 'comment-region)
(global-set-key (kbd "s-?") 'uncomment-region)


(use-package etags-select
  :commands etags-select-find-tag)

(defun ensime-edit-definition-with-fallback ()
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive)
  (unless (and (ensime-connection-or-nil)
               (ensime-edit-definition))
    (projectile-find-tag)))

(bind-key "M-." 'ensime-edit-definition-with-fallback ensime-mode-map)

(global-set-key (kbd "M-.") 'projectile-find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)

(use-package yatemplate
  :defer 2 ;; WORKAROUND https://github.com/mineo/yatemplate/issues/3
  :config
  (auto-insert-mode)
  (setq auto-insert-alist nil)
  (yatemplate-fill-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#003f8e")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff9da4")
     (40 . "#ffc58f")
     (60 . "#ffeead")
     (80 . "#d1f1a9")
     (100 . "#99ffff")
     (120 . "#bbdaff")
     (140 . "#ebbbff")
     (160 . "#ff9da4")
     (180 . "#ffc58f")
     (200 . "#ffeead")
     (220 . "#d1f1a9")
     (240 . "#99ffff")
     (260 . "#bbdaff")
     (280 . "#ebbbff")
     (300 . "#ff9da4")
     (320 . "#ffc58f")
     (340 . "#ffeead")
     (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

