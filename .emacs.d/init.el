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

(global-unset-key (kbd "s-n"))

(delete-selection-mode 1)

;; the package manager

; list the packages you want
(setq package-list
      '(ace-window avy ag s dash avy cyberpunk-theme popup s dash
                   company yasnippet sbt-mode undo-tree
                   find-file-in-repository
                   goto-chg highlight-symbol idea-darkula-theme
                   magit-find-file dash magit magit-popup dash
                   async git-commit with-editor dash async
                   dash with-editor dash async dash async
                   magit-popup dash async monokai-theme
                   play-routes-mode popup-imenu yatemplate
                   flx-ido flx popup dash projectile pkg-info
                   epl dash python-mode s sbt-mode
                   scala-mode slack emojify ht seq alert which-key
                   log4e gntp circe oauth2 request websocket multi-web-mode
                   smartparens dash  use-package diminish bind-key websocket
                   with-editor dash async yaml-mode yasnippet ujelly-theme org avy
                   color-theme-sanityinc-tomorrow helm helm-ag spacemacs-theme spaceline))

; list the repositories containing them
(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
					("melpa-stable" . "http://stable.melpa.org/packages/")
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

(show-paren-mode)

;; tree-undo

(global-undo-tree-mode)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "s-z") 'undo-tree-undo)

;; which-key

(which-key-mode)

;; avy

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1)))

;; use Shift+arrow_keys to move cursor around split panes
;;(windmove-default-keybindings)

;; when cursor is on edge, move to the other side, as in a torus space
(setq windmove-wrap-around t )

(global-linum-mode t)
(setq-default truncate-lines 1)

(scroll-bar-mode 0)
(fset `yes-or-no-p `y-or-n-p)

;;(load-theme 'monokai t)

(require 'spaceline-config)
(spaceline-emacs-theme)
(spaceline-helm-mode)

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
(global-set-key (kbd "s-n") 'ensime-search)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "s-/") 'other-frame)

(global-set-key (kbd "<C-s-right>") 'next-multiframe-window)
(global-set-key (kbd "<C-s-left>") 'previous-multiframe-window)
                
;; multi-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags 
  '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
    (js-mode  "<script[^>]*>" "</script>")
    (css-mode "<style[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)
(setq helm-mode-fuzzy-match t)
(helm-autoresize-mode 1)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setq exec-path (append exec-path '("/usr/local/bin/")))

;; kill others

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; last change

(global-set-key (kbd "<s-backspace>") 'goto-last-change)

;; ag

(setq ag-executable "/usr/local/bin/ag")

;; yaml

(require 'yaml-mode)
   (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; javascript

(setq js-indent-level 2)

;;(when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize))

(setq exec-path (append exec-path '("/usr/local/bin")))

;; org

(setq org-table-convert-region-max-lines 9999)

(use-package ensime
  :ensure t
  :pin melpa)

;;setq ensime-server-version "2.0.0-SNAPSHOT")

(require 'ensime)
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'java-mode-hook 'ensime-scala-mode-hook)

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

;(use-package etags-select
;  :commands etags-select-find-tag)

;;(defun ensime-edit-definition-with-fallback ()
;;  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
;;  (interactive)
;;  (unless (and (ensime-connection-or-nil)
;;               (ensime-edit-definition))
;;    (projectile-find-tag)))

;;(bind-key "M-." 'ensime-edit-definition ensime-mode-map)

;;(bind-key (kbd "<s-mouse-1>")  'ensime-edit-definition ensime-mode-map)

;;(global-set-key (kbd "M-.") 'projectile-find-tag)
;;(global-set-key (kbd "M-,") 'pop-tag-mark)

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
 '(custom-safe-themes
   (quote
    ("f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "235dc2dd925f492667232ead701c450d5c6fce978d5676e54ef9ca6dd37f6ceb" "99473228af8c280ed5534952a1a687732c2450d076528c6363ec23febccccd7b" "99953b61ecd4c3e414a177934e888ce9ee12782bbaf2125ec2385d5fd732cbc2" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "6c62b1cd715d26eb5aa53843ed9a54fc2b0d7c5e0f5118d4efafa13d7715c56e" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" default)))
 '(package-selected-packages
   (quote
    (ensime spaceline spacemacs-theme helm-ag helm color-theme-sanityinc-tomorrow ujelly-theme yaml-mode use-package smartparens multi-web-mode which-key slack scala-mode python-mode projectile yatemplate popup-imenu play-routes-mode monokai-theme magit-find-file idea-darkula-theme highlight-symbol goto-chg find-file-in-repository etags-select undo-tree sbt-mode yasnippet company popup cyberpunk-theme ag ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "~/.emacs.d/ujelly-theme-source-code.el")
(load-theme 'ujelly t)
