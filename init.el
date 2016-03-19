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
(require 'package)
(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

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

(load-theme 'idea-darkula t)
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
