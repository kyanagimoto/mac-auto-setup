(require 'cask "/usr/local/opt/cask/cask.el")
(cask-initialize)

;; melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(require 'server)
(unless (server-running-p)
  (server-start))
(setq ns-pop-up-frames nil)

;; Spaceline
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; Undo-tree
(global-undo-tree-mode)

(global-linum-mode t)
(setq ruby-insert-encoding-magic-comment nil)

(if window-system (progn
    (set-frame-parameter nil 'alpha 90) 
    ))

(require 'all-the-icons)

;; ivy
(ivy-mode 1) 
(counsel-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-height 30) ;; minibufferのサイズを拡大！（重要）
(setq ivy-extra-directories nil)
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
(require 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
'(ivy-switch-buffer
  (:columns
   ((ivy-rich-candidate (:width 30))  ; return the candidate itself
    (ivy-rich-switch-buffer-size (:width 7))  ; return the buffer size
    (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
    (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
   :predicate
   (lambda (cand) (get-buffer cand)))
  counsel-M-x
  (:columns
   ((counsel-M-x-transformer (:width 40))  ; thr original transformer
    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
  counsel-describe-function
  (:columns
   ((counsel-describe-function-transformer (:width 40))  ; the original transformer
    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
  counsel-describe-variable
  (:columns
   ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
    (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
  counsel-recentf
  (:columns
   ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
    (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))) ; return the last modified time of the file

(defun ivy-rich-switch-buffer-icon (candidate)
     (with-current-buffer
   	  (get-buffer candidate)
	(let ((icon (all-the-icons-icon-for-mode major-mode)))
	  (if (symbolp icon)
	      (all-the-icons-icon-for-mode 'fundamental-mode)
	    icon))))
(setq ivy-rich--display-transformers-list
      '(ivy-switch-buffer
        (:columns
         ((ivy-rich-switch-buffer-icon (:width 2))
          (ivy-rich-candidate (:width 30))
          (ivy-rich-switch-buffer-size (:width 7))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project (:width 15 :face success))
          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))))

;; counsel
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file) 
(setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))

(setq-default indent-tabs-mode nil)

(require 'whitespace)
(set-face-foreground 'whitespace-space "DarkGoldenrod1")
(set-face-background 'whitespace-space nil)
(set-face-bold-p 'whitespace-space t)
(set-face-foreground 'whitespace-tab "DarkOliveGreen1")
(set-face-background 'whitespace-tab nil)
(set-face-underline  'whitespace-tab t)
(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(global-whitespace-mode 1) ;; Show full-width space.

(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(add-hook 'js-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(global-yascroll-bar-mode 1)

;; Dockerfile-mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; swiper
(global-set-key "\C-s" 'swiper)
(setq swiper-include-line-number-in-search t) ;; line-numberでも検索可能

;; company
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 3) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
(define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設定
(define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
(define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fで候補を設定
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う

;; yasnippetとの連携
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; find-file-in-project
(require 'find-file-in-project)
(global-set-key [(super shift i)] 'find-file-in-project)

;; recentf
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 200)             ;; recentf に保存するファイルの数
(setq recentf-exclude '(".recentf"))           ;; .recentf自体は含まない
(setq recentf-auto-cleanup 'never)             ;; 保存する内容を整理
(run-with-idle-timer 30 t '(lambda () (with-suppressed-message (recentf-save-list))))
(require 'recentf-ext) ;; ちょっとした拡張

(define-key global-map [(super r)] 'counsel-recentf) ;; counselにおまかせ！

;; plantuml
(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
(setq plantuml-options "-charset UTF-8")
(setq plantuml-jar-path "/Users/koichi.yanagimoto/plantuml.jar")

;; doom
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  ;; (vertical-bar   (doom-darken base5 0.4))
  ;; (doom-darken bg 0.4)
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  ;; Modeline
  (use-package doom-modeline
    :custom
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-icon t)
    (doom-modeline-major-mode-icon nil)
    (doom-modeline-minor-modes nil)
    :hook
    (after-init . doom-modeline-mode)
    :config
    (set-cursor-color "cyan")
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline 'main
	      '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))))

;; hide-mode-line
(use-package hide-mode-line
    :hook
    ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

;; dashboard
(use-package dashboard
    :diminish
    (dashboard-mode page-break-lines-mode)
    :custom
    (dashboard-startup-banner 4)
    (dashboard-items '((recents . 15)
               (projects . 5)
               (bookmarks . 5)
               (agenda . 5)))
    :hook
    (after-init . dashboard-setup-startup-hook)
    :config
    (add-to-list 'dashboard-items '(agenda) t))

;; nyan-cat
(use-package nyan-mode
   :custom
   (nyan-cat-face-number 4)
   (nyan-animate-nyancat t)
   :hook
   (doom-modeline-mode . nyan-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(package-selected-packages
   (quote
    (org-plus-contrib org volatile-highlights flymake-yaml beacon k8s-mode dumb-jump yasnippet yascroll yaml-mode web-mode use-package undo-tree swiper-helm spaceline-all-the-icons smex smartparens ruby-tools ruby-test-mode ruby-interpolation ruby-end ruby-compilation recentf-ext rainbow-delimiters projectile prodigy popwin pallet osx-org-clock-menubar org-pomodoro nyan-mode multiple-cursors maude-mode markdown-mode magit idle-highlight-mode htmlize hide-mode-line helm-themes helm-ls-git helm-gtags helm-git-grep flycheck-plantuml flycheck-cask find-file-in-project expand-region exec-path-from-shell erlang drag-stuff doom-themes doom-modeline doom dockerfile-mode dashboard-hackernews counsel-codesearch counsel-bbdb company-terraform company-anaconda color-theme-solarized auto-complete all-the-icons-ivy all-the-icons-gnus))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-pomodoro-mode-line ((t (:foreground "#ff5555"))))
 '(org-pomodoro-mode-line-break ((t (:foreground "#50fa7b"))))
 '(vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

;; dumb-jump, M-x dumb-jump-go
(setq dumb-jump-mode t)

;; For Rails Project
;; When ruby-mode, rinari-minor-mode activated
(require 'rinari)
(add-hook 'ruby-mode-hook 'rinari-minor-mode)

;; snippets for rspec-mode
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;; k8s mode
(use-package k8s-mode
  :ensure t
  :hook (k8s-mode . yas-minor-mode))

;; beacon
(use-package beacon
  :custom
  (beacon-color "yellow")
  (beadon-blink-when-point-moves)
  :config
  (beacon-mode 1))

(require 'flymake-yaml)
  (add-hook 'yaml-mode-hook 'flymake-yaml-load)
  (add-hook 'yaml-mode-hook
    '(lambda ()
      (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(use-package highlight-indent-guides
    :diminish
    :hook
    ((prog-mode yaml-mode) . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-auto-enabled t)
    (highlight-indent-guides-responsive t)
    (highlight-indent-guides-method 'column)) 

(use-package volatile-highlights
    :diminish
    :hook
    (after-init . volatile-highlights-mode)
    :custom-face
    (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

(use-package imenu-list
    :bind
    ("<f10>" . imenu-list-smart-toggle)
    :custom-face
    (imenu-list-entry-face-1 ((t (:foreground "white"))))
    :custom
    (imenu-list-focus-after-activation t)
    (imenu-list-auto-resize nil))
