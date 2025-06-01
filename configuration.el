;; idle garbage collecting
(run-with-idle-timer 30 t (lambda () (garbage-collect)))

;; keep .emacs.d clean
(use-package no-littering)

;; keep customization settings in a temporary file
(setq custom-file "~/.emacs.d/automatic-settings.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; suppress noise on startup
(setq-default inhibit-startup-screen t
              inhibit-splash-screen t
              initial-scratch-message ";;Hope you have a good day!
")

;; remove unwanted elements
(menu-bar-mode 0)   ; disable the menu bar at the top
(when (boundp 'x-toolkit-scroll-bars)
  (scroll-bar-mode 0))
 ; disable visible scrollbar
(scroll-all-mode 0) ; disable synchronized scrolling of buffers
(tool-bar-mode 0)   ; disable the toolbar
(tooltip-mode 0)    ; disable the tooltip

;; set indentation style for CC mode
(require 'cc-styles)

(setq c-default-style "linux"
      c-basic-offset 4
      tab-width 4)

;; set indentation for enriched text
(setq-default standard-indent 4)

;; use tab for auto-complete selection
;;(setq-default tab-always-indent 'complete)

;; prevent extraneous tabs -- affects TeX
(setq-default indent-tabs-mode nil)

;; package is minor mode to visualize white spaces
(require 'whitespace)
(setq-default indicate-empty-lines t)
(setq whitespace-line-column 79
      whitespace-style '(face trailing lines-tail empty
                              indentation::space space-before-tab::tab))
(global-whitespace-mode 0)

;; cleanup  whitespaces on save
(defcustom do-whitespace-cleanup t
  "Perform 'whitespace-cleanup' on save."
  :group 'whitespace)

(make-variable-buffer-local 'do-whitespace-cleanup)

(defun toggle-whitespace-cleanup ()
  "Turn the 'whitespace-cleanup' hook on and off."
  (interactive)
  (setq do-whitespace-cleanup (not do-whitespace-cleanup))
  (message "do-whitespace-cleanup set to %s" do-whitespace-cleanup))

(add-hook 'before-save-hook
          (lambda ()
            (when do-whitespace-cleanup
              (whitespace-cleanup))))

;; dont cleanup whitespace but do delete trailing whitespace
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t
                  do-whitespace-cleanup nil)
            (add-hook 'before-save-hook #'delete-trailing-whitespace)))

(add-hook 'prog-mode-hook (lambda () (whitespace-mode +1)))

;; add visual bell (removes audible dinging)
(setq visible-bell t)

;; flash location of cursor when moved
(use-package beacon
  :config (beacon-mode 1))

;; paired parentheses
(electric-pair-mode 1)

;; highlight matching parentheses
(show-paren-mode 1)
(setq-default show-paren-style 'parenthesis
              show-paren-when-point-in-periphery t)

;; download your desired themes here
(use-package solarized-theme :defer t)

;; use to make sure themes are switched correctly
(defun disable-all-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  "Disable current theme(s) before loading a new one."
  (disable-all-themes))

(defun light ()
  "Turn on light theme."
  (interactive)
  (load-theme 'dichromacy t))

;; make a little darker for higher contrast and darklight org src blocks
(defun dark ()
  "Turn on dark theme."
  (interactive)
  (load-theme 'solarized-zenburn t))

(light)

(defun fonts (fontsize)
  "Set FONTSIZE."
  (interactive "nFont size: ")
  ;; font for unicode characters
  (let ((font "Symbola")
        (size fontsize))
    (when (member font (font-family-list))
      (let ((fontspec (format "%s %d" font size)))
        (set-fontset-font t 'unicode fontspec nil 'prepend)
        (message fontspec))))

  ;; general editor font
  (let ((font "Noto Sans Mono")
        (size fontsize))
    (when (member font (font-family-list))
      (let ((fontspec (format "%s %d" font size)))
        (set-frame-font fontspec nil t)
        (add-to-list 'default-frame-alist `(font . ,fontspec))
        ;; (set-face-attribute 'default (selected-frame) :height 90)
        ;; (set-face-attribute 'default nil :height 100)
        (message fontspec)))))
(fonts 10)

;; show column number in modeline
(column-number-mode t)

;; uncomment to turn on line numbers everywhere
;; (global-display-line-numbers-mode t)

;; display line numbers only in prog-mode by default
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; scale line numbers with text size
(defun linum-scale (arg)
  (interactive "nScale: ")
  (set-face-attribute 'line-number nil :inherit nil :height arg))

;; highlight occurences of the same thing in buffer
(use-package highlight-thing
  :demand t
  :hook ((prog-mode . highlight-thing-mode)
         (org-mode . highlight-thing-mode))
  :config
  (setq highlight-thing-exclude-thing-under-point t
        highlight-thing-case-sensitive-p t
        highlight-thing-ignore-list
        '("False" "True", "return", "None", "if", "else", "self",
          "import", "from", "in", "def", "class")))

;; comment out to remove highlighting on line containing point
(global-hl-line-mode)

;; change frame title to buffer name
(setq frame-title-format
      '("emacs: " (:eval (if (buffer-file-name)
                             (abbreviate-file-name (buffer-file-name)) "%b"))))

;; enable line wrapping (otherwise will continue outside buffer)
(global-visual-line-mode 1)

;; lazy prompting
(fset 'yes-or-no-p 'y-or-n-p)

;; calendar settings
(setq-default
 european-calendar-style t
 calendar-date-style 'european
 calendar-week-start-day 1

 display-time-24hr-format t
 display-time-day-and-date t
 display-time-string-forms
 '((if (and (not display-time-format) display-time-day-and-date)
       (format-time-string "%a %b %e " now) "")
   (format-time-string (or display-time-format
                           (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
                       now))
 calendar-time-display-form
 '(24-hours ":" minutes
            (if time-zone " (") time-zone (if time-zone ")"))
 )
(setq calendar-latitude 52.36547
      calendar-longitude 4.81926
      calendar-location-name "Amsterdam"
      calendar-time-zone 60)

;; misc. buffer settings
(winner-mode 1)   ;; undo/redo window configurations

(setq-default
 ;; debug setting
 debug-on-error nil

 ;; structure settings
 fill-column 100
 message-log-max 2000

 sentence-end-double-space nil
 indicate-empty-lines t

 ;; backup settings
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 vc-make-backup-files t
 backup-directory-alist '((".*" . "~/.emacs.d/emacs-backups"))

 ;; ediff settings
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain

 ;; ibuffer settings
 ibuffer-use-other-window t
 ibuffer-formats
 '((mark modified read-only locked
         " " (name 36 36 :left :elide)
         " " (size 9 -1 :right)
         " " (mode 16 16 :left :elide) " " filename-and-process)
   (mark " " (name 16 -1) " " filename))

 async-shell-command-buffer 'new-buffer
 display-buffer-alist '(("*Async Shell Command*" . (display-buffer-no-window)))
 ibuffer-maybe-show-predicates
 `(,(lambda (buf)
      (or (and (string-match "^ " (buffer-name buf))
               (null buffer-file-name))
          (string-match ".*Async Shell Command.*" (buffer-name buf)))))

 ;; auto-complete settings
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 )

;; prevent storage of sudo passwords in plaintext
(setq-default
 auth-sources '("~/.emacs.0/.authinfo.gpg")
 auth-source-save-behavior nil
 )

;; man page settings
(setq-default
 Man-width 100
 Man-notify-method 'pushy
 )

;; alternative to built-in Emacs help
(use-package helpful
  :bind (("C-h j" . helpful-at-point)
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)))

;; paste at point, not location of mouse cursor
(setq-default mouse-yank-at-point t)

;; connect to server
(defun connect-server (server env)
  "Connect (via ssh/TRAMP) to SERVER and use the ENV python virtualenv.
This sets your default directory to live on SERVER. Call `M-x local` to move
back to the local machine. Note: if you don't use ssh host abbreviations, you
can also use ie. user@sevap.ru as SERVER."
  (interactive "sServer:
sVirtualenv (name of directory inside ~/.virtualenvs):")
  (setq default-directory (concat "/ssh:" server ":")
        shell-file-name "/bin/bash"
        python-shell-virtualenv-root (concat "~/.virtualenvs/" env)))

;; create default directory
(defun local ()
  "Set default directory back to local, remove (potentially remote) virtualenv."
  (interactive)
  (setq default-directory "~/"
        python-shell-virtualenv-root nil))

;; variable contains list of servers used
(defvar personal-hosts '("tete", "titty"))

;; tramp
(eval-after-load "tramp"
  `(setq tramp-default-method "ssh"
         password-cache-expiry nil
         remote-file-name-inhibit-cache nil
         tramp-completion-reread-directory-timeout nil
         tramp-use-ssh-controlmaster-options nil
         tramp-ssh-controlmaster-options
         (concat "-o ControlPath=~/.ssh/control/%%r@%%h:%%p "
                 "-o ControlMaster=auto "
                 "-o ControlPersist=yes ")))

(require 'tramp)

;; set language environment
(set-language-environment "UTF-8")
(setq uniquify-buffer-name-style 'forward
      locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; auto-revert reloaded images
(auto-image-file-mode 1)
(add-hook 'image-mode-hook 'auto-revert-mode)

;; allows for use of emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

(setf completion-styles '(basic flex)
            completion-auto-select t ;; Show completion on first call
            completion-auto-help 'visible ;; Display *Completions* upon first request
            completions-sort 'historical ;; Order based on minibuffer history
            completions-max-height 20 ;; Limit completions to 15 (completions start at line 5)
            completion-ignore-case t)

(completion-preview-mode)

;; minibuffer completions
(savehist-mode 1) ;; save minibuffer history for vertico

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  ;; either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; the :init configuration is always executed (Not lazy!)
  :init
  ;; must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package orderless
  :init
  ;; configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package jinx
  :hook (org-mode text-mode prog-mode conf-mode)
  :bind (("C-c j c" . jinx-correct)
         ("C-c j a" . jinx-correct-all)
         ;; alias defined using 'jinx-correct' keybinding
         ("C-c j d" . jinx-save-word-at-point))
  :custom
  ;; 'jinx-mode' only checks text possessing specific face properties like
  ;; 'font-lock-comment-face' in 'prog-mode' for example.
  (jinx-include-faces
   '((yaml-mode . conf-mode)
     (yaml-ts-mode . conf-mode)
     ;; Only check docstrings and comments; not strings
     (conf-mode font-lock-comment-face)
     (prog-mode font-lock-comment-face
                font-lock-doc-face
                tree-sitter-hl-face:comment
                tree-sitter-hl-face:doc)))

  (jinx-languages "en_GB" "ru_RU")
  :config
  ;; Quickly save word-at-point to dictionary used by 'jinx'
  (defalias 'jinx-save-word-at-point (kmacro "C-c j c @ RET"))

  ;; 'jinx-correct' suggestions displayed as grid instead of long list
  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20))))

(use-package magit
  :bind ("C-x g" . magit-status)
  :diminish magit-minor-mode
  :hook ((git-commit-mode . (lambda () (setq fill-column 72)))
         (after-save . magit-after-save-refresh-status))
  :mode ("/\\.gitmodules\\'" . conf-mode)
  :custom
  (magit-diff-hide-trailing-cr-characters t)
  (git-commit-summary-max-length 50))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
