;;; -*- lexical-binding: t; -*-
;;; Code:
(setq package-check-signature nil)

;; temporarily increase garbage collect to 4GB for fast startup
(setq gc-cons-threshold (* 4096 4096 4096)
      gc-cons-percentage 0.6
        max-lisp-eval-depth 4096
        max-specpdl-size 4096)

;; reset garbage collect parameters after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                    gc-cons-percentage 0.1)))

;; initialize package resources
(setq package-archives
      '(("gnu elpa" . "https://elpa.gnu.org/packages/")
        ("melpa"    . "https://melpa.org/packages/")
        ("nongnu"   . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("melpa"    . 7)
        ("gnu elpa" . 5)
        ("nongnu"   . 4))
      ;; Speed up package loading
      package-quickstart t
      package-native-compile t)


;; comment out if you dont want to refresh every time you start emacs
;; helps if you modifying your configuration.
;; (package-refresh-contents t)

;; Ensure the package list have been populated
(require 'package)
;; Skip package refresh and automatic-settings loading on startup for speed
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; use 'use-package' to install packages/dependencies
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; Default to installing every package from the repositories
;; Needed to get the latest org instead of the builtin one
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;; Suppress native comp errors
(setq native-comp-async-report-warnings-errors 'silent)


;; make sure you have correct environment variables when running emacs --daemon
(use-package exec-path-from-shell
  :defer t
  :init
  ;; Only initialize if not already done
  (unless (getenv "PATH")
    (exec-path-from-shell-initialize))
  :config
  (add-to-list 'exec-path (expand-file-name "~/.virtualenvs/emacs-tools/bin")))


;; load configurations from narrative
;; will create <narrative>.el file
;; Load compiled version if available and newer than source
(let* ((org-file (expand-file-name "configuration.org" user-emacs-directory))
       (el-file (expand-file-name "configuration.el" user-emacs-directory))
       (elc-file (expand-file-name "configuration.elc" user-emacs-directory))
       (makefile (expand-file-name "Makefile" user-emacs-directory)))
  (cond
   ;; If compiled file exists and is newer, load it
   ((and (file-exists-p elc-file)
         (file-newer-than-file-p elc-file org-file))
    (load elc-file))
   ;; If .el file exists and is newer than .org, load it
   ((and (file-exists-p el-file)
         (file-newer-than-file-p el-file org-file))
    (load el-file))
   ;; Otherwise, auto-compile if Makefile exists, or tangle manually
   (t
    (if (file-exists-p makefile)
        (progn
          (message "Configuration outdated, running auto-compile...")
          (shell-command "cd ~/.emacs.d && make auto-compile")
          (if (file-exists-p elc-file)
              (load elc-file)
            (load el-file)))
      ;; Fallback to manual tangling
      (use-package org :defer nil)
      (org-babel-load-file org-file)))))

;;; init.el ends here
