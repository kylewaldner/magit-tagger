;;; install.el --- Installation script for magit-tagger -*- lexical-binding: t; -*-

;; This script helps install and configure magit-tagger

;;; Code:

(defun magit-tagger-install ()
  "Install and configure magit-tagger."
  (interactive)

  ;; Check if Magit is available
  (unless (require 'magit nil t)
    (user-error "Magit is not installed. Please install Magit first"))

  ;; Check if transient is available
  (unless (require 'transient nil t)
    (user-error "Transient is not available. Please update Magit or install transient"))

  ;; Load magit-tagger
  (require 'magit-tagger)

  ;; Set up integration
  (magit-tagger-setup)

  (message "magit-tagger installed successfully!")
  (message "Access it via 'M-x magit-tagger' or press 'T' in Magit status buffer")

  ;; Offer to add to init file
  (when (y-or-n-p "Add magit-tagger configuration to your init file? ")
    (magit-tagger--add-to-init)))

(defun magit-tagger--add-to-init ()
  "Add magit-tagger configuration to init file."
  (let ((config-text "
;; magit-tagger configuration
(use-package magit-tagger
  :load-path \"~/emacs/plugins/magit-tagger\"
  :after magit
  :config
  (magit-tagger-setup)
  ;; Optional customizations
  ;; (setq magit-tagger-default-increment-type 'patch)
  ;; (setq magit-tagger-auto-push-tags nil)
  ;; (setq magit-tagger-show-tag-info-after-creation t)
  )"))
    (with-current-buffer (find-file-noselect user-init-file)
      (goto-char (point-max))
      (insert config-text)
      (save-buffer))
    (message "Configuration added to %s" user-init-file)))

;; Auto-install when this file is evaluated
(when (and (boundp 'magit-tagger-auto-install)
           magit-tagger-auto-install)
  (magit-tagger-install))

(provide 'install)

;;; install.el ends here
