;;; example-config.el --- Example configuration for magit-tagger -*- lexical-binding: t; -*-

;; This file shows different ways to configure magit-tagger

;;; Basic Configuration

;; Option 1: Simple setup (minimal configuration)
(require 'magit-tagger)
(magit-tagger-setup)

;; Option 2: Using use-package (recommended)
(use-package magit-tagger
  :load-path "~/emacs/plugins/magit-tagger"
  :after magit
  :config
  (magit-tagger-setup))

;; Option 3: Full configuration with customizations
(use-package magit-tagger
  :load-path "~/emacs/plugins/magit-tagger"
  :after magit
  :custom
  ;; Set default increment type (patch, minor, or major)
  (magit-tagger-default-increment-type 'patch)
  
  ;; Automatically push tags after creation (default: nil)
  (magit-tagger-auto-push-tags nil)
  
  ;; Show tag info after creating a tag (default: t)
  (magit-tagger-show-tag-info-after-creation t)
  
  :config
  (magit-tagger-setup)
  
  ;; Optional: Add custom keybindings
  :bind
  (:map magit-status-mode-map
        ("C-c t" . magit-tagger)))

;;; Advanced Configuration Examples

;; Example: Different defaults for different projects
(defun my-magit-tagger-setup ()
  "Custom setup for magit-tagger based on project."
  (let ((project-root (magit-toplevel)))
    (cond
     ;; For library projects, default to patch increments
     ((string-match-p "lib\\|library" project-root)
      (setq-local magit-tagger-default-increment-type 'patch))
     
     ;; For application projects, default to minor increments
     ((string-match-p "app\\|application" project-root)
      (setq-local magit-tagger-default-increment-type 'minor))
     
     ;; For experimental projects, auto-push tags
     ((string-match-p "experiment\\|demo" project-root)
      (setq-local magit-tagger-auto-push-tags t)))))

;; Hook the custom setup to magit
(add-hook 'magit-status-mode-hook #'my-magit-tagger-setup)

;; Example: Custom tag naming scheme
(defun my-custom-tag-prefix ()
  "Get custom tag prefix based on branch."
  (let ((branch (magit-get-current-branch)))
    (cond
     ((string= branch "main") "release-")
     ((string= branch "develop") "dev-")
     (t "feature-"))))

;;; Keybinding Examples

;; Global keybinding for quick access
(global-set-key (kbd "C-c C-t") #'magit-tagger)

;; Magit-specific keybindings
(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "T") #'magit-tagger)
  (define-key magit-status-mode-map (kbd "C-t p") #'magit-tagger-create-patch-tag)
  (define-key magit-status-mode-map (kbd "C-t m") #'magit-tagger-create-minor-tag)
  (define-key magit-status-mode-map (kbd "C-t M") #'magit-tagger-create-major-tag))

;;; Integration with other packages

;; Example: Integration with projectile
(with-eval-after-load 'projectile
  (defun projectile-magit-tagger ()
    "Open magit-tagger in current projectile project."
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (magit-tagger)))
  
  (define-key projectile-mode-map (kbd "C-c p t") #'projectile-magit-tagger))

;; Example: Integration with which-key for better discoverability
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c t" "tag management"
    "C-c t p" "patch version"
    "C-c t m" "minor version"
    "C-c t M" "major version"))

;;; Hooks and Automation

;; Example: Automatically create patch tags on certain actions
(defun auto-tag-on-release ()
  "Automatically create a patch tag when pushing to main branch."
  (when (and (string= (magit-get-current-branch) "main")
             (y-or-n-p "Create release tag? "))
    (call-interactively #'magit-tagger-create-patch-tag)))

;; Uncomment to enable auto-tagging
;; (add-hook 'magit-post-commit-hook #'auto-tag-on-release)

;;; example-config.el ends here 