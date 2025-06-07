;;; magit-tagger.el --- Enhanced git tag management for Magit -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Kyle Waldner
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (magit "3.0.0") (transient "0.3.0"))
;; Keywords: git, magit, tags, version
;; URL: https://github.com/yourusername/magit-tagger

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package extends Magit with enhanced tag management functionality.
;; It provides automatic version increment suggestions, detailed tag information,
;; and streamlined workflows for creating and managing git tags.

;; Features:
;; - Automatic next version suggestion based on latest tag
;; - Support for semantic versioning (patch, minor, major increments)
;; - Interactive tag creation with validation
;; - Detailed tag information display
;; - Remote push management
;; - Tag deletion functionality

;;; Code:

(require 'magit)
(require 'transient)

(defgroup magit-tagger nil
  "Enhanced git tag management for Magit."
  :group 'magit-extensions
  :prefix "magit-tagger-")

(defcustom magit-tagger-default-increment-type 'patch
  "Default increment type for version bumping."
  :type '(choice (const :tag "Patch" patch)
                 (const :tag "Minor" minor)
                 (const :tag "Major" major))
  :group 'magit-tagger)

(defcustom magit-tagger-auto-push-tags nil
  "Whether to automatically push tags to remote after creation."
  :type 'boolean
  :group 'magit-tagger)

(defcustom magit-tagger-show-tag-info-after-creation t
  "Whether to show detailed tag information after creating a tag."
  :type 'boolean
  :group 'magit-tagger)

(defcustom magit-tagger-key "M-t"
  "Key binding for magit-tagger in magit-dispatch.
This should be a key sequence string as understood by `kbd'.
Common choices: \"M-t\", \"C-c t\", \"T\", etc.

After changing this value, you may need to restart Emacs or
call `magit-tagger-setup' to apply the new binding."
  :type 'key-sequence
  :group 'magit-tagger)

;;; Utility Functions

(defun magit-tagger--run-git-command (args &optional default-directory)
  "Run git command with ARGS, return (success-p . output)."
  (let ((default-directory (or default-directory (magit-toplevel))))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process "git" nil t nil args)))
        (cons (= exit-code 0) (string-trim (buffer-string)))))))

(defun magit-tagger--get-latest-tag ()
  "Get the latest git tag."
  (let ((result (magit-tagger--run-git-command '("describe" "--tags" "--abbrev=0"))))
    (when (car result)
      (cdr result))))

(defun magit-tagger--parse-version (tag)
  "Parse version string like 'v1.2.3' into list (1 2 3)."
  (let ((version-str (string-remove-prefix "v" tag)))
    (condition-case nil
        (let ((parts (split-string version-str "\\.")))
          (when (= (length parts) 3)
            (mapcar #'string-to-number parts)))
      (error nil))))

(defun magit-tagger--increment-version (version-list increment-type)
  "Increment VERSION-LIST based on INCREMENT-TYPE."
  (let ((major (nth 0 version-list))
        (minor (nth 1 version-list))
        (patch (nth 2 version-list)))
    (pcase increment-type
      ('major (list (1+ major) 0 0))
      ('minor (list major (1+ minor) 0))
      ('patch (list major minor (1+ patch)))
      (_ (list major minor (1+ patch))))))

(defun magit-tagger--format-version (version-list)
  "Format VERSION-LIST back to string with 'v' prefix."
  (format "v%d.%d.%d" (nth 0 version-list) (nth 1 version-list) (nth 2 version-list)))

(defun magit-tagger--get-next-version (&optional increment-type)
  "Determine the next version based on current tag and INCREMENT-TYPE."
  (let* ((current-tag (magit-tagger--get-latest-tag))
         (increment-type (or increment-type magit-tagger-default-increment-type)))
    (if (not current-tag)
        "v0.0.1"
      (let ((current-version (magit-tagger--parse-version current-tag)))
        (if current-version
            (magit-tagger--format-version
             (magit-tagger--increment-version current-version increment-type))
          "v0.0.1")))))

(defun magit-tagger--tag-exists-p (tag-name)
  "Check if TAG-NAME already exists."
  (car (magit-tagger--run-git-command (list "rev-parse" tag-name))))

(defun magit-tagger--validate-version-format (tag)
  "Validate that TAG follows semantic versioning format."
  (let ((parsed (magit-tagger--parse-version tag)))
    (and parsed
         (= (length parsed) 3)
         (cl-every (lambda (n) (and (integerp n) (>= n 0))) parsed))))

;;; Interactive Functions

(defun magit-tagger-create-tag (tag-name description &optional push)
  "Create an annotated git tag with TAG-NAME and DESCRIPTION.
If PUSH is non-nil, push the tag to remote."
  (interactive
   (let* ((suggested-tag (magit-tagger--get-next-version))
          (tag-name (read-string (format "Tag name (%s): " suggested-tag)
                                 nil nil suggested-tag))
          (description (read-string "Tag description: "))
          (push (or magit-tagger-auto-push-tags
                    (y-or-n-p "Push tag to remote? "))))
     (list tag-name description push)))

  ;; Add 'v' prefix if not present
  (unless (string-prefix-p "v" tag-name)
    (setq tag-name (concat "v" tag-name)))

  ;; Validate format
  (unless (magit-tagger--validate-version-format tag-name)
    (user-error "Invalid version format '%s'. Expected format: v1.2.3" tag-name))

  ;; Check if tag exists
  (when (magit-tagger--tag-exists-p tag-name)
    (user-error "Tag '%s' already exists!" tag-name))

  ;; Require description
  (when (string-empty-p description)
    (user-error "Tag description is required!"))

  ;; Create tag
  (let ((result (magit-tagger--run-git-command
                 (list "tag" "-a" tag-name "-m" description))))
    (if (car result)
        (progn
          (message "Tag '%s' created successfully!" tag-name)
          (when magit-tagger-show-tag-info-after-creation
            (magit-tagger-show-tag-info tag-name))
          (when push
            (magit-tagger-push-tags)))
      (user-error "Error creating tag: %s" (cdr result)))))

(defun magit-tagger-show-tag-info (tag-name)
  "Show detailed information about TAG-NAME."
  (interactive
   (list (magit-completing-read "Show info for tag"
                                (magit-git-lines "tag" "-l")
                                nil t nil nil
                                (magit-tagger--get-latest-tag))))
  (let ((buffer-name (format "*magit-tagger: %s*" tag-name)))
    (with-current-buffer (get-buffer-create buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)

      ;; Insert tag details
      (insert (format "%s\n" (make-string 50 ?=)))
      (insert (format "TAG DETAILS: %s\n" tag-name))
      (insert (format "%s\n" (make-string 50 ?=)))

      (let ((tag-info (magit-tagger--run-git-command
                       (list "show" tag-name "--no-patch"))))
        (when (car tag-info)
          (insert (cdr tag-info))
          (insert "\n\n")))

      ;; Insert recent commits
      (insert (format "%s\n" (make-string 50 ?=)))
      (insert "RECENT COMMITS:\n")
      (insert (format "%s\n" (make-string 50 ?=)))

      (let ((log-info (magit-tagger--run-git-command
                       '("log" "--oneline" "--decorate" "-10"))))
        (when (car log-info)
          (insert (cdr log-info))))

      (setq buffer-read-only t)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun magit-tagger-delete-tag (tag-name &optional remote)
  "Delete TAG-NAME locally and optionally from REMOTE."
  (interactive
   (let* ((tag-name (magit-completing-read "Delete tag"
                                           (magit-git-lines "tag" "-l")
                                           nil t))
          (remote (when (y-or-n-p "Also delete from remote? ")
                    (magit-read-remote "Delete from remote"))))
     (list tag-name remote)))

  (when (y-or-n-p (format "Really delete tag '%s'%s? "
                          tag-name
                          (if remote (format " (including from %s)" remote) "")))
    ;; Delete local tag
    (let ((result (magit-tagger--run-git-command (list "tag" "-d" tag-name))))
      (if (car result)
          (progn
            (message "Tag '%s' deleted locally." tag-name)
            ;; Delete remote tag if requested
            (when remote
              (let ((remote-result (magit-tagger--run-git-command
                                    (list "push" remote "--delete" tag-name))))
                (if (car remote-result)
                    (message "Tag '%s' deleted from remote '%s'." tag-name remote)
                  (message "Error deleting tag from remote: %s" (cdr remote-result))))))
        (user-error "Error deleting tag: %s" (cdr result))))))

(defun magit-tagger-push-tags (&optional remote)
  "Push all tags to REMOTE (defaults to origin)."
  (interactive
   (list (if current-prefix-arg
             (magit-read-remote "Push tags to remote")
           "origin")))

  (let ((result (magit-tagger--run-git-command
                 (list "push" remote "--tags"))))
    (if (car result)
        (message "Tags pushed to remote '%s' successfully!" remote)
      (user-error "Error pushing tags: %s" (cdr result)))))

(defun magit-tagger-create-version-tag (increment-type)
  "Create a new version tag with automatic INCREMENT-TYPE."
  (interactive
   (list (intern (completing-read "Increment type: "
                                  '("patch" "minor" "major")
                                  nil t nil nil
                                  (symbol-name magit-tagger-default-increment-type)))))

  (let* ((suggested-tag (magit-tagger--get-next-version increment-type))
         (description (read-string "Tag description: "))
         (push (or magit-tagger-auto-push-tags
                   (y-or-n-p "Push tag to remote? "))))
    (magit-tagger-create-tag suggested-tag description push)))

;;; Transient Interface

;;;###autoload (autoload 'magit-tagger "magit-tagger" nil t)
(transient-define-prefix magit-tagger ()
  "Enhanced git tag management."
  :man-page "git-tag"
  ["Arguments"
   ("-p" "Push to remote" "--push")]
  ["Version Creation"
   ("p" "Create patch version (x.x.X+1)" magit-tagger-create-patch-tag)
   ("m" "Create minor version (x.X+1.0)" magit-tagger-create-minor-tag)
   ("M" "Create major version (X+1.0.0)" magit-tagger-create-major-tag)]
  ["Tag Management"
   ("c" "Create custom tag" magit-tagger-create-tag)
   ("s" "Show tag info" magit-tagger-show-tag-info)
   ("d" "Delete tag" magit-tagger-delete-tag)
   ("P" "Push tags" magit-tagger-push-tags)]
  ["Quick Actions"
   ("l" "List all tags" magit-show-refs)
   ("r" "Refresh" magit-refresh)])

(defun magit-tagger-create-patch-tag ()
  "Create a patch version tag."
  (interactive)
  (magit-tagger-create-version-tag 'patch))

(defun magit-tagger-create-minor-tag ()
  "Create a minor version tag."
  (interactive)
  (magit-tagger-create-version-tag 'minor))

(defun magit-tagger-create-major-tag ()
  "Create a major version tag."
  (interactive)
  (magit-tagger-create-version-tag 'major))

;;; Integration with Magit

;;;###autoload
(defun magit-tagger-setup ()
  "Set up magit-tagger integration with Magit."
  ;; Remove any existing binding first to avoid conflicts
  (ignore-errors
    (transient-remove-suffix 'magit-dispatch magit-tagger-key))
  ;; Add the binding with the current key
  (transient-append-suffix 'magit-dispatch "t"
    `(,magit-tagger-key "Enhanced Tagging" magit-tagger)))

;;;###autoload
(defun magit-tagger-change-key (new-key)
  "Change the magit-tagger key binding to NEW-KEY.
This is a convenience function for interactive key changes."
  (interactive "sNew key for magit-tagger: ")
  (customize-set-variable 'magit-tagger-key new-key)
  (when (featurep 'magit)
    (magit-tagger-setup))
  (message "magit-tagger key changed to %s" new-key))

;;;###autoload
(with-eval-after-load 'magit
  (magit-tagger-setup))

(provide 'magit-tagger)

;;; magit-tagger.el ends here
