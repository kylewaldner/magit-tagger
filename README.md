# magit-tagger

Enhanced git tag management for Magit - an Emacs extension that provides advanced tagging functionality with automatic version management.

## Features

- **Automatic Version Suggestions**: Automatically suggests the next version based on your latest git tag
- **Semantic Versioning**: Support for patch, minor, and major version increments
- **Interactive Tag Creation**: Streamlined workflow for creating annotated tags with validation
- **Detailed Tag Information**: View comprehensive tag details and recent commits
- **Remote Management**: Push tags to remote repositories with confirmation
- **Tag Deletion**: Safe tag deletion with confirmation prompts
- **Magit Integration**: Seamlessly integrates with Magit's transient interface

## Installation

### Using straight.el (Recommended)

**Declarative (in your config):**
```elisp
(straight-use-package '(magit-tagger :type git :host github :repo "kylewaldner/magit-tagger"))
```

**Interactive (manual install):**
```
M-x straight-use-package RET magit-tagger RET
```
Then provide the recipe when prompted:
```
(:type git :host github :repo "kylewaldner/magit-tagger")
```

**With use-package:**
```elisp
(use-package magit-tagger
  :straight (:type git :host github :repo "kylewaldner/magit-tagger")
  :after magit)
```

### Manual Installation via git

1. Clone this repository or download `magit-tagger.el`
2. Add the directory to your Emacs load path:
   ```elisp
   (add-to-list 'load-path "/path/to/magit-tagger")
   ```
3. Require the package:
   ```elisp
   (require 'magit-tagger)
   ```

## Usage

### Quick Start

1. In any git repository, open Magit (`M-x magit-status`)
2. Press `M-t` to open the enhanced tagging interface
3. Choose from the available options:
   - `p` - Create patch version (e.g., v1.0.0 → v1.0.1)
   - `m` - Create minor version (e.g., v1.0.0 → v1.1.0)
   - `M` - Create major version (e.g., v1.0.0 → v2.0.0)
   - `c` - Create custom tag
   - `s` - Show tag information
   - `d` - Delete tag
   - `P` - Push tags to remote

### Automatic Version Management

The plugin automatically:
- Detects your latest git tag
- Parses semantic version format (v1.2.3)
- Suggests the appropriate next version
- Validates version format before creation
- Prevents duplicate tag creation

### Example Workflow

1. **Create a patch version**: Press `M-t` then `p`
   - Plugin suggests next patch version (e.g., v1.2.4)
   - Enter tag description
   - Choose whether to push to remote
   - View tag details automatically

2. **Create a custom tag**: Press `M-t` then `c`
   - Enter custom tag name (v-prefix added automatically)
   - Enter description
   - Validation ensures proper format
   - Option to push to remote

3. **View tag information**: Press `M-t` then `s`
   - Select tag from completion list
   - View detailed tag information
   - See recent commits in context

## Configuration

### Customization Options

```elisp
;; Customize the key binding (default: "M-t")
(setq magit-tagger-key "T")          ; Use "T" instead of "M-t"
;; or
(setq magit-tagger-key "C-c t")      ; Use "C-c t"

;; Set default increment type
(setq magit-tagger-default-increment-type 'patch) ; 'patch, 'minor, or 'major

;; Automatically push tags after creation
(setq magit-tagger-auto-push-tags t)

;; Show tag info after creation
(setq magit-tagger-show-tag-info-after-creation t)
```

**Note**: When using `use-package`, set the key binding before loading:

```elisp
(use-package magit-tagger
  :straight (:type git :host github :repo "kylewaldner/magit-tagger")
  :after magit
  :custom
  (magit-tagger-key "T")  ; Set your preferred key
  (magit-tagger-default-increment-type 'patch))
```

You can also change the key binding interactively:
- `M-x magit-tagger-change-key` - Change the key binding at runtime
- `M-x customize-group RET magit-tagger RET` - Access all customization options

### Key Bindings

The plugin integrates with Magit's transient system. Access it via:
- `M-x magit-tagger` - Direct access to tagging interface
- In Magit status buffer: `M-t` (default) - Enhanced tagging menu

**Note**: The key binding is customizable via `magit-tagger-key`. See [Customization Options](#customization-options) above.

## Comparison with Python Script

This Emacs extension provides the same core functionality as the original Python script:

| Feature | Python Script | magit-tagger |
|---------|---------------|--------------|
| Version auto-increment | ✓ | ✓ |
| Tag validation | ✓ | ✓ |
| Interactive prompts | ✓ | ✓ |
| Tag information display | ✓ | ✓ |
| Remote push management | ✓ | ✓ |
| Tag deletion | ✓ | ✓ |
| Semantic versioning | ✓ | ✓ |
| Magit integration | ✗ | ✓ |
| Emacs workflow | ✗ | ✓ |
| Transient interface | ✗ | ✓ |

## Requirements

- Emacs 25.1 or later
- Magit 3.0.0 or later
- Git (obviously!)
