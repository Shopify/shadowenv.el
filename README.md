# shadowenv.el
[![MELPA](https://melpa.org/packages/shadowenv-badge.svg)](https://melpa.org/#/shadowenv)

[Shadowenv](https://shopify.github.io/shadowenv) integration for emacs, per-buffer environment shadowing and eshell integration. Display the number of shadowed environment variables in the mode line.

## Commands
* `shadowenv-mode` Toggle shadowenv mode in a buffer.
* `shadowenv-global-mode` Enable global shadowenv mode.
* `shadowenv-reload` Reload shadowenv environment.
* `shadowenv-shadows` Display changes to the current environment.

## use-package
Here is an example `use-package` configuration:
```elisp
(use-package shadowenv
  :hook (after-init . shadowenv-global-mode))
```
