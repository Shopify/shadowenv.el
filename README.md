# shadowenv.el
[Shadowenv](https://shopify.github.io/shadowenv) integration for emacs, per-buffer environment shadowing and eshell integration. Display the number of shadowed environment variables in the mode line.

## Commands
* `shadowenv-mode` Toggle shadowenv mode in a buffer.
* `shadowenv-global-mode` Enable shadowenv global mode.
* `shadowenv-reload` Reload shadowenv environment.
* `shadowenv-shadows` Display changes to the current environment.

## use-package
Here is an example `use-package` configuration:
```elisp
(use-package shadowenv
  :load-path "~/.emacs.d/shadowenv"
  :config
  (shadowenv-global-mode))
```
