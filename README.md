# shadowenv.el
[Shadowenv](https://shopify.github.io/shadowenv) integration for emacs, per-buffer environment shadowing and eshell integration. Display the number of shadowed environment variables in the mode line.

## Commands
* `shadowenv-mode` Toggle shadowenv mode in a buffer.
* `shadowenv-reload` Reload shadowenv environment.
* `shadowenv-shadows` Display changes to the environment variables.

## use-package
Here is an example `use-package` configuration:
```elisp
  (use-package shadowenv
  :load-path "~/.emacs.d/shadowenv"
  :hook ((find-file . shadowenv-reload)
         (eshell-mode . shadowenv-reload)
         (eshell-directory-change . shadowenv-reload)
         (comint-mode . shadowenv-reload))
  :commands (shadowenv-mode))
```
