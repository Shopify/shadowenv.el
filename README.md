# shadowenv.el
[Shadowenv](https://shopify.github.io/shadowenv) integration for emacs, per-buffer environment shadowing and eshell integration. Display the number of shadowed environment variables in the mode line.

## Commands
* `shadowenv-mode` Toggle shadowenv mode in a buffer.
* `shadowenv-reload` Reload shadowenv environment.

## use-package
Here is an example `use-package` configuration:
```elisp
(use-package shadowenv
  :hook ((find-file . shadowenv-reload)
         (eshell-mode . shadowenv-reload)
         (eshell-directory-change . shadowenv-reload))
  :commands (shadowenv-mode))
  ```
