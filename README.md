# shadowenv.el
[Shadowenv](https://shopify.github.io/shadowenv) integration for emacs. Lists the number of shadowed environment variables in the mode line.

## Commands
`shadowenv-mode` Toggle shadowenv mode in a buffer. `shadowenv-reload` Reload shadowenv environment.

## Hooks
To setup shadowenv for each file that gets opened, `shadowenv-mode` can be hooked to `find-file-hook`.  For eshell `shadowenv-mode` can be hooked to `eshell-mode-hook` to set itself up on eshell opening and `shadowenv-reload` can be hooked to `eshell-directory-change` to set up shadowenv for each directory. 

## use-package
Here is an example `use-package` configuration:
```elisp
(use-package shadowenv
  :hook ((find-file . shadowenv-reload)
         (eshell-mode . shadowenv-reload)
         (eshell-directory-change . shadowenv-reload))
  :commands (shadowenv-mode))
  ```
