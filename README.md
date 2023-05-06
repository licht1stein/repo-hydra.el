# repo-hydra
Easily create repo-specific menus.

This library was inspired by an amazingly instructinve [interview](https://www.youtube.com/watch?v=2nH59edD5Uo) by Juan Monetta ([@jpmonettas](https://github.com/jpmonettas)), he also wrote the main macro which I only slightly improved and turned into a library.

## Examples
### Normal repo hydra
This is an example repo-hydra for this repository. If `repo-hydra-show` is called from within the `repo-hydra.el` repository, the hydra will show.
```elisp
(repo-hydra-define
 "repo-hydra.el"
 ("i" text-scale-increase "In")
 ("o" text-scale-decrease "Out"))
 ```
 
  ![](./example/demo-1.gif)
 

## Install

### Using use-package and straight
```elisp
(use-package repo-hydra
  :straight (:type git :host github :repo "licht1stein/repo-hydra.el")
  :bind ("<f6>" . repo-hydra-show))
```


## Versioning
This library uses break versioning: https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md
