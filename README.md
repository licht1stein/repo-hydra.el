# repo-hydra
Easily create repo-specific menus.

This library was inspired by an amazingly instructinve [interview](https://www.youtube.com/watch?v=2nH59edD5Uo) by Juan Monetta ([@jpmonettas](https://github.com/jpmonettas))

## Install

### Using use-package and straight
```elisp
(use-package repo-hydra
  :straight (:type git :host github :repo "licht1stein/repo-hydra.el")
  :bind ("<f6>" . repo-hydra-show))
```

## Versioning
This library uses break versioning: https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md
