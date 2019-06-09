# vline-mode [![Build Status](https://travis-ci.org/EricCrosson/vline-mode.svg?branch=master)](https://travis-ci.org/EricCrosson/vline-mode) [![Version](https://img.shields.io/github/tag/EricCrosson/vline-mode.svg)](https://github.com/EricCrosson/vline-mode/releases)

> Minor-mode to highlight current column

## Install

With [Quelpa](https://framagit.org/steckerhalter/quelpa)

``` {.sourceCode .lisp}
(use-package vline-mode
  :quelpa (vline-mode
           :fetcher github
           :repo "EricCrosson/vline-mode"))
```

Or manually, after downloading into your `load-path`

``` {.sourceCode .lisp}
(require 'vline-mode)
```

## Use

To display a vertical line, invoke `M-x vline-mode`.  `vline-mode`
doesn't effect other buffers, because it is a buffer local minor mode.
To hide a vertical line, type `M-x vline-mode` again.

To display a vertical line in all buffers, type `M-x vline-global-mode`.

<!-- ## Example -->

<!-- ![TODO: set hover-text](https://raw.githubusercontent.com/EricCrosson/vline-mode/master/img/demo.{TODO: set filetype png,gif}) -->

## Acknowledgments

This is not my code, it is just hosted here for security's sake.

Many thanks to all involved with this package, especially Taiki SUGAWARA.

## License

GPL 2 (or higher) © [Free Software Foundation, Inc](http://www.fsf.org/about).
