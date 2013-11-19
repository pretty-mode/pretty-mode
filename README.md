# pretty-mode
[![Build Status](https://travis-ci.org/akatov/pretty-mode.png)](https://travis-ci.org/akatov/pretty-mode)
[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/akatov/pretty-mode/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

Redisplays parts of the Emacs buffer as pretty symbols. Highly configurable and extendable

## Installation

Install the ELPA package from [MELPA](http://melpa.milkbox.net/#/pretty-mode)

In your [`Cask` file](https://github.com/cask/cask):

```lisp
(source melpa)
(depends-on "pretty-mode")
```

## Usage

```lisp
(require 'pretty-mode)
; if you want to set it globally
(global-pretty-mode t)
; if you want to set it only for a specific mode
(add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)
```

## Development

```bash
cask install
make test
cask exec emacs -q -L . -eval "(progn (require 'pretty-mode) (global-pretty-mode t))"
```

## Credits

- Arthur Danskin released pretty-mode in 2008 which fontifies various symbols.
- [Grant Rettke](https://github.com/grettke) added support for some Lispy stuff,
  Unicode character documentation, and Unicode source references.
- [Dmitri Akatov](https://github.com/akatov) is currently maintaining
  `pretty-mode` and is adding support for more symbols, more modes and
  customization.
