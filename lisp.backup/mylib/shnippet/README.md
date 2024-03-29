# Haskell-Snippets


**Haskell-Snippets** is a collection of
[YASnippet][yas]
[Haskell][haskell] snippets for Emacs.


## Installation

Enable the Emacs package manager and make sure the melpa package
repository is set up.

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

<kbd>M-x package-install haskell-snippets</kbd>

Add the following to your Emacs config:

```lisp
(require 'haskell-snippets)
```

Snippets may have to be recompiled and reloaded in Emacs if YASnippet
is already in use:

<kbd>M-x yas-recompile-all</kbd>
<kbd>M-x yas-reload-all</kbd>

Haskell snippts should now be available to use! In a `haskell-mode`
buffer, type `fn<TAB>`. A prompt should appear asking which `fn`
snippet to expand.

I **highly** recommend using YASnippet with [ido-mode]. Configure
Emacs:

```lisp
(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
```

This is important so that alternatives (like `import` vs. `import
qualified`) can quickly be selected with a single key stroke.


## Available Expansion Keys

* `new` - newtype
* `mod` - module [simple, exports]
* `main ` - main module and function
* `let` - let bindings
* `lang` - language extension pragmas
* `opt` - GHC options pragmas
* `\` - lambda function
* `inst` - instance declairation
* `imp` - import modules [simple, qualified]
* `if` - if conditional [inline, block]
* `<-` - monadic get
* `fn` - top level function [simple, guarded, clauses]
* `data` - data type definition [inline, record]
* `=>` - type constraint
* `{-` - block comment
* `case` - case statement


## Design Ideals

* Keep snippet keys (the prefix used to auto-complete) to four
  characters or less while still being as easy to guess as possible.

* Have as few keys as possible. The more keys there are to remember,
  the harder snippets are to use and learn.

* Leverage [ido-mode][] when reasonable. For instance, to keep the
  number of snippet keys to a minimum as well as auto complete things
  like [Haskell Langauge Extension Pragmas][lang-pragma]. When
  multiple snippets share a key (ex: `fn`), the `ido-mode` prompts are
  unique to one character (ex: `guarded function` and `simple
  function` are `g` and `s` respectively).


## Authors

This code is written and maintained by Luke Hoersten,
<luke@hoersten.org>.


[yas]: https://github.com/capitaomorte/yasnippet
[ido-mode]: http://www.emacswiki.org/emacs/InteractivelyDoThings
[lang-pragma]: http://hackage.haskell.org/packages/archive/Cabal/1.16.0.3/doc/html/Language-Haskell-Extension.html#t:KnownExtension
[haskell]: http://haskell.org/
