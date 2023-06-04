cyd@20230107

When using the solarized theme under emacs 28.0-28.2, it emits the error message:
error: (wrong-number-of-arguments (3 . 4) 2)

The reason is emacs 28 changed the signature of the macro "define-obsolete-function-alias".
It requires 3 parameters other than 2 parameters.

According to daviderestivo/galactic-emacs#26
We just need append a string parameter to "define-obsolete-function-alias"

So we change line 431-433:

(define-obsolete-function-alias 'create-solarized-theme-file 'solarized-create-theme-file "1.3.0")
(define-obsolete-function-alias 'create-solarized-theme 'solarized-create-theme "1.3.0")
(define-obsolete-function-alias 'create-solarized-theme-with-palette 'solarized-create-theme-with-palette "1.3.0")

where "1.3.0" is current version of Solarized theme.