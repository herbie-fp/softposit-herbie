Herbie support for posits
===

This repository contains a plugin for [Herbie](https://herbie.uwplse.org) to support [posit arithmetic](https://posithub.org/) using the SoftPosit package (through [David's package](https://pkgs.racket-lang.org/package/softposit-rkt)). Installing this package will:

+ Enable functions like `real->posit16` or `+.p16` in Herbie
+ Teach Herbie rewrite rules for these operators
+ Allow you to specify `:precision posit16` for Herbie inputs

The best way to install this package is using the Racket package manager:

    raco pkg install softposit-herbie
