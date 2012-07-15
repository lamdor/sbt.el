# sbt.el #

## Getting started ##

There are two main ways to get started with `sbt.el`. Either through [el-get]() or manually

### el-get ###

Add to your emacs init.el/initialization or el-get configuration.

```lisp
(add-to-list 'el-get-sources
  (:name sbt
   :website "https://github.com/rubbish/sbt.el"
   :description "support for running sbt in inferior mode."
   :type github
   :pkgname "rubbish/sbt.el"
   :post-init (add-hook 'scala-mode 'turn-on-sbt-mode)))
```

### Manually ###

  1. Clone somewhere onto your emacs load-path.
  2. Add to your emacs init.el or initialization.
```lisp
(autoload 'turn-on-sbt-mode "sbt" "" t)
(add-hook 'scala-mode 'turn-on-sbt-mode)
```

## Keybindings ##

  * `C-c s s` -- starts an interactive sbt repl
  * `C-c s c` -- sends `compile` to the sbt repl
  * `C-c s t` -- sends `test` to the sbt repl
  * `C-c s o` -- if in a test file, sends `test-only com.test.SomeTest` to the sbt repl

## License ##

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.
