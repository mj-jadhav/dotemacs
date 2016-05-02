# MJ's dotemacs configuration

Basic emacs configuration for clojure, cider and magit

Still a work in progress...

## Prerequisites

Your `~/.lein/profiles.clj` should at least have the following stuff:

```Clojure
{:repl {:dependencies [[org.clojure/tools.nrepl "0.2.12"]]}}
```

## Features

- C-x g         Opens magit status
- C-x C-a       Jump to web search: Google, wikipedia, emacs-wiki
- C-x C-f ~     Go to HOME
- window-numbering with M-1, M-2

NOTE: More keybindings can be found under .emacs.d/init.el file

## Getting started

To learn Emacs you should follow any generic Emacs tutorial. If you're
a Clojure programmer then you may follow the guide in
[Brave Clojure](http://www.braveclojure.com/basic-emacs/).
