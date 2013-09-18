# Batsh

Batsh is a simple programming language which compiles to Bash, Windows [Batch](http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/batch.mspx) (and [PowerShell](http://technet.microsoft.com/en-us/scriptcenter/powershell.aspx)).
Batsh enables you to write code once runs on all platforms without any additional dependency.

Both Bash and Batch are tricky and messy due to historical reasons.
You have to spend a lot of time learning either of them, and write paltform-dependent code.
If you happen to be a maintainer of a cross-platform tool which relies on Bash on Linux/Mac and Batch on Windows as "glue code", and found it painful to "synchronize" between them, you would like to try Batsh.

## How to get it

Batsh is under early-stage development and may change dramatically. No stable version is released. Use it at your risk.

Batsh is implemented in [OCaml](http://caml.inria.fr/ocaml/) and dependencies are managed by [OPAM](http://opam.ocamlpro.com/). Follow the steps below to run it on your computer:

1. Install OPAM. See [instructions](http://opam.ocamlpro.com/doc/Quick_Install.html).
2. Switch to the latest version (or at least 4.00.1) of OCaml by running `opam switch`.
3. Install dependencities: `opam install core menhir`
4. Run unit testa: `./test`
5. Compile your own code: `./batsh filename`

## Syntax

The syntax of Batsh is [C-based](https://en.wikipedia.org/wiki/List_of_C-based_programming_languages) (derived from C programming language).
If you have learned C, Java, C++ or JavaScript, Batsh is quite easy for you.

TODO

## Why not Python/Ruby/Node.js

Because they are not preinstalled on all platforms (including Windows) and functionalities like piping processes are not convinient to use.
