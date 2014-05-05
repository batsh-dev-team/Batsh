# Batsh

Batsh is a simple programming language that compiles to Bash and Windows [Batch](http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/batch.mspx).
It enables you to write your script once runs on all platforms without **any** additional dependency.

Both Bash and Batch are messy to read and tricky to write due to historical reasons.
You have to spend a lot of time learning either of them and write platform-dependent code for each operating system.
I have wasted lots of time in my life struggling with bizare syntaxes and unreasonable behaviors of them, and do not want to waste any more.

If you happen to be a maintainer of a cross-platform tool which relies on Bash on Linux/Mac and Batch on Windows as "glue code", and found it painful to "synchronize" between them, you would definitely like to try Batsh.

## How to get it

### The easiest way

[Try it online: http://batsh.org](http://batsh.org/)

### Install from OPAM

Batsh is implemented in OCaml and managed by [OPAM](http://opam.ocaml.org/pkg/batsh/0.0.5/).

1. Install OPAM. See [instructions](http://opam.ocaml.org/doc/Quick_Install.html).
2. Switch to the latest version (or at least 4.00.1) of OCaml by running `opam switch`.
3. Install Batsh: `opam install batsh`

### Build from source

You have to install OCaml (version 4.00.1 or higher) development environment before compiling Batsh from source code, and follow steps below:

1. Download source code of Batsh from [releases](https://github.com/BYVoid/Batsh/releases) or clone with git.
2. Uncompress source code tarball.
3. `make`
4. `make install`
5. Run: `batsh`

#### Dependencies

If there is any missing dependency, you can install them by running `opam install ocp-build core ounit dlist cmdliner`

* [ocp-build](http://www.typerex.org/ocp-build.html): Build framework.
* [core](http://janestreet.github.io/): An industrial strength alternative to OCaml's standard library.
* [ounit](http://ounit.forge.ocamlcore.org/): Unit test framework.
* [dlist](https://github.com/BYVoid/Dlist): A purely functional list-like data structure supporting O(1) concatenation.
* [cmdliner](http://erratique.ch/software/cmdliner): Command line interfaces parser.

## Syntax

The syntax of Batsh is [C-based](https://en.wikipedia.org/wiki/List_of_C-based_programming_languages) (derived from C programming language).
If you have learned C, Java, C++ or JavaScript, Batsh is quite easy for you.

### Assignment

```javascript
a = 1;
b = "string";
c = [1, 2, "str", true, false];
```

### Expression

```javascript
a = 1 + 2;
b = a * 7;
c = "Con" ++ "cat";
d = c ++ b;
```

### Command

```javascript
// On UNIX
output = ls();
// On Windows
output = dir();
// Platform independent
output = readdir();

// Test existence
ex = exists("file.txt");
```

### If condition

```javascript
a = 3;
if (a > 2) {
  println("Yes");
} else {
  println("No");
}
```

### Loop

```javascript
// Fibonacci
n = 0;
i = 0;
j = 1;
while (n < 60) {
  k = i + j;
  i = j;
  j = k;
  n = n + 1;
  println(k);
}
```

### Function

```javascript
v1 = "Global V1";
v2 = "Global V2";
function func(p) {
  v1 = "Local " ++ p;
  global v2;
  v2 = "V3 Modified.";
}
func("Var");
```

### Recursion

```javascript
function fibonacci(num) {
  if (num == 0) {
    return 0;
  } else if (num == 1) {
    return 1;
  } else {
    return (fibonacci(num - 2) + fibonacci(num - 1));
  }
}
println(fibonacci(8));
```

### [More examples](https://github.com/BYVoid/Batsh/tree/master/tests)

## Built-in functions

In order to make script cross-platform, Batsh provided some "built-in" functions that will compile to platform-dependent code. It is assumed that Bash script runs on Linux or Mac OS and Batch script runs on Windows (XP or higher), which means Cygwin or wine are not supported.

### `print(text, ...)`

Prints a text string to console without a newline.

### `println(text, ...)`

Prints a text string to console with a new line (LF for bash, CRLF for batch).

### `call(path, arg, ...)`

Runs command from path through shell.

### `bash(rawStatement)`

Put `rawStatement` into compiled code for Bash. Ignore for Windows Batch.

### `batch(rawStatement)`

Put `rawStatement` into compiled code for Windows Batch. Ignore for Bash.

### `readdir(path)`

Equals to `ls` and `dir /w`.

### `exists(path)`

Test existence of given path. 

## Command Line Usage

```
NAME
       batsh - A language that compiles to Bash and Windows Batch.

SYNOPSIS
       batsh COMMAND ...

COMMANDS
       bash
           Compile to Bash script.

       batsh
           Format source file.

       winbat
           Compile to Windows Batch script.

OPTIONS
       --help[=FMT] (default=pager)
           Show this help in format FMT (pager, plain or groff).

       --version
           Show version information.
```

## Why not Python/Ruby/Node.js/Lua

Yes you can use any of them as platform-independent glue code. But there are several disadvantages:

1. None of them is **preinstalled on all platforms** (including Windows).
2. Functionalities like process piping are not convenient to use.
3. Hard to integrate with existing code written in Bash or Batch.

Those reasons are why I developed Batsh.

## License

[MIT](http://opensource.org/licenses/MIT)

## Contributors

* [Carbo Kuo](https://github.com/BYVoid)
* [Song Zhang](http://www.linkedin.com/pub/song-zhang/76/632/b51)
* [Anthony Chan](https://github.com/anthonyhchan)
* [jeb-de](https://github.com/jeb-de)
* [Al Ramirez](https://github.com/mirez)
* [Nixola](https://github.com/Nixola)
