# Batsh

Batsh is a simple programming language that compiles to Bash, Windows [Batch](http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/batch.mspx) (and [PowerShell](http://technet.microsoft.com/en-us/scriptcenter/powershell.aspx), in development).
It enables you to write code once runs on all platforms without any additional dependency.

Both Bash and Batch are messy to read and tricky to write due to historical reasons.
You have to spend a lot of time learning either of them, and write platform-dependent code for each operating system.
If you happen to be a maintainer of a cross-platform tool which relies on Bash on Linux/Mac and Batch on Windows as "glue code", and found it painful to "synchronize" between them, you would like to try Batsh.

## How to get it

You can try it [online](http://batsh.byvoid.com/) now!

Batsh is under early-stage development and may change dramatically. No stable version is released. You can check out [prereleases](https://github.com/BYVoid/Batsh/releases) and use it at your risk.

### Install from OPAM

Batsh is implemented in [OCaml](http://caml.inria.fr/ocaml/) and managed by [OPAM](http://opam.ocamlpro.com/pkg/batsh/0.0.2/).

1. Install OPAM. See [instructions](http://opam.ocamlpro.com/doc/Quick_Install.html).
2. Switch to the latest version (or at least 4.00.1) of OCaml by running `opam switch`.
3. Install Batsh: `opam install batsh`

### Build from source

1. Check out source code of Batsh.
2. `./configure`
3. `make`
4. Run: `./main.byte`

You can install dependencies by running `opam install core menhir ounit dlist`

If you want to run unit tests, configure with `./configure --enable-tests`, and run `make test`.

Build framework is managed by [Oasis](http://oasis.forge.ocamlcore.org/).

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
echo("Hello");
// On UNIX
output = ls();
// On Windows
Output = dir();
// Platform independent
output = readdir();
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

## Why not Python/Ruby/Node.js/LUA

Yes you can use any of them as platform-independent glue code.
But they are not **preinstalled on all platforms** (including Windows). Also functionalities like piping processes are not convenient to use. You can use Batsh to integrate existing code written in Bash or Batch as well.

## License

[MIT](http://opensource.org/licenses/MIT)
