# Batsh

Batsh is a simple programming language that compiles to Bash, Windows [Batch](http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/batch.mspx) (and [PowerShell](http://technet.microsoft.com/en-us/scriptcenter/powershell.aspx)).
Batsh enables you to write code once runs on all platforms without any additional dependency.

Both Bash and Batch are tricky and messy due to historical reasons.
You have to spend a lot of time learning either of them, and write platform-dependent code.
If you happen to be a maintainer of a cross-platform tool which relies on Bash on Linux/Mac and Batch on Windows as "glue code", and found it painful to "synchronize" between them, you would like to try Batsh.

## How to get it

Batsh is under early-stage development and may change dramatically. No stable version is released. Use it at your risk.

Batsh is implemented in [OCaml](http://caml.inria.fr/ocaml/) and dependencies are managed by [OPAM](http://opam.ocamlpro.com/). Follow the steps below to run it on your computer:

1. Install OPAM. See [instructions](http://opam.ocamlpro.com/doc/Quick_Install.html).
2. Switch to the latest version (or at least 4.00.1) of OCaml by running `opam switch`.
3. Install dependencies: `opam install core menhir`
4. Run unit testa: `./test`
5. Compile your own code: `./batsh filename`

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
output = Batsh.readdir();
```

### If condition

```javascript
a = 3;
if (a > 2) {
  echo("Yes");
} else {
  echo("No");
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
  echo(k);
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
    echo(0);
  } else if (num == 1) {
    echo(1);
  } else {
    v0 = fibonacci(num - 2);
    v1 = fibonacci(num - 1);
    echo(v0 + v1);
  }
}
```

## Why not Python/Ruby/Node.js

Because they are not preinstalled on all platforms (including Windows) and functionalities like piping processes are not convenient to use.
