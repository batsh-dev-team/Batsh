// Function call
function func1(p1, p2) {
  echo(p1, p2);
}
func1("Hello", "World");

// Global and local variables
v1 = "Global V1";
v2 = "Global V2";
v3 = "Global V3";
function func2(p) {
  v1 = "Local " ++ p;
  echo(v1);
  echo(v2);
  global v3;
  v3 = "V3 Modified.";
}
func2("Var");
echo(v1);
echo(p);
echo(v3);

// Return value
function func3(num) {
  echo(num + 41);
}
func3(4);
ret = func3(1);
echo("Returned:", ret);
