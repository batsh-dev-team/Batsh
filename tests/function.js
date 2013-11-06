// Function call
function func1(p1, p2) {
  println(p1, p2);
}
func1("Hello", "World");

// Global and local variables
v1 = "Global V1";
v2 = "Global V2";
v3 = "Global V3";
function func2(p) {
  v1 = "Local " ++ p;
  println(v1);
  println(v2);
  global v3;
  v3 = "V3 Modified.";
}
func2("Var");
println(v1);
println(v3);

// Return value
function func3(num) {
  return num + 41;
}
func3(4);
println();
ret = func3(1);
println("Returned:", ret);

// Argument containing space
function g(text) {
  return text;
}
function f(text) {
  return g(text);
}
test = f("Param with space");
println(test);
