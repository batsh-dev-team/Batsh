v1 = "Global V1";
v2 = "Global V2";
v3 = "Global V3";

function func1(p1, p2) {
  echo(p1, p2);
}

function func2(p) {
  v1 = "Local " ++ p;
  echo(v1);
  echo(v2);
  global v3;
  v3 = "V3 Modified.";
}

func1("Hello", "World");
func2("Var");
echo(v1);
echo(p);
echo(v3);
