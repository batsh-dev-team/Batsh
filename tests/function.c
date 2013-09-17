v1 = "Global V1";
v2 = "Global V2";

function func1(p1, p2) {
  echo(p1, p2);
}

function func2(p) {
  v1 = "Local " ++ p;
  echo(v1);
  echo(v2);
}

func1("Hello", "World");
func2("Var");
echo(v1, p);
