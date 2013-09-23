function fact(num) {
  if (num === 0) {
    print(1);
  } else {
    print(fact(num - 1) * num);
  }
}
println(fact(5));

function fibonacci(num) {
  if (num === 0) {
    print(0);
  } else if (num === 1) {
    print(1);
  } else {
    print(fibonacci(num - 2) + fibonacci(num - 1));
  }
}

i = 0;
while (i < 7) {
  println(fibonacci(i));
  i = i + 1;
}
