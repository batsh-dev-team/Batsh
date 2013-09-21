function fact(num) {
  if (num == 0) {
    echo(1);
  } else {
    echo(fact(num - 1) * num);
  }
}
fact(5);

function fibonacci(num) {
  if (num == 0) {
    echo(0);
  } else if (num == 1) {
    echo(1);
  } else {
    echo(fibonacci(num - 2) + fibonacci(num - 1));
  }
}

i = 0;
while (i < 7) {
  echo(fibonacci(i));
  i = i + 1;
}
