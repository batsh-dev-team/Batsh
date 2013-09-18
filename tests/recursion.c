function fact(num) {
  if (num == 0) {
  	echo(1);
  } else {
  	v = fact(num - 1);
  	echo(v * num);
  }
}
fact(5);

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

i = 0;
while (i < 7) {
  echo(fibonacci(i));
  i = i + 1;
}
