i=$((0))
while [ $(($i < 5)) == 1 ]; do
  "echo" "-ne" "$i"" "
  i=$(($i + 1))
done
"echo" "-e"
# Fibonacci
n=$((0))
i=$((0))
j=$((1))
while [ $(($n < 40)) == 1 ]; do
  k=$(($i + $j))
  i="$j"
  j="$k"
  n=$(($n + 1))
  "echo" "-e" "$k"
done
