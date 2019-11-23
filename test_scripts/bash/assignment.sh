a="Value: "$((1 + ((4 + 6) * 3)))
"echo" "-e" "$a"
b=$((3 + 4))
"echo" "-e" "$b"
c="$a"
"echo" "-e" "$c"
d="$b""$c"
"echo" "-e" "$d"
