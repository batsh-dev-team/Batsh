if [ $((2 < 10)) == 1 ]; then
  "echo" "-e" "Yes"
fi
if [ $((1)) == 1 ]; then
  if [ $((0)) == 1 ]; then
    v=$((4 + 1))
  else
    v=$((2))
  fi
else
-
fi
"echo" "-e" "$v"
if [ $((2 > 1)) == 1 ]; then
  "echo" "-e" "True"
fi
if [ $((1 == 12)) == 1 ]; then
  "echo" "-e" "No"
fi
if [ "a" == "b" ]; then
  "echo" "-e" "No"
else
  "echo" "-e" "a is not b"
fi
num=$((43))
if [ "43" == "$num" ]; then
  "echo" "-e" "43 == num"
fi
_0="43"
if [ $(($_0 == $num)) == 1 ]; then
  "echo" "-e" "43 === num"
fi
