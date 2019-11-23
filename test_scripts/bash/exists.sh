[ -e "Makefile" ]
ex=$((!$?))
"echo" "-e" "$ex"
[ -e "Makefile" ]
if [ -e "Makefile" ]; then
  "echo" "-e" "Yes"
fi
if [ -e "none" ]; then
  "echo" "-e" "Impossible"
else
  "echo" "-e" "No"
fi
