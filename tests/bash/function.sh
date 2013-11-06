# Function call
function func1 {
  local p1
  local p2
  p1="$1"
  p2="$2"
  "echo" "-e" "$p1" "$p2"
}
"func1" "Hello" "World"
# Global and local variables
v1="Global V1"
v2="Global V2"
v3="Global V3"
function func2 {
  local v1
  local p
  p="$1"
  v1="Local ""$p"
  "echo" "-e" "$v1"
  "echo" "-e" "$v2"
  
  v3="V3 Modified."
}
"func2" "Var"
"echo" "-e" "$v1"
"echo" "-e" "$v3"
# Return value
function func3 {
  local num
  num="$1"
  "echo" "-ne" $(($num + 41))
  return
}
"func3" $((4))
"echo" "-e"
ret=$("func3" $((1)))
"echo" "-e" "Returned:" "$ret"
# Argument containing space
function g {
  local text
  text="$1"
  "echo" "-ne" "$text"
  return
}
function f {
  local text
  text="$1"
  "echo" "-ne" $("g" "$text")
  return
}
test=$("f" "Param with space")
"echo" "-e" "$test"
