@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

if 2 LSS 10 (
  echo Yes
)
if 1 EQU 1 (
  if 1 NEQ 1 (
    set /a v=^(4 + 1^)
  ) else (
    set /a v=2
  )
) else (

)
echo !v!
if 2 GTR 1 (
  echo True
)
if 1 EQU 12 (
  echo No
)
if a EQU b (
  echo No
) else (
  echo a is not b
)
set /a num=43
if 43 EQU !num! (
  echo 43 ^=^= num
)
set _0=43
if !_0! EQU !num! (
  echo 43 ^=^=^= num
)
