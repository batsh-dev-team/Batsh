@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

if /i 2 LSS 10 (
  echo Yes
)
if /i 1 EQU 1 (
  if /i 1 NEQ 1 (
    set /a v=^(4 + 1^)
  ) else (
    set /a v=2
  )
) else (

)
echo !v!
if /i 2 GTR 1 (
  echo True
)
if /i 1 EQU 12 (
  echo No
)
if /i a EQU b (
  echo No
) else (
  echo a is not b
)
set /a num=43
if /i 43 EQU !num! (
  echo 43 ^=^= num
)
set _0=43
if /i !_0! EQU !num! (
  echo 43 ^=^=^= num
)
