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
  set _0=a is not b
  echo !_0!
)
set /a num=43
if /i 43 EQU !num! (
  set _1=43 ^=^= num
  echo !_1!
)
set _2=43
if /i !_2! EQU !num! (
  set _3=43 ^=^=^= num
  echo !_3!
)
