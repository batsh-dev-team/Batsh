@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

echo 0
echo 1
echo 42
set /a _0=^(1 + ^(^(4 + 6^) * 3^)^)
echo !_0!
set /a _1=^(8 - ^(3 %% 2^)^)
echo !_1!
set /a _2=^(-9 - 9^)
echo !_2!
set /a _3=^(^(2 + 8^) / 3^)
echo !_3!
if 2 EQU 2 (
  set /a _4=1
) else (
  set /a _4=0
)
echo !_4!
if 6 NEQ 8 (
  set /a _5=1
) else (
  set /a _5=0
)
echo !_5!
if 3 GTR 2 (
  set /a _6=1
) else (
  set /a _6=0
)
echo !_6!
if 4 LSS 5 (
  set /a _7=1
) else (
  set /a _7=0
)
echo !_7!
if 6 GEQ 2 (
  set /a _8=1
) else (
  set /a _8=0
)
echo !_8!
if 19 LEQ 30 (
  set /a _9=1
) else (
  set /a _9=0
)
echo !_9!
if 1 NEQ 1 (
  set /a _10=1
) else (
  set /a _10=0
)
echo !_10!
if 0 NEQ 1 (
  set /a _11=1
) else (
  set /a _11=0
)
echo !_11!
set /a _12=^(2 - 1^)
if !_12! NEQ 1 (
  set /a _13=1
) else (
  set /a _13=0
)
echo !_13!
