@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

echo BYVoid
echo Slash/
echo Backslash\
echo Quote^"^'
echo Tab	Tab
rem println("Newline\nLine2");
rem println("!");
echo http://www.byvoid.com
set /a _0=^(6 / 2^)
set /a _1=^(3 + 5^)
echo !_0!BYVoid!_1!
set _2=3
set /a _3=^(3 + !_2!^)
echo !_3!
set _4=3
set /a _5=^(3 + !_4!^)
echo !_5!2
set _6=32
set /a _7=^(3 + !_6!^)
echo !_7!
if BYVoid EQU BYVoid (
  set /a _8=1
) else (
  set /a _8=0
)
echo !_8!
