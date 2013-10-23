@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

set _0_0=
set _0_1=y
set /a _0_2=-1
set /a _0_3=1
set a=!_0!
set /a a_0=^(2 * 9^)
set a_2=abx
set a_5=5!a_0!
echo !a_0! !a_1! !a_2! !a_3! !a_4! !a_5!
set /a _1_0=1
set /a _1_1=2
set /a _1_2=3
set a=!_1!
echo !a_0! !a_1! !a_2!
set _2=10!a_0!
set /a _3=^(!_2! * 2^)
echo !_3!
len !a!
set _4=!_!
echo !_4!
len !a!
set _5=!_!
set /a _6=^(!_5! * 8^)
echo !_6!
::println([1, 2, 3]);
