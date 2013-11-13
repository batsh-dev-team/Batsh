@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

set a_0=
set a_1=y
set /a a_2=-1
set /a a_3=1
set /a a_0=^(2 * 9^)
set a_2=abx
set a_4=5!a_0!
echo !a_0! !a_1! !a_2! !a_3! !a_4!
set /a a_0=1
set /a a_1=2
set /a a_2=3
echo !a_0! !a_1! !a_2!
set _0=10!a_0!
set /a _1=^(!_0! * 2^)
echo !_1!
for /f "delims=" %%i in ('len !a!') do set _2=%%i
echo !_2!
for /f "delims=" %%i in ('len !a!') do set _3=%%i
set /a _4=^(!_3! * 8^)
echo !_4!
rem println([1, 2, 3]);
