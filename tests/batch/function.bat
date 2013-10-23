@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

:: Function call
call :func1 _1 0 Hello World
echo | set /p ^=!_1!
:: Global and local variables
set v1=Global V1
set v2=Global V2
set v3=Global V3
call :func2 _2 0 Var
echo | set /p ^=!_2!
echo !v1!
echo !v3!
:: Return value
call :func3 _3 0 4
echo | set /p ^=!_3!
echo:
call :func3 _4 0 1
set _0=!_4!
set ret=!_0!
echo Returned: !ret!

goto :EOF
:func1
set p1_%~2=%~3
set p2_%~2=%~4
echo !p1_%~2! !p2_%~2!

goto :EOF
:func2
set p_%~2=%~3
set v1_%~2=Local !p_%~2!
echo !v1_%~2!
echo !v2!
set v3=V3 Modified.

goto :EOF
:func3
set num_%~2=%~3
set /a _0_%~2=^(!num_%~2! + 41^)
set %~1=!_0_%~2!
goto :EOF
