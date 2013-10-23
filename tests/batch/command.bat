@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

echo Println Called
set cmd=echo
!cmd! Echo Called
expr 36 + 6
set _0=!_!
echo !_0!
