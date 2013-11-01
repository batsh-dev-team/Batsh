@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

set _0=Println Called
echo !_0!
set cmd=echo
set _1=Echo Called
!cmd! !_1!
expr 36 + 6
set _2=!_!
echo !_2!
