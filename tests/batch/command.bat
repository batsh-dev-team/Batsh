@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

set _0=Println Called
echo !_0!
set cmd=echo
set _1=Echo Called
!cmd! !_1!
set _2=Value 100%%
for /f "delims=" %%i in ('echo !_2!') do set retval=%%i
echo !retval!
