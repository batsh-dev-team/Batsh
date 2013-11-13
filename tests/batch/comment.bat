@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

set /a a=3
rem This is comment 1
set /a a=^(!a! * 5^)
rem This is comment 2
echo !a!
rem This is comment 3
