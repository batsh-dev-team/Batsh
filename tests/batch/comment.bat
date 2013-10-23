@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

set /a a=3
:: This is comment 1
set /a a=^(!a! * 5^)
:: This is comment 2
echo !a!
::This is comment 3
