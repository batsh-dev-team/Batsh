@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

set /a _0=^(1 + ^(^(4 + 6^) * 3^)^)
set a=Value: !_0!
echo !a!
set /a b=^(3 + 4^)
echo !b!
set c=!a!
echo !c!
set d=!b!!c!
echo !d!
