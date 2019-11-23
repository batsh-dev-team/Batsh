@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

set /a i=0
:WHILE_0
if !i! LSS 5 (
  echo | set /p ^=!i! 
  set /a i=^(!i! + 1^)
  goto WHILE_0
)
echo:
rem Fibonacci
set /a n=0
set /a i=0
set /a j=1
:WHILE_1
if !n! LSS 40 (
  set /a k=^(!i! + !j!^)
  set i=!j!
  set j=!k!
  set /a n=^(!n! + 1^)
  echo !k!
  goto WHILE_1
)
