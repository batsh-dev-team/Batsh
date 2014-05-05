@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

set /a _0=10
call :loop _5 0 _0
echo | set /p ^=!_5!
set /a _1=5
call :fact _6 0 _1
set _2=!_6!
echo !_2!
set /a i=0
:WHILE_8
if !i! LSS 7 (
  set _3=!i!
  call :fibonacci _7 0 _3
  set _4=!_7!
  echo !_4!
  set /a i=^(!i! + 1^)
  goto WHILE_8
)

goto :EOF
:loop
set num_%~2=!%~3!
echo !num_%~2!
if !num_%~2! GTR 0 (
  set /a _0_%~2=^(!num_%~2! - 1^)
  set /a _1_%~2=^(1 + %~2^)
  call :loop _2_%~2 !_1_%~2! _0_%~2
  echo | set /p ^=!_2_%~2!
)

goto :EOF
:fact
set num_%~2=!%~3!
if !num_%~2! EQU 0 (
  set %~1=1
  goto :EOF
) else (
  set /a _0_%~2=^(!num_%~2! - 1^)
  set /a _3_%~2=^(1 + %~2^)
  call :fact _4_%~2 !_3_%~2! _0_%~2
  set _1_%~2=!_4_%~2!
  set /a _2_%~2=^(!_1_%~2! * !num_%~2!^)
  set %~1=!_2_%~2!
  goto :EOF
)

goto :EOF
:fibonacci
set num_%~2=!%~3!
if !num_%~2! EQU 0 (
  set %~1=0
  goto :EOF
) else (
  if !num_%~2! EQU 1 (
    set %~1=1
    goto :EOF
  ) else (
    set /a _0_%~2=^(!num_%~2! - 2^)
    set /a _5_%~2=^(1 + %~2^)
    call :fibonacci _6_%~2 !_5_%~2! _0_%~2
    set _1_%~2=!_6_%~2!
    set /a _2_%~2=^(!num_%~2! - 1^)
    set /a _7_%~2=^(1 + %~2^)
    call :fibonacci _8_%~2 !_7_%~2! _2_%~2
    set _3_%~2=!_8_%~2!
    set /a _4_%~2=^(!_1_%~2! + !_3_%~2!^)
    set %~1=!_4_%~2!
    goto :EOF
  )
)
