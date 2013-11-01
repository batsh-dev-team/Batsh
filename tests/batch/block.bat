@echo off
setlocal EnableDelayedExpansion
setlocal EnableExtensions

::Level 0 Start
echo Hello
::Level 1 Start
echo Lo
::Level 2 Start
set _0=and behold
echo !_0!
::Level 2 End
::Level 1 End
echo End
::Level 0 End
