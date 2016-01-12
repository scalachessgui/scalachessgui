
rem set /p dirname="Enter directory to be deleted: "

del /s /f /q %1\*.*

for /f %%f in ('dir /ad /b %1\') do rd /s /q %1\%%f

rd %1

