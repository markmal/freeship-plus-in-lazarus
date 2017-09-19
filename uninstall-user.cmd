@echo off
echo Complete uninstallation FreeShip from user home
set HOME=%USERPROFILE%
set FS_HOME=%HOME%\FreeShip
set FS_APP=%APPDATA%\FreeShip
set FS_LAPP=%LOCALAPPDATA%\FreeShip
set FS_BIN=%FS_HOME%

echo ATTENTION: this Uninstaller will not delete your Ships folder:
echo %FS_APP%/Ships
echo. 
echo Following files and folders will be deleted:
echo %FS_BIN%\
echo %FS_HOME%\
echo %FS_LAPP%\
echo %FS_APP%\Languages\
echo %FS_APP%\Manuals\
echo %FS_APP%\Themes\
echo %FS_APP%\install\
echo %FS_APP%\Whatsnew.txt
echo %FS_APP%\install-HOWTO.txt
echo %FS_APP%\copyright
echo %FS_APP%\"GNU General Public License (GPL).txt"

set /P CONFIRM=Please confirm (type YES): 

if %CONFIRM%==YES goto CONFRMD
echo cancelled
goto FINAL
:CONFRMD
echo confirmed

rem delete menu item and MIME types

set PSCMD="$objShell=New-Object -ComObject ('WScript.Shell');
set PSCMD=%PSCMD% $Startup=$objShell.SpecialFolders.Item('Startup');
set PSCMD=%PSCMD% Remove-Item ($Startup+'\FreeShip') -Recurse"
powershell -command %PSCMD% 2>NUL

set PSCMD="Remove-Item 'hkcu:\Software\Classes\Applications\FreeShip.exe' -Recurse;
set PSCMD=%PSCMD% Remove-Item 'hkcu:\Software\Classes\.fbm' -Recurse;
set PSCMD=%PSCMD% Remove-Item 'hkcu:\Software\Classes\fbm_auto_file' -Recurse;
set PSCMD=%PSCMD% Remove-Item 'hkcu:\software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm' -Recurse;
set PSCMD=%PSCMD% Remove-Item 'hkcu:\Software\Classes\.ftm' -Recurse;
set PSCMD=%PSCMD% Remove-Item 'hkcu:\Software\Classes\ftm_auto_file' -Recurse;
set PSCMD=%PSCMD% Remove-Item 'hkcu:\software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm' -Recurse"
powershell -command %PSCMD% 2>NUL

rmdir /S /Q %FS_BIN%\
rmdir /S /Q %FS_HOME%\
rmdir /S /Q %FS_LAPP%\
rmdir /S /Q %FS_APP%\Languages\
rmdir /S /Q %FS_APP%\Manuals\
rmdir /S /Q %FS_APP%\Themes\
rmdir /S /Q %FS_APP%\install\
del /Q /F %FS_APP%\Whatsnew.txt
del /Q /F %FS_APP%\install-HOWTO.txt
del /Q /F %FS_APP%\copyright
del /Q /F %FS_APP%\"GNU General Public License (GPL).txt"

rem restart explorer
taskkill /f /fi "imagename eq explorer.exe"
START explorer.exe

:FINAL