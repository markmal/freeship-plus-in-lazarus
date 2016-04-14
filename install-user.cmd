@echo off
echo "Installation FreeShip into user home"
set HOME=%USERPROFILE%
set FS_HOME=%HOME%/FreeShip
set FS_APP=%APPDATA%/FreeShip
set FS_LAPP=%LOCALAPPDATA%/FreeShip
set FS_BIN=%FS_HOME%

if not exist %FS_HOME% mkdir %FS_HOME%
if not exist %FS_APP% mkdir %FS_APP%
if not exist %FS_LAPP% mkdir %FS_LAPP%
if not exist %FS_BIN% mkdir %FS_BIN%

echo "  Install executables"
rem copy executable
xcopy /Y /H FreeShip.exe %FS_BIN%/
xcopy /Y /E /H *.dll %FS_BIN%/

if not exist %FS_APP%/Export mkdir %FS_APP%/Export
if not exist %FS_APP%/Import mkdir %FS_APP%/Import
if not exist %FS_APP%/Ships  mkdir %FS_APP%/Ships
if not exist %FS_APP%/Temp   mkdir %FS_APP%/Temp
if not exist %FS_APP%/Themes mkdir %FS_APP%/Themes

rem these files are needed for uninstall
copy uninstall-user.cmd %FS_APP%/
copy -r install %FS_APP%/

if not exist %FS_HOME%/Exec ] mkdir %FS_APP%/Exec

xcopy /Y /E /H Languages %FS_APP%/
xcopy /Y /E /H Manuals %FS_APP%/
xcopy /Y /E /H Ships %FS_APP%/
xcopy /Y /E /H Themes %FS_APP%/
xcopy /Y /E /H install %FS_APP%/

copy "GNU General Public License (GPL).txt" %FS_APP%/

xcopy /Y /H Whatsnew.txt %FS_APP%/
xcopy /Y /H uninstall-user.sh %FS_APP%/
xcopy /Y /H install-HOWTO.txt %FS_APP%/
xcopy /Y /H copyright %FS_APP%/


rem save original ships lists that will be used when uninstall-user.sh executed to remove only files that were originally installed,
rem preserving files created or modified by user
rem original-ships.md5 is list of files with their md5 checksums. It is created with this command

cd Ships/

rem find . -type f -exec md5sum \{\} \; > .original-ships.md5
rem we do md5sum in PowerShell. PowerShell scripts are disabled by default. We trick it by assembling script into one command.
set PSCMD="$md5 = New-Object -TypeName System.Security.Cryptography.MD5CryptoServiceProvider;
set PSCMD=%PSCMD% Get-ChildItem -Recurse . | ForEach-Object { 
set PSCMD=%PSCMD% $hash = [System.BitConverter]::ToString($md5.ComputeHash([System.IO.File]::ReadAllBytes( $_.FullName )));
set PSCMD=%PSCMD% Write-Host $hash $_.FullName }"

powershell -Command %PSCMD% >.original-ships.md5 2>NUL

rem save original dirs
rem find . -type d | grep -v '^.$' > .original-ship-dirs.lst
dir /b /ad /s > .original-ship-dirs.lst

cd ..

echo "  Install configuration"
set CFG=%FS_LAPP%/FreeShip.ini

if not exist %CFG% goto CFGNOTEXIST
  echo Configuration file %CFG% already exits.
  set /P CHANGE="Whould you like to change shared directories to user ones? [Y/N]: "
  
  if %CHANGE% == "Y" (
    copy %CFG% %CFG%.0
    echo Old configuration is backed up to %CFG%.0

    SETLOCAL ENABLEEXTENSIONS
    SETLOCAL DISABLEDELAYEDEXPANSION

    for /f "tokens=1,* delims=]" %%A in ('"type %CFG.0|find /n /v """') do (
      set "line=%%B"
      if defined line (
        call set "line=echo.%%line:%~1=%~2%%"
        for /f "delims=" %%X in ('"echo."%%line%%""') do %%~X
      ) ELSE echo.
    )	

	rem we do regex replacement in PowerShell. PowerShell scripts are disabled by default. We trick it with assembling script into command.
	set PSCMD="${FSAPPD}=%FSAPPD%;
	set PSCMD=%PSCMD% Get-Content FreeShip.ini | ForEach-Object {
	set PSCMD=%PSCMD%  $newstr = $_ -replace '(LanguagesDirectory)\S*=\S*(.*)\S*$', ('${1}='+${FSAPPD}+'/Languages');
	set PSCMD=%PSCMD%  $newstr = $newstr -replace '(ExecDirectory)\S*=\S*(.*)\S*$', ('${1}='+${FSAPPD}+'/Exec');
	set PSCMD=%PSCMD%  $newstr = $newstr -replace '(ManualsDirectory)\S*=\S*(.*)\S*$', ('${1}='+${FSAPPD}+'/Manuals');
	set PSCMD=%PSCMD%  Write-Host $newstr;
	set PSCMD=%PSCMD% }"
	powershell -command %PSCMD% >%CFG% 2>NUL
  )
goto CFGEXISTEND
:CFGNOTEXIST
  echo [Directories] >%CFG%
  echo LanguagesDirectory=%FS_APP%/Languages >>%CFG%
  echo ExecDirectory=%FS_APP%/Exec >>%CFG%
  echo ManualsDirectory=%FS_APP%/Manuals >>%CFG%
  echo TempDirectory=%FS_HOME%/Temp >>%CFG%
  echo OpenDirectory=%FS_APP%/Ships >>%CFG%
  echo SaveDirectory=%FS_HOME%/Ships >>%CFG%
  echo ImportDirectory=%FS_APP%/Import >>%CFG%
  echo ExportDirectory=%FS_HOME%/Export >>%CFG%
:CFGEXISTEND

rem Make menu item and MIME types

$objShell = New-Object -ComObject ("WScript.Shell");
echo $objShell.SpecialFolders.Item("StartMenu");
echo $objShell.SpecialFolders.Item("Startup");


$objShell = New-Object -ComObject ("WScript.Shell");
$Startup=$objShell.SpecialFolders.Item("Startup");
mkdir ($Startup+"\FreeShip");
$objShortCut = $objShell.CreateShortcut($objShell.SpecialFolders.Item("Startup")+"\FreeShip\FreeShip.lnk");
$objShortCut.TargetPath=$env:USERPROFILE + "\FreeShip\FreeShip.exe";
$objShortCut.Description='FreeShip in Lazarus';
$objShortCut.WorkingDirectory=$env:USERPROFILE + "\FreeShip";
$objShortCut.Save();

goto AVOID
Remove-Item 'hkcu:\Software\Classes\Applications\FreeShip.exe' -Recurse;
Remove-Item 'hkcu:\Software\Classes\.fbm' -Recurse;
Remove-Item 'hkcu:\Software\Classes\fbm_auto_file' -Recurse;
Remove-Item 'hkcu:\software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm' -Recurse;
Remove-Item 'hkcu:\Software\Classes\.ftm' -Recurse;
Remove-Item 'hkcu:\Software\Classes\ftm_auto_file' -Recurse;
Remove-Item 'hkcu:\software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm' -Recurse;
:AVOID

rem register FreeShip
set PSCMD=New-Item -Path 'hkcu:\Software\Classes\Applications\FreeShip.exe';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\Applications\FreeShip.exe\shell';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\Applications\FreeShip.exe\shell\open';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\Applications\FreeShip.exe\shell\open\command';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Classes\Applications\FreeShip.exe\shell\open\command' -Name '(Default)' -Value '"%USERPROFILE%\FreeShip\FreeShip.exe" "%1"';
powershell -command %PSCMD%

rem register .fbm association
set PSCMD=New-Item -Path 'hkcu:\Software\Classes\.fbm';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Classes\.fbm' -Name '(Default)' -Value 'fbm_auto_file';
powershell -command %PSCMD%

set PSCMD=New-Item -Path 'hkcu:\Software\Classes\fbm_auto_file';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\fbm_auto_file\shell';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\fbm_auto_file\shell\open';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\fbm_auto_file\shell\open\command';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Classes\fbm_auto_file\shell\open\command' -Name '(Default)' -Value '"%USERPROFILE%\FreeShip\FreeShip.exe" "%1"';
powershell -command %PSCMD%

set PSCMD=New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm\OpenWithList';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm\OpenWithList' -Name 'a' -Value 'FreeShip.exe';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm\OpenWithList' -Name 'MRUList' -Value 'a';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm\OpenWithProgids';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm\OpenWithProgids' -Name 'fbm_auto_file';
powershell -command %PSCMD%

rem register .ftm association
set PSCMD=New-Item -Path 'hkcu:\Software\Classes\.ftm';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Classes\.ftm' -Name '(Default)' -Value 'ftm_auto_file';
powershell -command %PSCMD%

set PSCMD=New-Item -Path 'hkcu:\Software\Classes\ftm_auto_file';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\ftm_auto_file\shell';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\ftm_auto_file\shell\open';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\ftm_auto_file\shell\open\command';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Classes\ftm_auto_file\shell\open\command' -Name '(Default)' -Value '"%USERPROFILE%\FreeShip\FreeShip.exe" "%1"';
powershell -command %PSCMD%

set PSCMD=New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm\OpenWithList';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm\OpenWithList' -Name 'a' -Value 'FreeShip.exe';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm\OpenWithList' -Name 'MRUList' -Value 'a';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm\OpenWithProgids';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm\OpenWithProgids' -Name 'ftm_auto_file';
powershell -command %PSCMD%

rem restart explorer
taskkill /f /fi "imagename eq explorer.exe"
START explorer.exe
