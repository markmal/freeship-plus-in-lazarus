@echo off
echo "Installation FreeShip into user home"
set HOME=%USERPROFILE%
set FS_HOME=%HOME%\FreeShip
set FS_APP=%APPDATA%\FreeShip
set FS_LAPP=%LOCALAPPDATA%\FreeShip
set FS_BIN=%FS_HOME%

if not exist %FS_HOME% mkdir %FS_HOME%
if not exist %FS_APP% mkdir %FS_APP%
if not exist %FS_LAPP% mkdir %FS_LAPP%
if not exist %FS_BIN% mkdir %FS_BIN%

echo "  Install executables"
rem copy executable
xcopy /Y /H FreeShip.exe %FS_BIN%\
xcopy /Y /E /H *.dll %FS_BIN%\

powershell get-date -format "{yyyy-MM-dd_HHmmss}" > %TEMP%/dt
set /p DT=<%TEMP%/dt
if exist %FS_APP%\Ships  move %FS_APP%\Ships %FS_APP%\Ships_%DT%.bkp

if not exist %FS_APP%\Exec mkdir %FS_APP%\Exec
if not exist %FS_APP%\Export mkdir %FS_APP%\Export
if not exist %FS_APP%\Import mkdir %FS_APP%\Import
if not exist %FS_APP%\Ships  mkdir %FS_APP%\Ships
if not exist %FS_APP%\Temp   mkdir %FS_APP%\Temp
if not exist %FS_APP%\Themes mkdir %FS_APP%\Themes

rem these files are needed for uninstall
xcopy /Y /H uninstall-user.cmd %FS_APP%\
rem it is for linux
rem xcopy /Y /H /E install %FS_APP%\

xcopy /Y /E /H /I Exec %FS_APP%\Exec
xcopy /Y /E /H /I Languages %FS_APP%\Languages
xcopy /Y /E /H /I Manuals %FS_APP%\Manuals
xcopy /Y /E /H /I Ships %FS_APP%\Ships
xcopy /Y /E /H /I Themes %FS_APP%\Themes

xcopy /Y /H "GNU General Public License (GPL).txt" %FS_APP%\
xcopy /Y /H Whatsnew.txt %FS_APP%\
xcopy /Y /H install-HOWTO.txt %FS_APP%\
xcopy /Y /H copyright %FS_APP%\


rem save original ships lists that will be used when uninstall-user.sh executed to remove only files that were originally installed,
rem preserving files created or modified by user
rem original-ships.md5 is list of files with their md5 checksums. It is created with this command

pushd %FS_APP%\Ships\

rem find . -type f -exec md5sum /{\} /; > .original-ships.md5
rem we do md5sum in PowerShell. PowerShell scripts are disabled by default. We trick it by assembling script into one command.
set PSCMD="$md5 = New-Object -TypeName System.Security.Cryptography.MD5CryptoServiceProvider;
set PSCMD=%PSCMD% Get-ChildItem -Recurse . | ForEach-Object { if( -not $_.PSIsContainer){
set PSCMD=%PSCMD% $hash = [System.BitConverter]::ToString($md5.ComputeHash([System.IO.File]::ReadAllBytes( $_.FullName )));
set PSCMD=%PSCMD% Write-Host $hash $_.FullName } }"

powershell -Command %PSCMD% >.original-ships.md5 2>NUL
attrib +r +h .original-ships.md5

rem save original dirs
rem find . -type d | grep -v '^.$' > .original-ship-dirs.lst
dir /b /ad /s > .original-ship-dirs.lst
attrib +r +h .original-ship-dirs.lst

popd

echo "  Install configuration"
set CFG=%FS_LAPP%\FreeShip.ini

if not exist %CFG% goto CFGNOTEXIST
  echo Configuration file %CFG% already exits.
  set /P CHANGE="Whould you like to change shared directories to user ones? [Y\N]: "
  
  if %CHANGE% == "Y" (
    xcopy /Y %CFG% %CFG%.0
    echo Old configuration is backed up to %CFG%.0

    SETLOCAL ENABLEEXTENSIONS
    SETLOCAL DISABLEDELAYEDEXPANSION

	rem we do regex replacement in PowerShell. PowerShell scripts are disabled by default. We trick it with assembling script into command.
	set PSCMD="${FSAPPD}='%FS_APP%';
	set PSCMD=%PSCMD% Get-Content %CFG% | ForEach-Object {
	set PSCMD=%PSCMD%  $newstr = $_ -replace '(LanguagesDirectory)\S*=\S*(.*)\S*$', ('${1}='+${FSAPPD}+'\Languages');
	set PSCMD=%PSCMD%  $newstr = $newstr -replace '(ExecDirectory)\S*=\S*(.*)\S*$', ('${1}='+${FSAPPD}+'\Exec');
	set PSCMD=%PSCMD%  $newstr = $newstr -replace '(ManualsDirectory)\S*=\S*(.*)\S*$', ('${1}='+${FSAPPD}+'\Manuals');
	set PSCMD=%PSCMD%  Write-Host $newstr;
	set PSCMD=%PSCMD% }"
	powershell -command %PSCMD% >%CFG% 2>NUL
  )
goto CFGEXISTEND
:CFGNOTEXIST
  echo [Directories] >%CFG%
  echo ExportDirectory=%FS_HOME%\Export >>%CFG%
  echo LanguagesDirectory=%FS_APP%\Languages >>%CFG%
  echo ExecDirectory=%FS_APP%\Exec >>%CFG%
  echo ManualsDirectory=%FS_APP%\Manuals >>%CFG%
  echo TempDirectory=%FS_HOME%\Temp >>%CFG%
  echo OpenDirectory=%FS_APP%\Ships >>%CFG%
  echo SaveDirectory=%FS_HOME%\Ships >>%CFG%
  echo ImportDirectory=%FS_APP%\Import >>%CFG%
:CFGEXISTEND

rem Make menu item and MIME types

set PSCMD="$objShell=New-Object -ComObject ('WScript.Shell');
set PSCMD=%PSCMD% $Startup=$objShell.SpecialFolders.Item('Startup');
set PSCMD=%PSCMD% mkdir ($Startup+'\FreeShip');
set PSCMD=%PSCMD% $objShortCut=$objShell.CreateShortcut($objShell.SpecialFolders.Item('Startup')+'\FreeShip\FreeShip.lnk');
set PSCMD=%PSCMD% $objShortCut.TargetPath=$env:USERPROFILE+'\FreeShip\FreeShip.exe';
set PSCMD=%PSCMD% $objShortCut.Description='FreeShip in Lazarus';
set PSCMD=%PSCMD% $objShortCut.WorkingDirectory=$env:USERPROFILE+'\FreeShip';
set PSCMD=%PSCMD% $objShortCut.Save()"
echo %PSCMD%
powershell -Command %PSCMD%

rem register FreeShip
set PSCMD="New-Item -Path 'hkcu:\Software\Classes\Applications\FreeShip.exe';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\Applications\FreeShip.exe\shell';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\Applications\FreeShip.exe\shell\open';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\Applications\FreeShip.exe\shell\open\command';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Classes\Applications\FreeShip.exe\shell\open\command' -Name '(Default)' -Value '%USERPROFILE%\FreeShip\FreeShip.exe %1'"
powershell -command %PSCMD%

rem register .fbm association
set PSCMD="New-Item -Path 'hkcu:\Software\Classes\.fbm';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Classes\.fbm' -Name '(Default)' -Value 'fbm_auto_file'"
powershell -command %PSCMD%

set PSCMD="New-Item -Path 'hkcu:\Software\Classes\fbm_auto_file';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\fbm_auto_file\shell';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\fbm_auto_file\shell\open';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\fbm_auto_file\shell\open\command';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Classes\fbm_auto_file\shell\open\command' -Name '(Default)' -Value '%USERPROFILE%\FreeShip\FreeShip.exe %1'"
powershell -command %PSCMD%

set PSCMD="New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm\OpenWithList';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm\OpenWithList' -Name 'a' -Value 'FreeShip.exe';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm\OpenWithList' -Name 'MRUList' -Value 'a';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm\OpenWithProgids';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.fbm\OpenWithProgids' -Name 'fbm_auto_file'"
powershell -command %PSCMD%

rem register .ftm association
set PSCMD="New-Item -Path 'hkcu:\Software\Classes\.ftm';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Classes\.ftm' -Name '(Default)' -Value 'ftm_auto_file'"
powershell -command %PSCMD%

set PSCMD="New-Item -Path 'hkcu:\Software\Classes\ftm_auto_file';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\ftm_auto_file\shell';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\ftm_auto_file\shell\open';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Classes\ftm_auto_file\shell\open\command';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Classes\ftm_auto_file\shell\open\command' -Name '(Default)' -Value '%USERPROFILE%\FreeShip\FreeShip.exe %1'"
powershell -command %PSCMD%

set PSCMD="New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm\OpenWithList';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm\OpenWithList' -Name 'a' -Value 'FreeShip.exe';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm\OpenWithList' -Name 'MRUList' -Value 'a';
set PSCMD=%PSCMD% New-Item -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm\OpenWithProgids';
set PSCMD=%PSCMD% New-ItemProperty -Path 'hkcu:\Software\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.ftm\OpenWithProgids' -Name 'ftm_auto_file'"
powershell -command %PSCMD%

rem restart explorer
taskkill /f /fi "imagename eq explorer.exe"
START explorer.exe

:FINAL