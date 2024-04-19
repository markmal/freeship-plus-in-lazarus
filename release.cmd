setlocal enableextensions
for /F "tokens=*" %%V IN ('Utils\versinfo.exe --FileFullVersion --NoVerbose FreeShip.exe') do (SET VERS=%%V)
echo %VERS%
cd instpkg\wix\
cmd.exe /C make_installer.cmd
cd ..\..\
move instpkg\wix\FreeShip_x64.msi Releases\"FreeShip-%VERS%_x64.msi"
