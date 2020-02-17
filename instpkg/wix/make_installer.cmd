setlocal
set WIX="C:\Program Files (x86)\WiX Toolset v3.11\bin"
set PATH=%WIX%;%PATH%

del *.wixobj *.wixpdb
del FreeShip_x64.msi

set SRCPATH=C:\Users\mmalakanov\Documents\My\freeship-plus-in-lazarus

candle.exe -v -pedantic -arch x64 -dSrcPath="%SRCPATH%" FreeShip_x64.wxs -dLanguages="%SRCPATH%\Languages" languages.wxs -dlocale="%SRCPATH%\locale" locale.wxs -dManuals="%SRCPATH%\Manuals" manuals.wxs -dShips="%SRCPATH%\Ships" ships.wxs -dThemes="%SRCPATH%\Themes" themes.wxs -dExec="%SRCPATH%\Exec" exec.wxs 

light.exe -ext WixUIExtension -cultures:en-us -dWixUILicenseRtf="%SRCPATH%\Manuals\gpl-3.0.rtf" FreeShip_x64.wixobj languages.wixobj locale.wixobj manuals.wixobj ships.wixobj themes.wixobj exec.wixobj -out FreeShip_x64.msi

rem copy FreeShip_x64.msi C:\D\

endlocal