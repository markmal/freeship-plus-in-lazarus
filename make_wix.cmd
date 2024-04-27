set WIX="C:\Program Files (x86)\WiX Toolset v3.11\bin"
set PKGNAME=FreeShip_x64

del %PKGNAME%.wixobj
del %PKGNAME%.msi

%WIX%\candle -dSrcPath="C:\Users\mmalakanov\Documents\My\freeship-plus-in-lazarus" instpkg\wix\%PKGNAME%.wxs
%WIX%\light -ext WixUIExtension -cultures:en-us %PKGNAME%.wixobj
copy %PKGNAME%.msi C:\D\