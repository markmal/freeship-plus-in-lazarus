program printfileinfo;
{ Displays file version info for 
- Windows PE executables 
- Linux ELF executables (compiled by Lazarus)
- OSX MACH-O executables (compiled by Lazarus)
Runs on Windows, Linux, OSX...)
}
 
{$mode objfpc}{$H+}
 
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,sysutils
  // FPC trunk fileinfo reads exe resources as long as you register the appropriate units
  , fileinfo
  , winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables}
  ;
 
var
  FileVerInfo: TFileVersionInfo;
begin
  if Paramcount=0 then
  begin
    writeln('Missing executable filename parameters. Aborting.');
    halt(1);
  end;
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName:=paramstr(1);
    FileVerInfo.ReadFileInfo;
    writeln('Company: ',FileVerInfo.VersionStrings.Values['CompanyName']);
    writeln('File description: ',FileVerInfo.VersionStrings.Values['FileDescription']);
    writeln('File version: ',FileVerInfo.VersionStrings.Values['FileVersion']);
    writeln('Internal name: ',FileVerInfo.VersionStrings.Values['InternalName']);
    writeln('Legal copyright: ',FileVerInfo.VersionStrings.Values['LegalCopyright']);
    writeln('Original filename: ',FileVerInfo.VersionStrings.Values['OriginalFilename']);
    writeln('Product name: ',FileVerInfo.VersionStrings.Values['ProductName']);
    writeln('Product version: ',FileVerInfo.VersionStrings.Values['ProductVersion']);
  finally
    FileVerInfo.Free;
  end;
end.

