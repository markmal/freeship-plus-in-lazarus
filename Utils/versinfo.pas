program versinfo;
//Getting file version information
// slightly modified from fcl-res help file program res3 (mainly comments).
// This program reads a supported resource file (and autodetects the resource type)
// and prints out version info
{$mode objfpc}
 
uses
  Classes, SysUtils, resource, versionresource, versiontypes,
// You need to specify which resources you want to be able to read.
// The units initialize themselves and register themselves as readers....
  resreader, coffreader, elfreader, winpeimagereader,
  elfconsts;
 
var
  resources : TResources;
  inFile : TFileStream;
  reader : TAbstractResourceReader;
  ar : TAbstractResource;
  vr : TVersionResource;
  fi : TVersionFixedInfo;
  fv : TFileProductVersion;
  I,RC : integer;
  fos : longword;
begin
  inFile:=TFileStream.Create(paramstr(1), fmOpenRead or fmShareDenyNone);
  resources := TResources.Create;
  reader:=resources.FindReader(inFile);
  resources.LoadFromStream(inFile,reader);

  RC := resources.Count;
  for I:=0 to RC-1 do
  begin
    try
     ar := resources[I];
     if ar is TVersionResource then
     begin
       vr := TVersionResource(ar);
       fi:= vr.FixedInfo;
       writeln('File Full Version:' +#9+ IntToStr(fi.FileVersion[0])
                          + '.' + IntToStr(fi.FileVersion[1])
                          + '.' + IntToStr(fi.FileVersion[2])
                          + '.' + IntToStr(fi.FileVersion[3]));
       writeln('File Major Version:' +#9+ IntToStr(fi.FileVersion[0]));
       writeln('File Minor Version:' +#9+ IntToStr(fi.FileVersion[1]));
       writeln('File Release:' +#9+ IntToStr(fi.FileVersion[2]));
       writeln('File Build:' +#9+ IntToStr(fi.FileVersion[3]));

       writeln('Product Full Version:' +#9+ IntToStr(fi.ProductVersion[0])
                          + '.' + IntToStr(fi.ProductVersion[1])
                          + '.' + IntToStr(fi.ProductVersion[2])
                          + '.' + IntToStr(fi.ProductVersion[3]));
       writeln('Product Major Version:' +#9+ IntToStr(fi.ProductVersion[0]));
       writeln('Product Minor Version:' +#9+ IntToStr(fi.ProductVersion[1]));
       writeln('Product Release:' +#9+ IntToStr(fi.ProductVersion[2]));
       writeln('Product Build:' +#9+ IntToStr(fi.ProductVersion[3]));
     end;
    finally
    end;
  end;

  // clean up - adding try..except blocks would be nice:
  resources.Free;
  reader.Free;
  inFile.Free;
end.

