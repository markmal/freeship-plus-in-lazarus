program versinfo;
// Getting file version information
// slightly modified from fcl-res help file program res3 (mainly comments).
// This program reads a supported resource file (and autodetects the resource type)
// and prints out version info
//{$mode objfpc}
 
uses
  Classes, SysUtils, resource, versionresource, versiontypes,
// You need to specify which resources you want to be able to read.
// The units initialize themselves and register themselves as readers....
  resreader, coffreader, elfreader, winpeimagereader,
  elfconsts;
 
type
  TOutEnum = (
    osFileFullVersion, osFileMajorVersion, osFileMinorVersion,
    osFileRelease, osFileBuild,
    osProductFullVersion, osProductMajorVersion, osProductMinorVersion,
    osProductRelease, osProductBuild);
  TOutSet = set of TOutEnum;
var
  resources : TResources;
  inFile : TFileStream;
  reader : TAbstractResourceReader;
  ar : TAbstractResource;
  vr : TVersionResource;
  fi : TVersionFixedInfo;
  F,I,RC : integer;
  outset : TOutSet;
  p : string;
  inFileNames : TStringList;
  verbose: boolean;

  procedure printHelp;
  begin
  writeln('versinfo - Extracts version info from binary executables');
  writeln('Usage: versinfo [param] [param] ... [param]');
  writeln('  where param is one of');
  writeln('  --FileFullVersion');
  writeln('  --FileMajorVersion');
  writeln('  --FileMinorVersion');
  writeln('  --FileRelease');
  writeln('  --FileBuild');
  writeln('  --ProductFullVersion');
  writeln('  --ProductMajorVersion');
  writeln('  --ProductMinorVersion');
  writeln('  --ProductRelease');
  writeln('  --ProductBuild');
  writeln('  --NoVerbose');
  writeln('  --help');
  writeln('  <file name>');
  writeln('Note: At least one file name must be specified');
  writeln('      All will printed all when no options specified');
  writeln('      NoVebose prints just value(s) without name(s)');
  end;

begin
  if paramcount = 0 then printHelp;
  outset := [];
  inFileNames:=TStringList.Create;
  verbose := true;

  for I:=1 to paramcount do
    begin
    p:=paramstr(I);
    if      p='--FileFullVersion' then outset := outset + [osFileFullVersion]
    else if p='--FileMajorVersion' then outset := outset + [osFileMajorVersion]
    else if p='--FileMinorVersion' then outset := outset + [osFileMinorVersion]
    else if p='--FileRelease' then outset := outset + [osFileRelease]
    else if p='--FileBuild' then outset := outset + [osFileBuild]
    else if p='--ProductFullVersion' then outset := outset + [osProductFullVersion]
    else if p='--ProductMajorVersion' then outset := outset + [osProductMajorVersion]
    else if p='--ProductMinorVersion' then outset := outset + [osProductMinorVersion]
    else if p='--ProductRelease' then outset := outset + [osProductRelease]
    else if p='--ProductBuild' then outset := outset + [osProductBuild]
    else if p='--NoVerbose' then verbose := false
    else if p='--help' then begin printHelp; exit; end
    else inFileNames.Add(p);
    end;
  if inFileNames.Count=0 then
    begin writeln('File name not specified');
    exitCode:=1; exit;
    end;
  if outset = [] then outset:=[osFileFullVersion, osFileMajorVersion, osFileMinorVersion,
    osFileRelease, osFileBuild,
    osProductFullVersion, osProductMajorVersion, osProductMinorVersion,
    osProductRelease, osProductBuild];

  for F:=0 to inFileNames.Count-1 do
    begin
    inFile:=TFileStream.Create(inFileNames[F], fmOpenRead or fmShareDenyNone);
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

           if osFileFullVersion in outset then
             begin
               if verbose then write('File Full Version:' +#9);
               writeln(IntToStr(fi.FileVersion[0])
                              + '.' + IntToStr(fi.FileVersion[1])
                              + '.' + IntToStr(fi.FileVersion[2])
                              + '.' + IntToStr(fi.FileVersion[3]));
             end;

           if osFileMajorVersion in outset then
             begin
               if verbose then write('File Major Version:' +#9);
               writeln(IntToStr(fi.FileVersion[0]));
             end;

           if osFileMinorVersion in outset then
             begin
               if verbose then write('File Minor Version:' +#9);
               writeln(IntToStr(fi.FileVersion[1]));
             end;

           if osFileRelease in outset then
             begin
               if verbose then write('File Release:' +#9);
               writeln(IntToStr(fi.FileVersion[2]));
             end;

           if osFileBuild in outset then
             begin
               if verbose then write('File Build:' +#9);
               writeln(IntToStr(fi.FileVersion[3]));
             end;


           if osProductFullVersion in outset then
             begin
               if verbose then write('Product Full Version:' +#9);
               writeln(IntToStr(fi.ProductVersion[0])
                              + '.' + IntToStr(fi.ProductVersion[1])
                              + '.' + IntToStr(fi.ProductVersion[2])
                              + '.' + IntToStr(fi.ProductVersion[3]));
             end;

           if osProductMajorVersion in outset then
             begin
               if verbose then write('Product Major Version:' +#9);
               writeln(IntToStr(fi.ProductVersion[0]));
             end;

           if osProductMinorVersion in outset then
             begin
               if verbose then write('Product Minor Version:' +#9);
               writeln(IntToStr(fi.ProductVersion[1]));
             end;

           if osProductRelease in outset then
             begin
               if verbose then write('Product Release:' +#9);
               writeln(IntToStr(fi.ProductVersion[2]));
             end;

           if osProductBuild in outset then
             begin
               if verbose then write('Product Build:' +#9);
               writeln(IntToStr(fi.ProductVersion[3]));
             end;

         end;
        finally
        end;
      end;

    // clean up - adding try..except blocks would be nice:
    resources.Free;
    reader.Free;
    inFile.Free;
    end;

end.

