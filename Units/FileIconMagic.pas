unit FileIconMagic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, Forms, //LookupStringList,
  FileIcon,
  magic4lazarus;

type TFileMimeIcon = class(TFileIcon)
  FMgc:string; //path to magic.mgc file
  FMagic:TMagic;
  FTheme:string;
  FSize:integer;
  FGlobs:TStringList;
  FMimeIconNames: TStringList;
protected
  function searchGlobs(pattern:string):string;
  function getGlobsMimeForFile(filename:string):string;
  procedure findMagicMgc;
  function loadIconFromFile(ifilename:string):TIcon;
  procedure loadGlobs;
  procedure loadMimeIconNames;
public
  constructor Create();
  function getMimeTypeForFile(filename:string):string;
  function getMimeToIconName(mimeType:string):string;
  function getIconForFileMime( filename:string; size:integer;
                           var mimeType:string; var iconName:string):TIcon;

  function getIconByName(iconName:string; size:integer):TIcon; override;
  function getIconNameForFile(filename:string; size:integer):string; override;
  function getIconForFile(filename:string; size:integer):TIcon; override;
end;

function split(Input: string; const Delimiter: Char):TStringList;

implementation

constructor TFileMimeIcon.Create();
begin
  FMagic:=TMagic.Create;
  FTheme:='Default';
  FSize:=16;
  findMagicMgc;
  loadGlobs;
  loadMimeIconNames;
end;

procedure TFileMimeIcon.findMagicMgc;
var p:string;
begin
  p := GetEnvironmentVariable('MAGIC') + '/magic.mgc';
  if FileExists(p) then FMgc:=p
  else
    begin
    p := ExtractFilePath(Application.ExeName) + 'magic.mgc';
    if FileExists(p) then FMgc:=p
    else
      begin
        p:= ExtractFilePath(Application.ExeName) + 'lib/magic.mgc';
        if FileExists(p) then FMgc:=p
      else
        begin
          p:= '/usr/share/misc/magic.mgc';
          if FileExists(p) then FMgc:=p
        else
          begin
            p:= '/usr/share/file/magic.mgc';
            if FileExists(p) then FMgc:=p
          end;
        end;
      end;
    end;
end;

function TFileMimeIcon.loadIconFromFile(ifilename:string):TIcon;
var icon:TIcon; pic:TPicture;
begin
  result := nil;
  if FileExists(ifilename) then
    begin
    pic:=TPicture.Create;
    pic.LoadFromFile(ifilename);
    icon:=TIcon.Create;
    icon.Assign(pic.Icon);
    result:=icon;
    end;
end;


function TFileMimeIcon.getIconForFileMime(filename:string; size:integer;
           var mimeType:string; var iconName:string):TIcon;
var icon:TIcon;
begin
  result := nil;
  //mime := DetectMimeFromFile(filename, FMgc);
  mimeType:= DetectMimeTypeFromFile(filename, FMgc);
  iconName:= getMimeToIconName(mimeType);
  icon := getIconByName(iconName, size);
  result:=icon;
end;

function TFileMimeIcon.getIconForFile(filename:string; size:integer):TIcon;
var mimeType:string; var iconName:string;
begin
  result := nil;
  mimeType:= DetectMimeTypeFromFile(filename, FMgc);
  iconName:= getMimeToIconName(mimeType);
  result := getIconByName(iconName, size);
end;

function TFileMimeIcon.getIconNameForFile(filename:string; size:integer):string;
var mimeType:string; var iconName:string;
begin
  result := '';
  mimeType:= DetectMimeTypeFromFile(filename, FMgc);
  iconName := getMimeToIconName(mimeType);
  result := iconName;
end;


function TFileMimeIcon.getIconByName( iconName:string; size:integer):TIcon;
var icon:TIcon; szname, HOME,XDG_THEME,XDG_DATA_DIRS:string;

  function findIconAtXDGBase(base:string; theme:string; iconName:string; szname:string):TIcon;
  begin
    icon:=loadIconFromFile(base+'/icons/'+theme+'/'+szname+'/mimetypes/'+iconName+'.png');
    if not Assigned(icon) then
      icon:=loadIconFromFile(base+'/icons/'+theme+'/'+szname+'/places/'+iconName+'.png');
    result := icon;
  end;

begin
  // https://specifications.freedesktop.org/icon-theme-spec/icon-theme-spec-latest.html
  // Icons and themes are looked for in a set of directories.
  // By default, apps should look in $HOME/.icons (for backwards compatibility),
  // in $XDG_DATA_DIRS/icons and in /usr/share/pixmaps (in that order).

  result := nil;
  szname := IntToStr(size);
  szname := szname+'x'+szname;

  HOME := GetEnvironmentVariable('HOME');
  XDG_THEME := 'highcolor';

  icon:=findIconAtXDGBase(HOME+'/.icons', XDG_THEME, iconName, szname);
  XDG_DATA_DIRS := GetEnvironmentVariable('XDG_DATA_DIRS');

  icon:=loadIconFromFile(HOME+'/.local/share/icons/default.kde4/'+szname+'/mimetypes/'+iconName+'.png');
  if not Assigned(icon) then
    icon:=loadIconFromFile(HOME+'/.local/share/icons/default.kde4/'+szname+'/places/'+iconName+'.png');
  if not Assigned(icon) then
    icon:=loadIconFromFile(HOME+'/.local/share/icons/gnome/'+szname+'/mimetypes/'+iconName+'.png');
  if not Assigned(icon) then
    icon:=loadIconFromFile(HOME+'/.local/share/icons/gnome/'+szname+'/places/'+iconName+'.png');
  if not Assigned(icon) then
    icon:=loadIconFromFile(HOME+'/.local/share/icons/hicolor/'+szname+'/mimetypes/'+iconName+'.png');
  if not Assigned(icon) then
    icon:=loadIconFromFile(HOME+'/.local/share/icons/hicolor/'+szname+'/places/'+iconName+'.png');

  if not Assigned(icon) then
     icon:=loadIconFromFile('/usr/share/icons/default.kde4/'+szname+'/mimetypes/'+iconName+'.png');
  if not Assigned(icon) then
    icon:=loadIconFromFile('/usr/share/icons/default.kde4/'+szname+'/places/'+iconName+'.png');
  if not Assigned(icon) then
    icon:=loadIconFromFile('/usr/share/icons/gnome/'+szname+'/mimetypes/'+iconName+'.png');
  if not Assigned(icon) then
    icon:=loadIconFromFile('/usr/share/icons/gnome/'+szname+'/places/'+iconName+'.png');
  if not Assigned(icon) then
    icon:=loadIconFromFile('/usr/share/icons/hicolor/'+szname+'/mimetypes/'+iconName+'.png');
  if not Assigned(icon) then
    icon:=loadIconFromFile('/usr/share/icons/hicolor/'+szname+'/places/'+iconName+'.png');
  result:=icon;
end;


function TFileMimeIcon.searchGlobs(pattern:string):string;
var mime: string;
begin
  mime := Fglobs.Values[pattern];
  {
  mime := '';
  for i:=0 to Fglobs.Count-1 do
    begin
      pat  := Fglobs.Names[i];
      if (pat = pattern) or (pat = lowerCase(pattern)) then
         begin
         mime := Fglobs.ValueFromIndex[i];
         break;
         end;
    end;}
  result:=mime;
end;

function split(Input: string; const Delimiter: Char):TStringList;
var Strings:TStringList;
begin
   Strings:=TStringList.Create;
   Assert(Assigned(Strings)) ;
   Strings.StrictDelimiter := true;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
   result:=Strings;
end;

procedure TFileMimeIcon.loadGlobs;
var i:integer; wgt,mim,pat: string; globs2, row:TStringList;
begin
  if not FileExists('/usr/share/mime/globs2') then exit;
  globs2:=TStringList.Create;
  globs2.LoadFromFile('/usr/share/mime/globs2');
  globs2.Sorted := false;
  globs2.CaseSensitive:=false;

  // reload to FGlobs optimized for search
  Fglobs:=TStringList.Create;
  Fglobs.NameValueSeparator:=':';
  for i:=0 to globs2.Count-1 do
    if globs2[i][1]<>'#' then
    begin
      row := split(globs2[i], ':');
      wgt := row[0];
      mim := row[1];
      pat := row[2];
      Fglobs.Add(pat+':'+mim);
    end;
end;

function TFileMimeIcon.getGlobsMimeForFile(filename:string):string;
var pattern:string;
begin
  result:='';
  if Assigned(FGlobs) then
    begin
    pattern := '*'+ExtractFileExt(filename);
    result := searchGlobs(pattern);
    end;
end;


function TFileMimeIcon.getMimeTypeForFile(filename:string):string;
var magicMime:string; globsMime:string;
begin
  magicMime := DetectMimeTypeFromFile(filename, FMgc);
  result:=magicMime;
  if ((magicMime = 'application/octet-stream') or (magicMime = 'text/plain'))
    and Assigned(FGlobs) then
    begin
    globsMime := getGlobsMimeForFile(filename);
    if (globsMime <> '') then
      result:=globsMime;
    end;
end;

procedure TFileMimeIcon.loadMimeIconNames;
var ics:TStringList; i:integer; n,v:string;
begin
  FMimeIconNames:=TStringList.Create;
  FMimeIconNames.NameValueSeparator:=':';
  FMimeIconNames.Sorted:=true;
  FMimeIconNames.LoadFromFile('/usr/share/mime/generic-icons');

  ics:=TStringList.Create;
  ics.NameValueSeparator:=':';
  ics.LoadFromFile('/usr/share/mime/icons');
  for i:=0 to ics.count-1 do
    begin
      n := ics.Names[i];
      v := ics.ValueFromIndex[i];
      FMimeIconNames.Values[n]:=v;
    end;
  ics.Free;
end;

function TFileMimeIcon.getMimeToIconName(mimeType:string):string;
begin
  result := FMimeIconNames.Values[mimeType];
  if result = '' then result := 'text-x-generic'; // default icon for a file
end;

end.

