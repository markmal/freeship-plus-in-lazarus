unit FileIcon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type TFileIcon = class
public
  function loadIconFromFile(ifilename:string):TIcon;
  function getIconByName( iconName:string; size:integer):TIcon; virtual; abstract;
  function getIconNameForFile(filename:string; size:integer):string; virtual; abstract;
  function getIconForFile(filename:string; size:integer):TIcon; virtual; abstract;
end;

implementation

function TFileIcon.loadIconFromFile(ifilename:string):TIcon;
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

end.

