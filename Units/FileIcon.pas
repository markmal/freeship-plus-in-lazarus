unit FileIcon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls;

type TFileIconAdapter = class

  FSmallImageList: TImageList;
  FLargeImageList: TImageList;
  FIconNamesMap: TStringList;

public
  constructor Create(smallImageList: TImageList; largeImageList: TImageList);

  function loadIconFromFile(ifilename:string):TIcon;
  //function getIconByName( iconName:string; size:integer):TIcon; virtual; abstract;
  function getIconNameForFile(filename:string; size:integer):string; virtual; abstract;
  function getIconForFile(filename:string; size:integer):TIcon; virtual; abstract;

  function addIconsForFile(filename:string):integer;  virtual; abstract;

end;

implementation

constructor TFileIconAdapter.Create(smallImageList: TImageList; largeImageList: TImageList);
begin
  inherited Create;
  FSmallImageList := smallImageList;
  FLargeImageList := largeImageList;
  FIconNamesMap := TStringList.Create;
end;

function TFileIconAdapter.loadIconFromFile(ifilename:string):TIcon;
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

