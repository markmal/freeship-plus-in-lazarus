unit FileIconGtk;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  FileIcon,
  GTK2, GLib2,
  FileIconGtk3;

type TFileIconGtk = class(TFileIcon)
protected
  function getIconByNameGtk2(icon_name:Pgchar; size:integer):TIcon;
public
  function getIconByName( iconName:string; size:integer):TIcon; override;
  function getIconNameForFile(filename:string; size:integer):string; override;
  function getIconForFile(filename:string; size:integer):TIcon;override;
end;

implementation

function TFileIconGtk.getIconByNameGtk2(icon_name:Pgchar; size:integer):TIcon;
var icon_theme: PGtkIconTheme;
    icon_info: PGtkIconInfo;
    icon_filename: Pgchar;
    iconFileName: string;
begin
  result := nil;
  icon_theme := gtk_icon_theme_get_default;
  if gtk_icon_theme_has_icon(icon_theme, icon_name) then
  begin
       icon_info := gtk_icon_theme_lookup_icon( icon_theme, icon_name, size,
                                                GTK_ICON_LOOKUP_NO_SVG);
       icon_filename := gtk_icon_info_get_filename(icon_info);
       iconFileName := String(icon_filename);
       result := LoadIconFromFile(iconFileName);
  end;
end;

function TFileIconGtk.getIconByName( iconName:string; size:integer):TIcon;
begin
  result := getIconByNameGtk2(PChar(iconName), size);
end;

function TFileIconGtk.getIconNameForFile(filename:string; size:integer):string;
var icon_theme: PGtkIconTheme;  icon_names:PPgchar; icon_name:Pgchar; i:integer;
begin
  result := '';
  icon_names := getIconNamesForFileGtk3(filename, size);
  icon_theme := gtk_icon_theme_get_default;
  for i:=0 to (sizeOf(icon_names) div sizeOf(Pgchar))-1 do
  begin
      icon_name := icon_names[i];
      if gtk_icon_theme_has_icon(icon_theme, icon_name) then
      begin
         result := String(icon_name);
         break;
      end;
  end;
end;

function TFileIconGtk.getIconForFile(filename:string; size:integer):TIcon;
var icon:TIcon;   icon_names:PPgchar; i:integer; icon_name:Pgchar;
begin
  result := nil;
    icon_names := getIconNamesForFileGtk3(filename, size);
    for i:=0 to sizeOf(icon_names) do
        begin
        icon_name := icon_names[i];
        icon := getIconByNameGtk2(icon_name, size);
        result := icon;
        if icon <> nil then break;
        end;

end;

end.

