unit FileIconGtk3;
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, Graphics,
  LazGio2 ,LazGlib2;

function getIconNamesForFileGtk3(filename:string; size:integer):PPgchar;

implementation

{
There is no easy way to get an icon name for a file in GTK2.
So we use Gtk3 to just get icon names.
Then it can be used to get an icon file name using GTK2.

ATTENTION! Do not try to use LazGTK3 module here because it initializes app environment for GTK3.
that conflicts with GTK2 of an application.
Using GTK3 for whole application is not really possibble because LCL for GTK3 is not complete yet.
}
function getIconNamesForFileGtk3(filename:string; size:integer):PPgchar;
var file_name: Pgchar;
    file_info: PGFileInfo;
    gfile : PGFile;
    flags: TGFileQueryInfoFlags;
    cancellable: PGCancellable;
    error: PGError;
    gicon : PGIcon;
    gicon_str: Pgchar;
begin
  result := nil;
  file_name := PChar(filename);
  gfile := g_file_new_for_path(file_name);
  cancellable:= TGCancellable.new;
  flags := G_FILE_QUERY_INFO_NONE;
  error := new(PGError);
  file_info := g_file_query_info(gfile, 'standard::icon', flags, cancellable, @error);
  gicon := g_file_info_get_icon(file_info);
  // There is no other way to get a type of gicon. workaround is to get it from string.
  // it can be GBytesIcon, GEmblem, GEmblemedIcon, GFileIcon and GThemedIcon.
  // for now we expect just GThemedIcon
  gicon_str := gicon^.to_string;
  if strlcomp(gicon_str, '. GThemedIcon',11) = 0 then
     result := (PGThemedIcon(gicon))^.get_names
  else
     begin
     GetMem(result, sizeof(Pgchar));
     result[0] := gicon_str;
     end;
  //else
  //   raise Exception.create('Unknown icon string:"'+String(gicon_str)+'"');
end;

end.

