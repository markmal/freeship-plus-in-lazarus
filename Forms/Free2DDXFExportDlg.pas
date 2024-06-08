{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    e-mail                  : Info@FREEship.org                                              }
{    FREE!ship project page  : https://sourceforge.net/projects/freeship                      }
{    FREE!ship homepage      : www.FREEship.org                                               }
{                                                                                             }
{    This program is free software; you can redistribute it and/or modify it under            }
{    the terms of the GNU General Public License as published by the                          }
{    Free Software Foundation; either version 2 of the License, or (at your option)           }
{    any later version.                                                                       }
{                                                                                             }
{    This program is distributed in the hope that it will be useful, but WITHOUT ANY          }
{    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          }
{    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 }
{                                                                                             }
{    You should have received a copy of the GNU General Public License along with             }
{    this program; if not, write to the Free Software Foundation, Inc.,                       }
{    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    }
{                                                                                             }
{#############################################################################################}

unit Free2DDXFExportDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
    {$IFDEF Windows}
  Windows,
  shlobj,
    {$ifdef LCL}
         {$IFDEF VER3}
  LazUTF8,
  LazFileUtils,
     {$ELSE}
  FileUtil, //deprecated
     {$ENDIF}

    {$endif}
    {$ELSE}
  LCLIntf, LCLType,
          {$IFDEF VER3}
  LazUTF8,
  LazFileUtils,
     {$ELSE}
  FileUtil, //deprecated
     {$ENDIF}

    {$ENDIF}
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Spin,
  LCLTranslator,
  FreeShipUnit, FreeStringsUnit;
type

  { TDXFExport2DDialog }

  TDXFExport2DDialog = class(TForm)
    cbCreateIndividualFiles: TCheckBox;
    cbCreateIndividualLayers: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TFloatSpinEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    SaveDialog: TSaveDialog;
    BitBtn1: TSpeedButton;
    BitBtn2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private   { Private declarations }
    function FGetExportDirectory: string;
    procedure FSetExportDirectory(val: string);
    function FGetSegmentLength: double;
    procedure FSetSegmentLength(val: double);
    procedure FSetUnits;
  public    { Public declarations }
    function BrowseForFolder(
      const browseTitle: PAnsiChar; initialFolder: string = ''): string;
    function Execute: boolean;
    property ExportDirectory: string
      read FGetExportDirectory write FSetExportDirectory;
    property SegmentLength: double
      read FGetSegmentLength write FSetSegmentLength;
  end;

var DXFExport2DDialog: TDXFExport2DDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

var lg_StartFolder: string;

{$IFnDEF FPC}
///////////////////////////////////////////////////////////////////
// Call back function used to set the initial browse directory.
///////////////////////////////////////////////////////////////////
function BrowseForFolderCallBack(Wnd: HWND;
  uMsg: UINT; lParam, lpData: LPARAM): integer stdcall;
begin
  if uMsg = BFFM_INITIALIZED then
    SendMessage(Wnd, BFFM_SETSELECTION, 1, integer(@lg_StartFolder[1]));
  Result := 0;
end;{BrowseForFolderCallBack}

///////////////////////////////////////////////////////////////////
// This function allows the user to browse for a folder
//
// Arguments:-
//    browseTitle : The title to display on the browse dialog.
//  initialFolder : Optional argument. Use to specify the folder
//                  initially selected when the dialog opens.
//
// Returns: The empty string if no folder was selected (i.e. if the
//          user clicked cancel), otherwise the full folder path.
///////////////////////////////////////////////////////////////////
function TDXFExport2DDialog.BrowseForFolder(const browseTitle: PAnsiChar;
  initialFolder: string = ''): string;
var browse_info: TBrowseInfo;
  folder: array[0..MAX_PATH] of char;
  find_context: PItemIDList;
  I: integer;
begin
  FillChar(browse_info, SizeOf(browse_info), #0);
  lg_StartFolder := initialFolder;
  browse_info.pszDisplayName := @folder[0];
  browse_info.lpszTitle := browseTitle;
  browse_info.ulFlags := BIF_RETURNONLYFSDIRS;
  browse_info.hwndOwner := Application.Handle;
  if initialFolder <> '' then browse_info.lpfn := BrowseForFolderCallBack;
  find_context := SHBrowseForFolder(browse_info);
  if Assigned(find_context) then
  begin
    if SHGetPathFromIDList(find_context, folder) then
    begin
      Result := '';
      for I := 1 to length(Folder) do
        if Folder[I - 1] = #0 then break else Result := Result + Folder[I - 1];
    end
    else Result := '';
    GlobalFreePtr(find_context);
  end
  else Result := '';
end;{TDXFExport2DDialog.BrowseForFolder}
{$ENDIF}

function TDXFExport2DDialog.BrowseForFolder(const browseTitle: PAnsiChar;
  initialFolder: string = ''): string;
var dlg: TSelectDirectoryDialog;
begin
  dlg := TSelectDirectoryDialog.Create(Self);
  dlg.Title := browseTitle;
  dlg.InitialDir := initialFolder;
  if dlg.Execute then
    Result := dlg.FileName
  else
    Result := '';
end;

function TDXFExport2DDialog.FGetExportDirectory: string;
begin
  Result := Edit3.Text;
end;{TDXFExport2DDialog.FGetExportDirectory}

procedure TDXFExport2DDialog.FSetExportDirectory(val: string);
begin
  Edit3.Text := Val;
end;{TDXFExport2DDialog.FSetExportDirectory}

function TDXFExport2DDialog.FGetSegmentLength: double;
begin
  Result := Edit1.Value;
end;{TDXFExport2DDialog.FGetSegmentLength}

procedure TDXFExport2DDialog.FSetSegmentLength(val: double);
begin
  if Val < 1e-5 then Val := 1e-5;
  Edit1.Value := Val;
end;{TDXFExport2DDialog.FSetSegmentLength}

procedure TDXFExport2DDialog.FSetUnits;
var Str: string;
begin
  Str := ComboBox1.Text + Userstring(937);
  //Label3.Caption:=Str;
end;{TDXFExport2DDialog.FSetUnits}

function TDXFExport2DDialog.Execute: boolean;
begin
  FSetUnits;
  GlobalFreeship.Preferences.LoadImageIntoBitmap(BitBtn1.Glyph, 'Ok');
  GlobalFreeship.Preferences.LoadImageIntoBitmap(BitBtn2.Glyph, 'Cancel');
  //ShowTranslatedValues(Self);
  Showmodal;
  Result := ModalResult = mrOk;
end;{TDXFExport2DDialog.Execute}

procedure TDXFExport2DDialog.SpeedButton1Click(Sender: TObject);
var Tmp: string;
begin
  Tmp := BrowseForFolder(PAnsichar(rs_Choose_a_directory_where_you_want_to_save_the_dxf_files_to_ {UserString[209]} + ':'), ExportDirectory);
  if DirectoryExistsUTF8(Tmp) { *Converted from DirectoryExists* } then
    self.ExportDirectory := Tmp;
end;{TDXFExport2DDialog.SpeedButton1Click}

procedure TDXFExport2DDialog.BitBtn1Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TDXFExport2DDialog.BitBtn1Click}

procedure TDXFExport2DDialog.BitBtn2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TDXFExport2DDialog.BitBtn2Click}

procedure TDXFExport2DDialog.ComboBox1Change(Sender: TObject);
begin
  FSetUnits;
end;{TDXFExport2DDialog.ComboBox1Change}

end.
