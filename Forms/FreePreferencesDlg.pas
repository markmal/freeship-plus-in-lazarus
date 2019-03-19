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

unit FreePreferencesDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
     Buttons,
     ExtCtrls,
     FreeShipUnit,
     ComCtrls,
     Spin, ValEdit,
     FreeLanguageSupport;

type

{ TFreePreferencesDialog }

 TFreePreferencesDialog = class(TForm)
   ComboBox1: TComboBox;
   ComboBoxEncoding: TComboBox;
   ComboBoxThemes: TComboBox;
   EditExecDir: TEdit;
   EditExportDir: TEdit;
   EditGlobalImportDir: TEdit;
   EditGlobalOpenDir: TEdit;
   EditImportDir: TEdit;
   EditLanguagesDir: TEdit;
   EditManualsDir: TEdit;
   EditOpenDir: TEdit;
   EditSaveDir: TEdit;
   EditTempDir: TEdit;
   FreeNumInput1: TSpinEdit;
   Label1: TLabel;
   Label10: TLabel;
   Label11: TLabel;
   Label12: TLabel;
   Label13: TLabel;
   Label14: TLabel;
   Label15: TLabel;
   Label16: TLabel;
   Label17: TLabel;
   Label18: TLabel;
   Label19: TLabel;
   Label2: TLabel;
   Label20: TLabel;
   Label21: TLabel;
   Label22: TLabel;
   Label23: TLabel;
   Label24: TLabel;
   Label25: TLabel;
   Label26: TLabel;
   Label27: TLabel;
   Label29: TLabel;
   Label3: TLabel;
   Label30: TLabel;
   Label31: TLabel;
   Label32: TLabel;
   Label33: TLabel;
   Label34: TLabel;
   Label35: TLabel;
   Label36: TLabel;
   Label37: TLabel;
   Label38: TLabel;
   Label39: TLabel;
   Label4: TLabel;
   Label5: TLabel;
   Label6: TLabel;
   Label7: TLabel;
   Label8: TLabel;
   Label9: TLabel;
   LabelEncoding: TLabel;
   LabelLanguagesDir: TLabel;
   PageControl1: TPageControl;
   Panel: TPanel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel2: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    Panel26: TPanel;
    Panel28: TPanel;
    Panel29: TPanel;
    Panel3: TPanel;
    BitBtn2: TSpeedButton;
    ColorDialog: TColorDialog;
    Panel30: TPanel;
    Panel31: TPanel;
    Panel32: TPanel;
    Panel33: TPanel;
    Panel34: TPanel;
    Panel35: TPanel;
    Panel38: TPanel;
    Panel39: TPanel;
    Panel4: TPanel;
    Panel40: TPanel;
    Panel41: TPanel;
    Panel42: TPanel;
    Panel43: TPanel;
    Panel44: TPanel;
    Panel45: TPanel;
    PanelGlobalImportDir: TPanel;
    Panel47: TPanel;
    Panel48: TPanel;
    Panel49: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;

      SelectDirectoryDialog1: TSelectDirectoryDialog;
    BitBtn1: TSpeedButton;
    SelectToolIconSize: TComboBox;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButtonLanguagesDir: TSpeedButton;
    SpinEdit1: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure ColorPanelClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure ComboBoxThemesChange(      Sender: TObject);
    procedure EditDirChange(      Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject      );
    procedure SpeedButton13Click(Sender: TObject      );
    procedure SpeedButton14Click(Sender: TObject      );
    procedure SpeedButton15Click(Sender: TObject      );
    procedure SpeedButton16Click(Sender: TObject      );
    procedure SpeedButton17Click(Sender: TObject      );
    procedure SpeedButton18Click(Sender: TObject      );
    procedure SpeedButton19Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButtonLanguagesDirClick(Sender: TObject       );
    procedure SpeedButton9Click(Sender: TObject       );
    procedure SpinEdit1Change(Sender: TObject);
 private   { Private declarations }
    FFreeship:TFreeShip;
    FConfigChanged : boolean;
    FThemeChanged : boolean;
    procedure Updatedata;
    procedure ComboBoxEncodingFillItems;
 public    { Public declarations }
    property IsThemeChanged : boolean read FThemeChanged;
    property IsConfigChanged : boolean read FConfigChanged;
    //property Theme: string read ComboBoxThemes.Text;
    function Execute(Freeship:TFreeShip):Boolean;
end;

var FreePreferencesDialog: TFreePreferencesDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFreePreferencesDialog.Updatedata;
var i: integer;
begin
   Panel4.Color:=FFreeship.Preferences.ViewportColor;
   Panel2.Color:=FFreeship.Preferences.LayerColor;
   Panel5.Color:=FFreeship.Preferences.UnderWaterColor;
   Panel6.Color:=FFreeship.Preferences.EdgeColor;
   Panel7.Color:=FFreeship.Preferences.CreaseEdgeColor;
   Panel8.Color:=FFreeship.Preferences.CreaseColor;
   Panel9.Color:=FFreeship.Preferences.RegularPointColor;
   Panel10.Color:=FFreeship.Preferences.CreasePointColor;
   Panel11.Color:=FFreeship.Preferences.CornerPointColor;
   Panel12.Color:=FFreeship.Preferences.DartPointColor;
   Panel13.Color:=FFreeship.Preferences.SelectColor;
   Panel14.Color:=FFreeship.Preferences.GridColor;
   Panel15.Color:=FFreeship.Preferences.GridFontColor;
   Panel16.Color:=FFreeship.Preferences.StationColor;
   Panel17.Color:=FFreeship.Preferences.ButtockColor;
   Panel18.Color:=FFreeship.Preferences.WaterlineColor;
   Panel19.Color:=FFreeship.Preferences.NormalColor;
   Panel20.Color:=FFreeship.Preferences.DiagonalColor;
   Panel21.Color:=FFreeship.Preferences.LeakPointColor;
   Panel22.Color:=FFreeship.Preferences.MarkerColor;
   Panel23.Color:=FFreeship.Preferences.CurvaturePlotColor;
   Panel24.Color:=FFreeship.Preferences.ControlCurveColor;
   Panel25.Color:=FFreeship.Preferences.HydrostaticsFontColor;
   Panel26.Color:=FFreeship.Preferences.ZebraStripeColor;
   SpinEdit1.Value:=FFreeship.Preferences.PointSize;
   if FFreeship.Preferences.MaxUndoMemory<1 then FreeNumInput1.Value:=1
                                            else FreeNumInput1.Value:=FFreeship.Preferences.MaxUndoMemory;

   EditLanguagesDir.Text:= FFreeship.Preferences.LanguagesDirectory;
   EditManualsDir.Text:= FFreeship.Preferences.ManualsDirectory;
   EditExecDir.Text:= FFreeship.Preferences.ExecDirectory;
   EditTempDir.Text:= FFreeship.Preferences.TempDirectory;
   EditOpenDir.Text:= FFreeship.Preferences.OpenDirectory;
   EditSaveDir.Text:= FFreeship.Preferences.SaveDirectory;
   EditImportDir.Text:= FFreeship.Preferences.ImportDirectory;
   EditExportDir.Text:= FFreeship.Preferences.ExportDirectory;
   //EditToolIconsDir.Text:= FFreeship.Preferences.ToolIconDirectory;
   EditGlobalOpenDir.Text:= FFreeship.Preferences.GlobalOpenDirectory;
   EditGlobalImportDir.Text:= FFreeship.Preferences.GlobalImportDirectory;

   SelectToolIconSize.Text:=IntToStr(FFreeship.Preferences.ToolIconSize);

   ComboBoxEncodingFillItems;
   for i:=0 to ComboBoxEncoding.Items.Count-1 do
     if String(ComboBoxEncoding.Items.Objects[i]) = FFreeship.Preferences.FbmEncoding
       then break;
   if i>ComboBoxEncoding.Items.Count then i:=-1;
   ComboBoxEncoding.ItemIndex:=i;

end;{TFreePreferencesDialog.Updatedata}

function TFreePreferencesDialog.Execute(Freeship:TFreeShip):Boolean;
begin
   FFreeship:=Freeship;
   Updatedata;
   FConfigChanged:= false;
   FThemeChanged := false;
   Freeship.Preferences.LoadImageIntoBitmap(BitBtn1.Glyph,'Ok');
   Freeship.Preferences.LoadImageIntoBitmap(BitBtn2.Glyph,'Cancel');
   ShowTranslatedValues(Self);
   Showmodal;
   Result:=ModalResult=mrOK;
end;{TFreePreferencesDialog.Execute}

procedure TFreePreferencesDialog.ColorPanelClick(Sender: TObject);
var Panel: TPanel;
begin
   if ( Sender.ClassType <> TPanel ) then exit;
   Panel := TPanel(Sender);
   ColorDialog.Color:=Panel.Color;
   if ColorDialog.Execute then
     if ColorDialog.Color <> Panel.Color then
       begin
       Panel.Color:=ColorDialog.Color;
       FThemeChanged := true;
       end;
end;{TFreePreferencesDialog.ColorPanelClick}


procedure TFreePreferencesDialog.BitBtn1Click(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreePreferencesDialog.BitBtn1Click}

procedure TFreePreferencesDialog.BitBtn2Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreePreferencesDialog.BitBtn2Click}

procedure TFreePreferencesDialog.ComboBoxThemesChange(Sender: TObject);
begin
   if (Sender = ComboBoxThemes) then
     begin
     FFreeship.Preferences.LoadTheme(ComboBoxThemes.Text);
     FThemeChanged := false;
     UpdateData;
     end;
   FConfigChanged:= true;
end;

procedure TFreePreferencesDialog.EditDirChange(Sender: TObject);
begin
  FConfigChanged:= true;
end;

procedure TFreePreferencesDialog.FormShow(Sender: TObject);
begin
  PageControl1.Constraints.MinHeight
    := PanelGlobalImportDir.Top + PanelGlobalImportDir.Height + 8;
end;

procedure TFreePreferencesDialog.SpinEdit1Change(Sender: TObject);
begin
  FThemeChanged := true;
end;

procedure TFreePreferencesDialog.SpeedButton3Click(Sender: TObject);
begin
   if MessageDlg(Userstring(247)+'?'+#13#10+
                 Userstring(248)+'.',mtWarning,[mbYes,mbNo],0)=mrYes then
   begin
      FFreeship.Preferences.ResetColors;
      Updatedata;
   end;
end;{TFreePreferencesDialog.SpeedButton3Click}

procedure TFreePreferencesDialog.SpeedButtonLanguagesDirClick(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName:=EditLanguagesDir.Text;
  if SelectDirectoryDialog1.Execute then
    EditLanguagesDir.Text := SelectDirectoryDialog1.FileName;
end;

procedure TFreePreferencesDialog.SpeedButton9Click(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName:=EditManualsDir.Text;
  if SelectDirectoryDialog1.Execute then
    EditManualsDir.Text := SelectDirectoryDialog1.FileName;
end;

procedure TFreePreferencesDialog.SpeedButton12Click(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName:=EditExecDir.Text;
  if SelectDirectoryDialog1.Execute then
    EditExecDir.Text := SelectDirectoryDialog1.FileName;
end;

procedure TFreePreferencesDialog.SpeedButton13Click(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName:=EditTempDir.Text;
  if SelectDirectoryDialog1.Execute then
    EditTempDir.Text := SelectDirectoryDialog1.FileName;
end;

procedure TFreePreferencesDialog.SpeedButton14Click(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName:=EditOpenDir.Text;
  if SelectDirectoryDialog1.Execute then
    EditOpenDir.Text := SelectDirectoryDialog1.FileName;
end;

procedure TFreePreferencesDialog.SpeedButton15Click(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName:=EditSaveDir.Text;
  if SelectDirectoryDialog1.Execute then
    EditSaveDir.Text := SelectDirectoryDialog1.FileName;
end;

procedure TFreePreferencesDialog.SpeedButton16Click(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName:=EditImportDir.Text;
  if SelectDirectoryDialog1.Execute then
    EditImportDir.Text := SelectDirectoryDialog1.FileName;
end;

procedure TFreePreferencesDialog.SpeedButton17Click(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName:=EditExportDir.Text;
  if SelectDirectoryDialog1.Execute then
    EditExportDir.Text := SelectDirectoryDialog1.FileName;
end;

procedure TFreePreferencesDialog.SpeedButton18Click(Sender: TObject);
begin
 SelectDirectoryDialog1.FileName:=EditGlobalOpenDir.Text;
 if SelectDirectoryDialog1.Execute then
   EditGlobalOpenDir.Text := SelectDirectoryDialog1.FileName;
end;

procedure TFreePreferencesDialog.SpeedButton19Click(Sender: TObject);
begin
 SelectDirectoryDialog1.FileName:=EditGlobalImportDir.Text;
 if SelectDirectoryDialog1.Execute then
   EditGlobalImportDir.Text := SelectDirectoryDialog1.FileName;
end;

procedure TFreePreferencesDialog.ComboBoxEncodingFillItems;
begin
 with ComboBoxEncoding.Items do
   begin
    AddObject('ISO_8859_1 -  Central Europe', TObject(String('iso88591')));
    AddObject('ISO_8859_15 -  Western European languages', TObject(String('iso885915')));
    AddObject('ISO_8859_2 -  Eastern Europe', TObject(String('iso88592')));
    AddObject('CP1250 -  Central Europe', TObject(String('cp1250')));
    AddObject('CP1251 -  Cyrillic', TObject(String('cp1251')));
    AddObject('CP1252 -  Latin 1', TObject(String('cp1252')));
    AddObject('CP1253 -  Greek', TObject(String('cp1253')));
    AddObject('CP1254 -  Turkish', TObject(String('cp1254')));
    AddObject('CP1255 -  Hebrew', TObject(String('cp1255')));
    AddObject('CP1256 -  Arabic', TObject(String('cp1256')));
    AddObject('CP1257 -  Baltic', TObject(String('cp1257')));
    AddObject('CP1258 -  Vietnam', TObject(String('cp1258')));
    AddObject('CP437 -  DOS Central Europe', TObject(String('cp437')));
    AddObject('CP850 -  DOS Western Europe', TObject(String('cp850')));
    AddObject('CP852 -  DOS Central Europe', TObject(String('cp852')));
    AddObject('CP866 -  DOS and Windows console Cyrillic', TObject(String('cp866')));
    AddObject('CP874 -  Thai', TObject(String('cp874')));
    AddObject('KOI8 -  Russian Cyrillic', TObject(String('koi8')));
   end;
end;


end.
