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
     ComCtrls, Spin,
     FreeNumInput;

type

{ TFreePreferencesDialog }

 TFreePreferencesDialog = class(TForm)
   ComboBoxEncoding: TComboBox;
   ComboBoxThemes: TComboBox;
   LabelEncoding: TLabel;
   Label38: TLabel;
   SelectToolIconSize: TComboBox;
   EditTempDir: TEdit;
   EditLanguagesDir: TEdit;
   EditManualsDir: TEdit;
   EditExecDir: TEdit;
   EditOpenDir: TEdit;
   EditSaveDir: TEdit;
   EditImportDir: TEdit;
   EditExportDir: TEdit;
   Label28: TLabel;
   Label29: TLabel;
   Label30: TLabel;
   Label31: TLabel;
   Label32: TLabel;
   Label33: TLabel;
   Label34: TLabel;
   Label35: TLabel;
   Label37: TLabel;
   PageControl1: TPageControl;
    Panel1: TPanel;
    Panel: TPanel;
    Panel3: TPanel;
    BitBtn2: TSpeedButton;
    Label1: TLabel;
    Panel4: TPanel;
    ColorDialog: TColorDialog;
    Label2: TLabel;
    Panel2: TPanel;
    Label3: TLabel;
    Panel5: TPanel;
    Label4: TLabel;
    Panel6: TPanel;
    Label5: TLabel;
    Panel7: TPanel;
    Label6: TLabel;
    Panel8: TPanel;
    Label7: TLabel;
    Panel9: TPanel;
    Label8: TLabel;
    Panel10: TPanel;
    Label9: TLabel;
    Panel11: TPanel;
    Label10: TLabel;
    Panel12: TPanel;
    Label11: TLabel;
    Panel13: TPanel;
    Label12: TLabel;
    Panel14: TPanel;
    Label13: TLabel;
    Panel15: TPanel;
    Label14: TLabel;
    Panel16: TPanel;
    Label15: TLabel;
    Panel17: TPanel;
    Label16: TLabel;
    Panel18: TPanel;
    Label17: TLabel;

      SelectDirectoryDialog1: TSelectDirectoryDialog;
      SpeedButton12: TSpeedButton;
      SpeedButton13: TSpeedButton;
      SpeedButton14: TSpeedButton;
      SpeedButton15: TSpeedButton;
      SpeedButton16: TSpeedButton;
      SpeedButton17: TSpeedButton;
      SpeedButton8: TSpeedButton;
      SpeedButton9: TSpeedButton;
      SpinEdit1: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Label18: TLabel;
    Panel19: TPanel;
    Label19: TLabel;
    Panel20: TPanel;
    Label20: TLabel;
    Panel21: TPanel;
    Label21: TLabel;
    Panel22: TPanel;
    Label22: TLabel;
    Panel23: TPanel;
    Label23: TLabel;
    Panel24: TPanel;
    BitBtn1: TSpeedButton;
    Label24: TLabel;
    Panel25: TPanel;
    Label25: TLabel;
    Panel26: TPanel;
    SpeedButton3: TSpeedButton;
    Label26: TLabel;
    ComboBox1: TComboBox;
    Label27: TLabel;
    FreeNumInput1: TFreeNumInput;
    procedure ColorPanelClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure ComboBoxThemesChange(      Sender: TObject);
    procedure EditDirChange(      Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject      );
    procedure SpeedButton13Click(Sender: TObject      );
    procedure SpeedButton14Click(Sender: TObject      );
    procedure SpeedButton15Click(Sender: TObject      );
    procedure SpeedButton16Click(Sender: TObject      );
    procedure SpeedButton17Click(Sender: TObject      );
    procedure SpeedButton18Click(Sender: TObject      );
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject       );
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

uses FreeLanguageSupport;

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

procedure TFreePreferencesDialog.SpeedButton8Click(Sender: TObject);
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
  //SelectDirectoryDialog1.FileName:=EditToolIconsDir.Text;
  //if SelectDirectoryDialog1.Execute then
    //EditToolIconsDir.Text := SelectDirectoryDialog1.FileName;
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
