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
     FreeNumInput;

type TFreePreferencesDialog = class(TForm)
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
                                    TrackBar1: TTrackBar;
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
                                    SpeedButton1: TSpeedButton;
                                    SpeedButton2: TSpeedButton;
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
                                    procedure Panel4Click(Sender: TObject);
                                    procedure Panel14Click(Sender: TObject);
                                    procedure Panel15Click(Sender: TObject);
                                    procedure Panel2Click(Sender: TObject);
                                    procedure Panel5Click(Sender: TObject);
                                    procedure Panel6Click(Sender: TObject);
                                    procedure Panel7Click(Sender: TObject);
                                    procedure Panel8Click(Sender: TObject);
                                    procedure Panel9Click(Sender: TObject);
                                    procedure Panel10Click(Sender: TObject);
                                    procedure Panel11Click(Sender: TObject);
                                    procedure Panel12Click(Sender: TObject);
                                    procedure Panel13Click(Sender: TObject);
                                    procedure Panel16Click(Sender: TObject);
                                    procedure Panel17Click(Sender: TObject);
                                    procedure Panel18Click(Sender: TObject);
                                    procedure Panel19Click(Sender: TObject);
                                    procedure Panel20Click(Sender: TObject);
                                    procedure Panel21Click(Sender: TObject);
                                    procedure Panel22Click(Sender: TObject);
                                    procedure Panel23Click(Sender: TObject);
                                    procedure Panel24Click(Sender: TObject);
                                    procedure BitBtn1Click(Sender: TObject);
                                    procedure BitBtn2Click(Sender: TObject);
                                    procedure Panel25Click(Sender: TObject);
                                    procedure Panel26Click(Sender: TObject);
                                    procedure SpeedButton3Click(Sender: TObject);
                                 private   { Private declarations }
                                    FFreeship:TFreeShip;
                                    procedure Updatedata;
                                 public    { Public declarations }
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
   TrackBar1.Position:=FFreeship.Preferences.PointSize;
   if FFreeship.Preferences.MaxUndoMemory<1 then FreeNumInput1.Value:=1
                                            else FreeNumInput1.Value:=FFreeship.Preferences.MaxUndoMemory;
end;{TFreePreferencesDialog.Updatedata}

function TFreePreferencesDialog.Execute(Freeship:TFreeShip):Boolean;
begin
   FFreeship:=Freeship;
   Updatedata;
   Showmodal;
   Result:=ModalResult=mrOK;
end;{TFreePreferencesDialog.Execute}

procedure TFreePreferencesDialog.Panel4Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel4.Color;
   if ColorDialog.Execute then Panel4.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel4Click}

procedure TFreePreferencesDialog.Panel14Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel14.Color;
   if ColorDialog.Execute then Panel14.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel14Click}

procedure TFreePreferencesDialog.Panel15Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel15.Color;
   if ColorDialog.Execute then Panel15.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel15Click}

procedure TFreePreferencesDialog.Panel2Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel2.Color;
   if ColorDialog.Execute then Panel2.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel2Click}

procedure TFreePreferencesDialog.Panel5Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel5.Color;
   if ColorDialog.Execute then Panel5.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel5Click}

procedure TFreePreferencesDialog.Panel6Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel6.Color;
   if ColorDialog.Execute then Panel6.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel6Click}

procedure TFreePreferencesDialog.Panel7Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel7.Color;
   if ColorDialog.Execute then Panel7.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel7Click}

procedure TFreePreferencesDialog.Panel8Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel8.Color;
   if ColorDialog.Execute then Panel8.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel8Click}

procedure TFreePreferencesDialog.Panel9Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel9.Color;
   if ColorDialog.Execute then Panel9.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel9Click}

procedure TFreePreferencesDialog.Panel10Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel10.Color;
   if ColorDialog.Execute then Panel10.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel10Click}

procedure TFreePreferencesDialog.Panel11Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel11.Color;
   if ColorDialog.Execute then Panel11.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel11Click}

procedure TFreePreferencesDialog.Panel12Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel12.Color;
   if ColorDialog.Execute then Panel12.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel12Click}

procedure TFreePreferencesDialog.Panel13Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel13.Color;
   if ColorDialog.Execute then Panel13.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel13Click}

procedure TFreePreferencesDialog.Panel16Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel16.Color;
   if ColorDialog.Execute then Panel16.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel16Click}

procedure TFreePreferencesDialog.Panel17Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel17.Color;
   if ColorDialog.Execute then Panel17.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel17Click}

procedure TFreePreferencesDialog.Panel18Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel18.Color;
   if ColorDialog.Execute then Panel18.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel18Click}

procedure TFreePreferencesDialog.Panel19Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel19.Color;
   if ColorDialog.Execute then Panel19.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel19Click}

procedure TFreePreferencesDialog.Panel20Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel20.Color;
   if ColorDialog.Execute then Panel20.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel20Click}

procedure TFreePreferencesDialog.Panel21Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel21.Color;
   if ColorDialog.Execute then Panel21.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel21Click}

procedure TFreePreferencesDialog.Panel22Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel22.Color;
   if ColorDialog.Execute then Panel22.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel22Click}

procedure TFreePreferencesDialog.Panel23Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel23.Color;
   if ColorDialog.Execute then Panel23.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel23Click}

procedure TFreePreferencesDialog.Panel24Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel24.Color;
   if ColorDialog.Execute then Panel24.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel24Click}

procedure TFreePreferencesDialog.BitBtn1Click(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreePreferencesDialog.BitBtn1Click}

procedure TFreePreferencesDialog.BitBtn2Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreePreferencesDialog.BitBtn2Click}

procedure TFreePreferencesDialog.Panel25Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel25.Color;
   if ColorDialog.Execute then Panel25.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel25Click}

procedure TFreePreferencesDialog.Panel26Click(Sender: TObject);
begin
   ColorDialog.Color:=Panel26.Color;
   if ColorDialog.Execute then Panel26.Color:=ColorDialog.Color;
end;{TFreePreferencesDialog.Panel26Click}

procedure TFreePreferencesDialog.SpeedButton3Click(Sender: TObject);
begin
   if MessageDlg(Userstring(247)+'?'+#13#10+
                 Userstring(248)+'.',mtWarning,[mbYes,mbNo],0)=mrYes then
   begin
      FFreeship.Preferences.ResetColors;
      Updatedata;
   end;
end;{TFreePreferencesDialog.SpeedButton3Click}

end.
