{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }

{    Copyright © 2005, by Martijn van Engeland                                                }
{    Copyright © 2007-2010 by Timoshenko V.F.                                                 }
{    e-mail                  : vftim@rambler.ru                                               }
{    FREE!ship project page  : www.FREEship-plus.pisem.su                                     }
{    FREE!ship homepage      : www.FREEship-plus.land.ru                                      }

{    This program is free software; you can redistribute it and/or modify it under            }
{    the terms of the GNU General Public License as published by the                          }
{    Free Software Foundation; either version 2 of the License, or (at your option)           }
{    any later version.                                                                       }

{    This program is distributed in the hope that it will be useful, but WITHOUT ANY          }
{    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          }
{    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 }

{    You should have received a copy of the GNU General Public License along with             }
{    this program; if not, write to the Free Software Foundation, Inc.,                       }
{    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    }

{#############################################################################################}

unit FreeProjectSettingsDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, //LMessages,
{$ENDIF}
  //Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  FreeGeometry,
  FreeShipUnit,
  FreeLanguageSupport,
  Buttons, ComCtrls, Spin;

type

  { TFREEProjectSettingsDialog }

  TFREEProjectSettingsDialog = class(TForm)
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ComboBox1: TComboBox;
    Edit2: TFloatSpinEdit;
    Edit25: TFloatSpinEdit;
    Edit26: TFloatSpinEdit;
    Edit27: TFloatSpinEdit;
    Edit3: TFloatSpinEdit;
    Edit4: TFloatSpinEdit;
    Edit5: TFloatSpinEdit;
    Edit6: TFloatSpinEdit;
    fseMainframeLocation: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    lbWaterDensity: TLabel;
    Label13: TLabel;
    Label14: TLabel;
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
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel3: TPanel;
    ColorDialog: TColorDialog;
    BitBtn1: TSpeedButton;
    BitBtn2: TSpeedButton;
    PageControl1: TPageControl;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PrecisionBox: TComboBox;
    TabSheet1: TTabSheet;
    Panel: TPanel;
    Label1: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Edit1: TEdit;
    Edit7: TEdit;
    cbShadeUnderwater: TCheckBox;
    Panel4: TPanel;
    Unitbox: TComboBox;
    Edit9: TEdit;
    Edit10: TEdit;
    cbSavePreviewImage: TCheckBox;
    TabSheet2: TTabSheet;
    Panel2: TPanel;
    TabSheet3: TTabSheet;
    Panel5: TPanel;
    Edit17: TEdit;
    cbSimplifyIntersections: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    procedure Edit2EditingDone(Sender: TObject);
    procedure Edit3EditingDone(Sender: TObject);
    procedure Edit4EditingDone(Sender: TObject);
    procedure Edit5EditingDone(Sender: TObject);
    procedure Edit25EditingDone(Sender: TObject);
    procedure Edit6EditingDone(Sender: TObject);
    procedure Panel4Click(Sender: TObject);
    procedure UnitboxChange(Sender: TObject);
    procedure MainframeLocationEditingDone(Sender: TObject);
    procedure Edit26EditingDone(Sender: TObject);
    procedure Edit27EditingDone(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox11Click(Sender: TObject);
    procedure CheckBox12Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private   { Private declarations }
    ConversionFactor: double;
    function FGetConversionFactor: double;
    function FGetBeam: double;
    procedure FSetBeam(Val: double);
    function FGetCoefficient: double;
    procedure FSetCoefficient(Val: double);
    function FGetDensity: double;
    procedure FSetDensity(Val: double);
    function FGetTemper: double;
    procedure FSetTemper(Val: double);
    function FGetDraft: double;
    procedure FSetDraft(Val: double);
    function FGetLength: double;
    procedure FSetLength(Val: double);
    function FGetMainframe: double;
    procedure FSetMainframe(Val: double);
    function FGetYWindAreaMax: double;
    procedure FSetYWindAreaMax(Val: double);
    function FGetXWindAreaMax: double;
    procedure FSetXWindAreaMax(Val: double);
    procedure FSetUnitCaptions;
  public    { Public declarations }
    function Execute: boolean;
    property Beam: double
      read FGetBeam write FSetBeam;
    property Coefficient: double
      read FGetCoefficient write FSetCoefficient;
    property Density: double
      read FGetDensity write FSetDensity;
    property Temper: double
      read FGetTemper write FSetTemper;
    property Draft: double
      read FGetDraft write FSetDraft;
    property Length: double
      read FGetLength write FSetLength;
    property Mainframe: double
      read FGetMainframe write FSetMainframe;
    property YWindAreaMax: double
      read FGetYWindAreaMax write FSetYWindAreaMax;
    property XWindAreaMax: double
      read FGetXWindAreaMax write FSetXWindAreaMax;
  end;

var
  FREEProjectSettingsDialog: TFREEProjectSettingsDialog;

implementation


{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFREEProjectSettingsDialog.FGetConversionFactor: double;
begin
  if Unitbox.ItemIndex = 1 then
    Result := 1 / 0.3048
  else
    Result := 1.0;
end;{TFREEProjectSettingsDialog.FGetConversionFactor}

function TFREEProjectSettingsDialog.FGetBeam: double;
begin
{  if Edit3.Text = '' then
    Result := 0.0
  else
    Result := StrToFloat(Edit3.Text); }
 Result := Edit3.Value;
end;{TFREEProjectSettingsDialog.FGetBeam}

procedure TFREEProjectSettingsDialog.FSetBeam(Val: double);
begin
  //Edit3.Text := FloatToStrF(Val, ffFixed, 7, 4);
  Edit3.Value:=Val;
end;{TFREEProjectSettingsDialog.FSetBeam}

function TFREEProjectSettingsDialog.FGetCoefficient: double;
begin
  {if Edit6.Text = '' then
    Result := 0.0
  else
    Result := StrToFloat(Edit6.Text); }
  Result := Edit6.Value;
end;{TFREEProjectSettingsDialog.FGetCoefficient}

procedure TFREEProjectSettingsDialog.FSetCoefficient(Val: double);
begin
  //Edit6.Text := FloatToStrF(Val, ffFixed, 7, 4);
  Edit6.Value:=Val;
end;{TFREEProjectSettingsDialog.FSetCoefficient}

function TFREEProjectSettingsDialog.FGetDensity: double;
begin
  {if Edit5.Text = '' then
    Result := 0.0
  else
    Result := StrToFloat(Edit5.Text);}
  Result := Edit5.Value;
end;{TFREEProjectSettingsDialog.FGetDensity}

procedure TFREEProjectSettingsDialog.FSetDensity(Val: double);
begin
  //Edit5.Text := FloatToStrF(Val, ffFixed, 7, 4);
  Edit5.Value := Val;
end;{TFREEProjectSettingsDialog.FSetDensity}

function TFREEProjectSettingsDialog.FGetTemper: double;
begin
  {if Edit25.Text = '' then
    Result := 0.0
  else
    Result := StrToFloat(Edit25.Text);}
  Result := Edit25.Value;
end;{TFREEProjectSettingsDialog.FGetTemper}

procedure TFREEProjectSettingsDialog.FSetTemper(Val: double);
begin
  //Edit25.Text := FloatToStrF(Val, ffFixed, 7, 4);
  Edit25.Value := Val;
end;{TFREEProjectSettingsDialog.FSetTemper}

function TFREEProjectSettingsDialog.FGetDraft: double;
begin
  {if Edit4.Text = '' then
    Result := 0.0
  else
   Result := StrToFloat(Edit4.Text);}
  Result := Edit4.Value;
end;{TFREEProjectSettingsDialog.FGetDraft}

procedure TFREEProjectSettingsDialog.FSetDraft(Val: double);
begin
  //Edit4.Text := FloatToStrF(Val, ffFixed, 7, 4);
  Edit4.Value := Val;
end;{TFREEProjectSettingsDialog.FSetDraft}

function TFREEProjectSettingsDialog.FGetLength: double;
begin
  {if Edit2.Text = '' then
    Result := 0.0
  else
    Result := StrToFloat(Edit2.Text);}
  Result := Edit2.Value;
end;{TFREEProjectSettingsDialog.FGetLength}

procedure TFREEProjectSettingsDialog.FSetLength(Val: double);
begin
  //Edit2.Text := FloatToStrF(Val, ffFixed, 7, 4);
  Edit2.Value:=Val;
  if Checkbox2.Checked then
    Mainframe := 0.5 * Length;
  if Checkbox12.Checked then
    XWindAreaMax := 0.5 * Length;
end;{TFREEProjectSettingsDialog.FSetLength}

function TFREEProjectSettingsDialog.FGetMainframe: double;
begin
  {if fseMainframeLocation.Text = '' then
    Result := 0.0
  else
    Result := StrToFloat(fseMainframeLocation.Text);}
  Result := fseMainframeLocation.Value;
end;{TFREEProjectSettingsDialog.FGetMainframe}

procedure TFREEProjectSettingsDialog.FSetMainframe(Val: double);
begin
  //fseMainframeLocation.Text := FloatToStrF(Val, ffFixed, 7, 4);
  fseMainframeLocation.Value := Val;
end;{TFREEProjectSettingsDialog.FSetMainframe}

function TFREEProjectSettingsDialog.FGetYWindAreaMax: double;
begin
  {if Edit26.Text = '' then
    Result := 0.0
  else
    Result := StrToFloat(Edit26.Text);}
  Result := Edit26.Value;
end;{TFREEProjectSettingsDialog.FGetYWindAreaMax}

procedure TFREEProjectSettingsDialog.FSetYWindAreaMax(Val: double);
begin
  //Edit26.Text := FloatToStrF(Val, ffFixed, 7, 4);
  Edit26.Value := Val;
end;{TFREEProjectSettingsDialog.FSetYWindAreaMax}

function TFREEProjectSettingsDialog.FGetXWindAreaMax: double;
begin
  {if Edit27.Text = '' then
    Result := 0.0
  else
    Result := StrToFloat(Edit27.Text); }
  Result := Edit27.Value;
end;{TFREEProjectSettingsDialog.FGetXWindAreaMax}

procedure TFREEProjectSettingsDialog.FSetXWindAreaMax(Val: double);
begin
  //Edit27.Text := FloatToStrF(Val, ffFixed, 7, 4);
  Edit27.Value := Val;
end;{TFREEProjectSettingsDialog.FSetXWindAreaMax}

procedure TFREEProjectSettingsDialog.FSetUnitCaptions;
var
  Str: string;
begin
  if UnitBox.ItemIndex = 1 then
    Str := LengthStr(fuImperial)
  else
    Str := Lengthstr(fuMetric);
  Label9.Caption := Str;
  Label10.Caption := Str;
  Label11.Caption := Str;
  Label14.Caption := Str;
  Label21.Caption := Str;
  Label23.Caption := Str;
  if UnitBox.ItemIndex = 1 then
    Str := DensityStr(fuImperial)
  else
    Str := DensityStr(fuMetric);
  lbWaterDensity.Caption := Str;
end;{TFREEProjectSettingsDialog.FSetUnitCaptions}

function TFREEProjectSettingsDialog.Execute: boolean;
begin
  Conversionfactor := FGetConversionFactor;
  checkbox11.Checked := True;
  checkbox12.Checked := True;
  YWindAreaMax := 0.0;
  XWindAreaMax := 0.5 * Length;
  FSetUnitCaptions;

  GlobalFreeship.Preferences.LoadImageIntoBitmap(BitBtn1.Glyph, 'Ok');
  GlobalFreeship.Preferences.LoadImageIntoBitmap(BitBtn2.Glyph, 'Cancel');
  ShowTranslatedValues(Self);

  ShowModal;
  Result := Modalresult = mrOk;
end;{TFREEProjectSettingsDialog.Execute}

procedure TFREEProjectSettingsDialog.Edit2EditingDone(Sender: TObject);
begin
  Length := Length; // force repaint
end;{TFREEProjectSettingsDialog.Edit2Exit}

procedure TFREEProjectSettingsDialog.Edit3EditingDone(Sender: TObject);
begin
  Beam := Beam;
end;{TFREEProjectSettingsDialog.Edit3Exit}

procedure TFREEProjectSettingsDialog.Edit4EditingDone(Sender: TObject);
begin
  Draft := Draft;
end;{TFREEProjectSettingsDialog.Edit4Exit}

procedure TFREEProjectSettingsDialog.Edit5EditingDone(Sender: TObject);
begin
  Density := Density;
end;{TFREEProjectSettingsDialog.Edit5Exit}

procedure TFREEProjectSettingsDialog.Edit25EditingDone(Sender: TObject);
begin
  Temper := Temper;
end;{TFREEProjectSettingsDialog.Edit25Exit}

procedure TFREEProjectSettingsDialog.Edit6EditingDone(Sender: TObject);
begin
  Coefficient := Coefficient;
end;{TFREEProjectSettingsDialog.Edit6Exit}

procedure TFREEProjectSettingsDialog.Panel4Click(Sender: TObject);
begin
  ColorDialog.Color := panel4.Color;
  if ColorDialog.Execute then
  begin
    Panel4.Color := ColorDialog.Color;
  end;
end;{TFREEProjectSettingsDialog.Panel4Click}

procedure TFREEProjectSettingsDialog.UnitboxChange(Sender: TObject);
begin

  Length := Length / ConversionFactor;
  Beam := Beam / Conversionfactor;
  Draft := Draft / Conversionfactor;
  Mainframe := MainFrame / ConversionFactor;
  YWindAreaMax := YWindAreaMax / ConversionFactor;
  XWindAreaMax := XWindAreaMax / ConversionFactor;
  if (Unitbox.ItemIndex = 0) and (Conversionfactor > 1) then
    Density := Density / WeightConversionFactor;
  if (Unitbox.ItemIndex = 1) and (Conversionfactor = 1) then
    Density := Density * WeightConversionFactor;

  Conversionfactor := FGetConversionFactor;

  Length := Length * ConversionFactor;
  Beam := Beam * Conversionfactor;
  Draft := Draft * Conversionfactor;
  if not checkbox2.Checked then
    Mainframe := MainFrame * ConversionFactor;
  if not checkbox11.Checked then
    YWindAreaMax := YWindAreaMax * ConversionFactor;
  if not checkbox12.Checked then
    XWindAreaMax := XWindAreaMax * ConversionFactor;
  FSetUnitCaptions;

end;{TFREEProjectSettingsDialog.UnitboxClick}

procedure TFREEProjectSettingsDialog.MainframeLocationEditingDone(Sender: TObject);
begin
  Mainframe := Mainframe;
end;{TFREEProjectSettingsDialog.Edit8Exit}


procedure TFREEProjectSettingsDialog.Edit26EditingDone(Sender: TObject);
begin
  YWindAreaMax := YWindAreaMax;
end;{TFREEProjectSettingsDialog.Edit26Exit}



procedure TFREEProjectSettingsDialog.Edit27EditingDone(Sender: TObject);
begin
  XWindAreaMax := XWindAreaMax;
end;{TFREEProjectSettingsDialog.Edit27Exit}



procedure TFREEProjectSettingsDialog.CheckBox2Click(Sender: TObject);
begin
  if Checkbox2.Checked then
  begin
    //      Label13.Enabled:=False;
    //      Label14.Enabled:=False;
    fseMainframeLocation.Color := clBtnFace;
    fseMainframeLocation.Font.Color := clDkGray;
    fseMainframeLocation.Enabled := False;
  end
  else
  begin
    //      Label13.Enabled:=True;
    //      Label14.Enabled:=True;
    fseMainframeLocation.Color := clWindow;
    fseMainframeLocation.Font.Color := clBlack;
    fseMainframeLocation.Enabled := True;
  end;
end;{TFREEProjectSettingsDialog.CheckBox2Click}

procedure TFREEProjectSettingsDialog.CheckBox11Click(Sender: TObject);
begin
  if Checkbox11.Checked then
  begin
    //      Label20.Enabled:=False;
    //      Label21.Enabled:=False;
    Edit26.Color := clBtnFace;
    Edit26.Font.Color := clDkGray;
    Edit26.Enabled := False;
  end
  else
  begin
    //      Label20.Enabled:=True;
    //      Label21.Enabled:=True;
    Edit26.Color := clWindow;
    Edit26.Font.Color := clBlack;
    Edit26.Enabled := True;
  end;
end;{TFREEProjectSettingsDialog.CheckBox11Click}

procedure TFREEProjectSettingsDialog.CheckBox12Click(Sender: TObject);
begin
  if Checkbox12.Checked then
  begin
    //      Label22.Enabled:=False;
    //      Label23.Enabled:=False;
    Edit27.Color := clBtnFace;
    Edit27.Font.Color := clDkGray;
    Edit27.Enabled := False;
  end
  else
  begin
    //      Label22.Enabled:=True;
    //      Label23.Enabled:=True;
    Edit27.Color := clWindow;
    Edit27.Font.Color := clBlack;
    Edit27.Enabled := True;
  end;
end;{TFREEProjectSettingsDialog.CheckBox12Click}


procedure TFREEProjectSettingsDialog.BitBtn1Click(Sender: TObject);
begin
  Modalresult := mrOk;
end;{TFREEProjectSettingsDialog.BitBtn1Click}

procedure TFREEProjectSettingsDialog.BitBtn2Click(Sender: TObject);
begin
  Modalresult := mrCancel;
end;{TFREEProjectSettingsDialog.BitBtn2Click}

procedure TFREEProjectSettingsDialog.FormShow(Sender: TObject);
begin
  Pagecontrol1.ActivePage := Tabsheet1;
  ActiveControl := Edit1;

end;{TFREEProjectSettingsDialog.FormShow}

end.
