{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }

{    Copyright © 2010, by Timoshenko Victor F.                                                }
{    e-mail                  : vftim@rambler.ru, tvf@pisem.net                                }
{    FREE!ship project page  : http://freeship-plus.land.ru                                   }
{    FREE!ship homepage      : http://freeship-plus.pisem.su                                  }

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
unit FreeResistance_FungLeibDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
  TeEngine,
  Series,
  TeeProcs,
  Chart,
{$ELSE}
  LCLIntf, LCLType,
  TAChartTeeChart,
  TATools, TASeries, TACustomSeries, TAGraph, TAChartUtils,
  TAChartAxis, TAChartAxisUtils,
  PrintersDlgs, Printer4Lazarus, FreePrinter,
       {$IFDEF VER3}
  LazUTF8,
  LazFileUtils,
     {$ELSE}
  FileUtil, //deprecated
     {$ENDIF}

{$ENDIF}
  SysUtils,
  Graphics,
  Forms,
  Spin,
  Dialogs,
  StdCtrls,
  ComCtrls,
  Controls,
  Buttons,
  Classes,
  Printers,
  FreeTypes,
  FreeGeometry,
  FreeshipUnit,
  Math,
  ExtCtrls,
  ImgList,
  Menus;

{$IFDEF FPC}
const
  clTeeColor = clTAColor;
{$ENDIF}

type

  { TFreeResistance_FungLeib }

  TFreeResistance_FungLeib = class(TForm)
    A10Box: TFloatSpinEdit;
    A11Box: TComboBox;
    A1Box: TFloatSpinEdit;
    A2Box: TFloatSpinEdit;
    A3Box: TFloatSpinEdit;
    A4Box: TFloatSpinEdit;
    A5Box: TFloatSpinEdit;
    A6Box: TFloatSpinEdit;
    A7Box: TFloatSpinEdit;
    A8Box: TFloatSpinEdit;
    A9Box: TFloatSpinEdit;
    BABox: TFloatSpinEdit;
    BwlBox: TFloatSpinEdit;
    CheckBox2: TCheckBox;
    CpBox: TFloatSpinEdit;
    CstrnBox: TFloatSpinEdit;
    DensityBox: TFloatSpinEdit;
    DisplacementBox: TFloatSpinEdit;
    DpBox: TFloatSpinEdit;
    DraftBox: TFloatSpinEdit;
    DraftTotalBox: TFloatSpinEdit;
    EndSpeedBox: TFloatSpinEdit;
    Estimate2Box: TCheckBox;
    EstimateBox: TCheckBox;
    GeneralPanel1: TPanel;
    GeneralPanel2: TPanel;
    GeneralPanel3: TPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    K1Box: TFloatSpinEdit;
    K2Box: TFloatSpinEdit;
    K3Box: TFloatSpinEdit;
    K4Box: TFloatSpinEdit;
    K5Box: TFloatSpinEdit;
    K6Box: TFloatSpinEdit;
    K7Box: TFloatSpinEdit;
    KBulbBox: TFloatSpinEdit;
    KeBox: TFloatSpinEdit;
    KeelAreaBox: TFloatSpinEdit;
    KeelChordLengthbox: TFloatSpinEdit;
    KsBox: TFloatSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
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
    Label28: TLabel;
    Label281: TLabel;
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
    Label40: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label61: TLabel;
    Label6_1: TLabel;
    Label6_10: TLabel;
    Label6_11: TLabel;
    Label6_2: TLabel;
    Label6_3: TLabel;
    Label6_4: TLabel;
    Label6_5: TLabel;
    Label6_6: TLabel;
    Label6_7: TLabel;
    Label6_8: TLabel;
    Label6_9: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LCBBox: TFloatSpinEdit;
    LoSbox: TFloatSpinEdit;
    LwlBox: TFloatSpinEdit;
    NpBox: TFloatSpinEdit;
    Panel10: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel7: TPanel;
    Panel9: TPanel;
    PrintDialog: TPrintDialog;
    PageControl1: TPageControl;
    General: TTabSheet;
    Panel1: TPanel;
    Panel6: TPanel;
    Data2: TTabSheet;


    Label_28: TLabel;
    Label_16: TLabel;
    RudderAreaBox: TFloatSpinEdit;
    RudderChordLengthbox: TFloatSpinEdit;
    StartSpeedBox: TFloatSpinEdit;
    StepSpeedBox: TFloatSpinEdit;
    ViscosityBox: TFloatSpinEdit;
    WettedSurfacebox: TFloatSpinEdit;
    Results: TTabSheet;
    Results2: TTabSheet;
    Panel5: TPanel;
    Panel15: TPanel;
    Resultsmemo: TMemo;
    Resultsmemo2: TMemo;
    Chart: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Series3: TLineSeries;
    Series4: TLineSeries;
    ToolBar1: TToolBar;
    ToolButton20: TToolButton;
    ZBulbBox: TFloatSpinEdit;
    _Label10: TLabel;
    _Label11: TLabel;
    _Label12: TLabel;
    _Label14: TLabel;
    _Label15: TLabel;
    _Label16: TLabel;
    _Label31: TLabel;
    _Label32: TLabel;
    _Label33: TLabel;
    _Label36: TLabel;
    _Label8: TLabel;
    _Label9: TLabel;
    _ToolButton10: TToolButton;
    _ToolButton14: TToolButton;
    ToolButton25: TToolButton;
    ToolButton7: TToolButton;
    MenuImages: TImageList;
    PrintButton: TToolButton;
    _Label17: TLabel;
    _Label18: TLabel;
    _Label19: TLabel;
    _Label20: TLabel;
    _Label30: TLabel;
    _Label34: TLabel;
    _Label37: TLabel;
    procedure File_ExportData(dat, dan: array of single);
    procedure File_ExportDataKe(dat, dan: array of single);
    procedure CheckBox2Click(Sender: TObject);
    procedure ToolButton25Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton20Click(Sender: TObject);
    procedure PrintButtonClick(Sender: TObject);
    procedure DraftTotalBoxAfterSetValue(
      Sender: TObject);
    procedure StartSpeedBoxAfterSetValue(
      Sender: TObject);
    procedure LwlBoxAfterSetValue(Sender: TObject);
    procedure KeelChordLengthboxAfterSetValue(
      Sender: TObject);
    procedure EstimateBoxClick(Sender: TObject);
    procedure Estimate2BoxClick(Sender: TObject);
  private
    FFreeship: TFreeship;
    B_T, L_D, A_D, L_B, Vsr, Rtotal: single;
    Am, Cm, Cwp, S_, Ie_, Ie, Z0, Tc: single;
    Ae0, P_D0, Nver: single;
    procedure CalculateResistanceFungLeib(
      ConvertedSpeed, LCB, Cp: single; var Rf, Rr, w, t0, nr: single);
    function FGetStartSpeed: single;
    procedure FSetStartSpeed(val: single);
    function FGetEndSpeed: single;
    procedure FSetEndSpeed(val: single);
    function FGetDensity: single;
    procedure FSetDensity(val: single);
    function FGetDisplacement: single;
    procedure FSetDisplacement(val: single);
    function FGetDraft: single;
    procedure FSetDraft(val: single);
    function FGetDraftTotal: single;
    procedure FSetDraftTotal(val: single);
    function FGetLwl: single;
    procedure FSetLwl(val: single);
    function FGetLCB: single;
    procedure FSetLCB(val: single);
    function FGetKe: single;
    procedure FSetKe(val: single);
    function FGetBA: single;
    procedure FSetBA(val: single);
    function FGetKBulb: single;
    procedure FSetKBulb(val: single);
    function FGetZBulb: single;
    procedure FSetZBulb(val: single);
    function FGetCstrn: single;
    procedure FSetCstrn(val: single);
    function FGetNp: single;
    procedure FSetNp(val: single);
    function FGetDp: single;
    procedure FSetDp(val: single);
    function FGetKs: single;
    procedure FSetKs(val: single);
    function FGetKeelChordLength: single;
    procedure FSetKeelChordLength(val: single);
    function FGetKeelArea: single;
    procedure FSetKeelArea(val: single);
    function FGetRudderChordLength: single;
    procedure FSetRudderChordLength(val: single);
    function FGetRudderArea: single;
    procedure FSetRudderArea(val: single);
    function FGetBwl: single;
    procedure FSetBwl(val: single);
    function FGetCp: single;
    procedure FSetCp(val: single);
    function FGetViscosity: single;
    procedure FSetViscosity(val: single);
    function FGetWettedSurface: single;
    procedure FSetWettedSurface(val: single);
    function FGetLos: single;
    procedure FSetLos(val: single);
    function FGetStepSpeed: single;
    procedure FSetStepSpeed(val: single);
    function FGetExtractFromHull: boolean;
    procedure FSetExtractFromHull(Val: boolean);
    function FGetK1: single;
    procedure FSetK1(val: single);
    function FGetK2: single;
    procedure FSetK2(val: single);
    function FGetK3: single;
    procedure FSetK3(val: single);
    function FGetK4: single;
    procedure FSetK4(val: single);
    function FGetK5: single;
    procedure FSetK5(val: single);
    function FGetK6: single;
    procedure FSetK6(val: single);
    function FGetK7: single;
    procedure FSetK7(val: single);

    function FGetA1: single;
    procedure FSetA1(val: single);
    function FGetA2: single;
    procedure FSetA2(val: single);
    function FGetA3: single;
    procedure FSetA3(val: single);
    function FGetA4: single;
    procedure FSetA4(val: single);
    function FGetA5: single;
    procedure FSetA5(val: single);
    function FGetA6: single;
    procedure FSetA6(val: single);
    function FGetA7: single;
    procedure FSetA7(val: single);
    function FGetA8: single;
    procedure FSetA8(val: single);
    function FGetA9: single;
    procedure FSetA9(val: single);
    function FGetA10: single;
    procedure FSetA10(val: single);
    function FGetA11: integer;
    procedure FSetA11(val: integer);

  public
    PathFile, PathFileOld, FileToFind, FileName: string;
    Wlarea: single;
    function CorrectInputdata: boolean;
    procedure Calculate;
    function Execute(Freeship: TFreeship;
      AutoExtract: boolean): boolean;
    property Bwl: single
      read FGetBwl write FSetBwl;
    property Cp: single
      read FGetCp write FSetCp;
    property Density: single
      read FGetDensity write FSetDensity;
    property Displacement: single
      read FGetDisplacement write FSetDisplacement;
    property Draft: single
      read FGetDraft write FSetDraft;
    property DraftTotal: single
      read FGetDraftTotal write FSetDraftTotal;
    property EndSpeed: single
      read FGetEndSpeed write FSetEndSpeed;
    property ExtractFromHull: boolean
      read FGetExtractFromHull write FSetExtractFromHull;
    property KeelChordLength: single
      read FGetKeelChordLength write FSetKeelChordLength;
    property KeelArea: single
      read FGetKeelArea write FSetKeelArea;
    property LCB: single
      read FGetLCB write FSetLCB;
    property Ke: single
      read FGetKe write FSetKe;
    property K1: single
      read FGetK1 write FSetK1;
    property K2: single
      read FGetK2 write FSetK2;
    property K3: single
      read FGetK3 write FSetK3;
    property K4: single
      read FGetK4 write FSetK4;
    property K5: single
      read FGetK5 write FSetK5;
    property K6: single
      read FGetK6 write FSetK6;
    property K7: single
      read FGetK7 write FSetK7;
    property A1: single
      read FGetA1 write FSetA1;
    property A2: single
      read FGetA2 write FSetA2;
    property A3: single
      read FGetA3 write FSetA3;
    property A4: single
      read FGetA4 write FSetA4;
    property A5: single
      read FGetA5 write FSetA5;
    property A6: single
      read FGetA6 write FSetA6;
    property A7: single
      read FGetA7 write FSetA7;
    property A8: single
      read FGetA8 write FSetA8;
    property A9: single
      read FGetA9 write FSetA9;
    property A10: single
      read FGetA10 write FSetA10;
    property A11: integer
      read FGetA11 write FSetA11;
    property BA: single
      read FGetBA write FSetBA;
    property KBulb: single
      read FGetKBulb write FSetKBulb;
    property ZBulb: single
      read FGetZBulb write FSetZBulb;
    property Cstrn: single
      read FGetCstrn write FSetCstrn;
    property Np: single
      read FGetNp write FSetNp;
    property Dp: single
      read FGetDp write FSetDp;
    property Ks: single
      read FGetKs write FSetKs;
    property Lwl: single
      read FGetLwl write FSetLwl;
    property RudderChordLength: single
      read FGetRudderChordLength write FSetRudderChordLength;
    property RudderArea: single
      read FGetRudderArea write FSetRudderArea;
    property StartSpeed: single
      read FGetStartSpeed write FSetStartSpeed;
    property StepSpeed: single
      read FGetStepSpeed write FSetStepSpeed;
    property Viscosity: single
      read FGetViscosity write FSetViscosity;
    property WettedSurface: single
      read FGetWettedSurface write FSetWettedSurface;
    property Los: single
      read FGetLos write FSetLos;
  end;

var
  FreeResistance_FungLeib: TFreeResistance_FungLeib;

implementation

uses FreeStringsUnit, FreeProcess;

{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeResistance_FungLeib.CorrectInputdata: boolean;
begin
  Result := False;

  if (Draft <= 0.02) then
    Draft := 0.021;
  if (Lwl <= 0) then
    LwlBox.Color := clYellow
  else
    LwlBox.Color := clDefault;
  if (Bwl <= 0) then
    BwlBox.Color := clYellow
  else
    BwlBox.Color := clDefault;
  if (DraftTotal < Draft) then
    Draft := DraftTotal;
  if (WettedSurface <= 0) then
    WettedSurfaceBox.Color := clYellow
  else
    WettedSurfaceBox.Color := clDefault;
  if (Displacement <= 0) then
    DisplacementBox.Color := clYellow
  else
    DisplacementBox.Color := clDefault;
  if (Cp <= 0) then
    CpBox.Color := clYellow
  else
    CpBox.Color := clDefault;
  if (Viscosity <= 0) then
    ViscosityBox.Color := clYellow
  else
    ViscosityBox.Color := clDefault;
  if (EndSpeed <= 0.0) then
    EndSpeed := 10.0;
  if (EndSpeed <= StartSpeed) then
    EndSpeedBox.Color := clYellow
  else
    EndSpeedBox.Color := clDefault;
  if (StepSpeed <= 0) then
    StepSpeed := 10.0;

  if (DraftTotal <= 0.02) or (Lwl <= 0) or (Bwl <= 0) or (Los <= 0) or
    (Displacement <= 0) or (Cp <= 0) or (Viscosity <= 0) or (EndSpeed <= StartSpeed) then
    exit;

  if Tc = 0 then
    Tc := DraftTotal;
  B_T := Bwl / Tc;
  L_D := Lwl / power(Displacement, 1 / 3);
  A_D := WlArea / power(Displacement, 2 / 3);
  L_B := Lwl / Bwl;
  Cm := Cp * (Lwl * Bwl * Tc) / Displacement;
  Am := Cm * Tc * Bwl;
  if Cm >= 1. then
  begin
    Am := Displacement / (Lwl * Cp);
    Cm := Am / (Bwl * Tc);
  end;
  Cwp := WlArea / (Lwl * Bwl);
  if (Cwp = 1) or (Cp = 1) then
  begin
    MessageDlg('Error: Cp=1 or Cwp=1', mtError, [mbOK], 0);
    Exit;
  end;

  Result := True;
end;{TFreeResistance_FungLeib.CorrectInputdata}

procedure TFreeResistance_FungLeib.Calculate;
var
  ConvertedSpeed: single;
  FroudeNumber: single;
  Rf, Rr, Rt, flag: single;
  w, t0, nr, S, Ae_Ao: single;
  P_D, Z, K, Tb, Ta, Dp_: single;
  Kdt, Vms, x, Tmax: single;
  Psi, Lpp, Cb, Fr_max: single;
  index: integer;
  CPopt, LCBopt: array of single;
  Speed, StepSpeed1: single;
  Lr, L, DLR, Bt, Tarea, Tw: single;
  Line, Tmp, Str: string;
  Units: TFreeUnitType;
  Stop: boolean;
  I, II, ispeed: integer;
  ffile: textfile;
  Vsr, C, Ke_: single;
  Vs: array[1..5] of single;
  Res: array[1..11, 1..11] of single;
  Rr_min: array[1..11] of single;
  SS: array[1..11] of string;
  pathFile, FileToFind: string;
  PathFileOld: string;
  FOpenDirectory: string;
  FExecDirectory, ExecFullName: string;
  ParamFileName: string = 'fungdata.dat';
  ResultFileName: string = 'fungleib.res';
label
  NewSearch;
label
  NewSearch1;
begin
  Vs[1] := StartSpeed;
  Vs[4] := EndSpeed;
  Vs[5] := StepSpeed;
  Vs[2] := Vs[1] + (Vs[4] - Vs[1]) / 3;
  Vs[3] := Vs[1] + (Vs[4] - Vs[1]) * 2 / 3;
  Vsr := EndSpeed;

  Series1.Clear;
  Series2.Clear;
  Series3.Clear;
  Series4.Clear;
  ResultsMemo.Visible := True;
  ResultsMemo2.Visible := True;
  PrintButton.Enabled := False;
  // Вывод помощи для метода Fung-Leibman
  ResultsMemo2.Text := '';
  ResultsMemo2.Lines.Add(rs_1_Viscosity_by_default_is_for_sea_water_with_density_1025_kg_m_3_and_temperature_15__C_ {UserString[333]});
  ResultsMemo2.Lines.Add(rs_You_can_change_the_temperature_and_the_density__Project__Project_Settings__Hydrostatics {UserString[334]});
  ResultsMemo2.Lines.Add('');
  ResultsMemo2.Lines.Add(rs_2_The_trim_is_calculated_by_formulaes__dT_Tf_Ta__Tf_Ttdt_2__Ta_T_dt_2 {UserString[335]});
  ResultsMemo2.Lines.Add('');
  for i := 336 to 344 do
    ResultsMemo2.Lines.Add(Userstring(i));
  ResultsMemo2.Lines.Add('');
  ResultsMemo2.Lines.Add(rs_4_If_wetted_surface_area_S_is_set_as_0__then_S_to_calculate_by_formulae_from_method_of_Holtrop_ {UserString[345]});
  ResultsMemo2.Lines.Add('');
  for i := 1406 to 1419 do
    ResultsMemo2.Lines.Add(Userstring(i));
  ResultsMemo2.Lines.Add(rs_t_____________________________t______________________t {UserString[1407]});
  ResultsMemo2.Lines.Add('');
  ResultsMemo2.Lines.Add('Copyright (c) 2010, Timoshenko V.F.');
  ResultsMemo2.Visible := True;

  if CorrectInputdata then
  begin
    Units := FFreeship.ProjectSettings.ProjectUnits;
    ResultsMemo.Text := '';
    ResultsMemo.Lines.Add('');
    ResultsMemo.Lines.Add('');
    FFreeship.CreateOutputHeader(Space(10) + rs_Program_of_resistance_and_power_prediction_for_naval_ships__NUoS_Ukraine_2010_ {UserString[1248]} + '.', ResultsMemo.Lines);
    ResultsMemo.Lines.Add(
      '          ----------------------------------------------------------------------------------');
    ResultsMemo.Lines.Add('');
    ResultsMemo.Lines.Add('');
    ResultsMemo.Lines.Add(Space(10) + rs_Input_variables {UserString[250]});
    ResultsMemo.Lines.Add(
      '          ----------------------------------------------------------------------------------');
    ResultsMemo.Lines.Add('');



    ResultsMemo.Lines.Add(Space(10) + rs_General {UserString[251]});
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Start_speed {UserString[252]}, 40) +
      ' : ' + FloatToStrF(Vs[1], ffFixed, 6, 2) + ' ' + rs_kn {UserString[326]});
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_End_speed {UserString[253]}, 40) +
      ' : ' + FloatToStrF(Vs[5], ffFixed, 6, 2) + ' ' + rs_kn {UserString[326]});
    //      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Speed_step {UserString[254]},40)+' : '+FloatToStrF(StepSpeed,ffFixed,6,2)+' '+rs_kn {UserString[326]});
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Water_density {UserString[50]}, 40) +
      ' : ' + FloatToStrF(Density, ffFixed, 8, 3) + #32 + DensityStr(Units));
    if Units = fuImperial then
      ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Water_viscosity {UserString[255]}, 40) + ' : ' +
        FloatToStrF(Viscosity, ffFixed, 8, 4) + '*10^(-6) ' + rs_ft_2_s {UserString[471]})
    else
      ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Water_viscosity {UserString[255]}, 40) + ' : ' +
        FloatToStrF(Viscosity, ffFixed, 8, 4) + '*10^(-6) ' + rs_m_2_s {UserString[472]});
    ResultsMemo.Lines.Add('');
    ResultsMemo.Lines.Add(Space(10) + rs_Hull {UserString[256]});
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Length_on_waterline {UserString[17]}, 40) +
      ' : ' + FloatToStrF(Lwl, ffFixed, 6, 3) + #32 + LengthStr(Units));
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Length_over_surface_Los {UserString[843]}, 40) +
      ' : ' + FloatToStrF(Los, ffFixed, 6, 3) + #32 + LengthStr(Units));
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Beam_on_waterline {UserString[18]}, 40) +
      ' : ' + FloatToStrF(Bwl, ffFixed, 6, 3) + #32 + LengthStr(Units));
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Draft_on_midship {UserString[304]}, 40) +
      ' : ' + FloatToStrF(Tc, ffFixed, 6, 3) + #32 + LengthStr(Units));
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Draft_on_F_P_ {UserString[305]}, 40) +
      ' : ' + FloatToStrF(Tc + Draft / 2., ffFixed, 6, 3) + #32 + LengthStr(Units));
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Draft_on_F_A_ {UserString[306]}, 40) +
      ' : ' + FloatToStrF(Tc - Draft / 2., ffFixed, 6, 3) + #32 + LengthStr(Units));

    Stop := False;
    if (abs(Draft) / Lwl > 0.05) or (abs(Draft) > DraftTotal) then
    begin
      ResultsMemo.Lines.Add(Space(10) + rs_Warning____Very_big_trim__Tf_Ta_ {UserString[307]});
      Stop := True;
      //Exit;
    end;
    if EstimateBox.Checked then
      ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Wetted_surface_area {UserString[10]}, 40) + ' : ' +
        FloatToStrF(WettedSurface, ffFixed, 6, 2) + #32 + AreaStr(Units) + ' (' + rs_Estimated {UserString[266]} + ')')
    else
      ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Wetted_surface_area {UserString[10]}, 40) + ' : ' +
        FloatToStrF(WettedSurface, ffFixed, 6, 2) + #32 + AreaStr(Units));
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Waterplane_area {UserString[19]}, 40) +
      ' : ' + FloatToStrF(WlArea, ffFixed, 6, 2) + #32 + AreaStr(Units));
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Displacement {UserString[4]}, 40) +
      ' : ' + FloatToStrF(Displacement, ffFixed, 6, 3) + #32 + VolStr(Units));
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Longitudinal_center_of_buoyancy {UserString[11]}, 40) +
      ' : ' + FloatToStrF(LCB, ffFixed, 6, 3) + ' %');
    ResultsMemo.Lines.Add(Space(14) + Makelength(rs_Prismatic_coefficient {UserString[8]}, 40) +
      ' : ' + FloatToStrF(Cp, ffFixed, 6, 4));
    ResultsMemo.Lines.Add('');
(*
  if (K1+K2+K3+K4+K5+K6+K7+KeelChordLength+KeelArea+RudderChordLength+RudderArea>0) then begin
      ResultsMemo.Lines.Add(Space(10)+rs_Appendages_areas_ {UserString[322]});
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Shaft_brackets_area {UserString[314]},40)+' : '+FloatToStrF(K1,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Strut_bossing_area {UserString[316]},40)+' : '+FloatToStrF(K2,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Hull_bossing_area {UserString[317]},40)+' : '+FloatToStrF(K4,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Stabiliser_fins_area {UserString[321]},40)+' : '+FloatToStrF(K3,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Dome_area {UserString[311]},40)+' : '+FloatToStrF(K5,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Exposed_shafts_area_ {UserString[318]},40));
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Angle_vs_buttoks_about_10__ {UserString[319]},40)+' : '+FloatToStrF(K6,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Angle_vs_buttoks_about_20__ {UserString[320]},40)+' : '+FloatToStrF(K7,ffFixed,6,3)+#32+AreaStr(Units));

      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(14)+rs_Areas_of_keels {UserString[308]});
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Skeg {UserString[315]},40)+' : '+FloatToStrF(KeelChordLength,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Bilge_keels {UserString[310]},40)+' : '+FloatToStrF(KeelArea,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(14)+rs_Areas_of_rudders {UserString[309]});
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_Rudder_behind_skeg_stern {UserString[312]},40)+' : '+FloatToStrF(RudderChordLength,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(14)+Makelength(rs_2_screw_rudder_slender_thick {UserString[313]},40)+' : '+FloatToStrF(RudderArea,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add('');
  end;
*)
    if (A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10 + A11 + Ks > 0) and Estimate2Box.Checked then
    begin
      ResultsMemo.Lines.Add(Space(10) + Makelength(GroupBox3.Caption, 30));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label6_1.Caption, 45) +
        ' : ' + FloatToStrF(A1, ffFixed, 6, 1));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label6_2.Caption, 45) +
        ' : ' + FloatToStrF(A2, ffFixed, 6, 3));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label6_3.Caption, 45) +
        ' : ' + FloatToStrF(A3, ffFixed, 6, 2));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label6_4.Caption, 45) +
        ' : ' + FloatToStrF(A4, ffFixed, 6, 2));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label6_5.Caption, 45) +
        ' : ' + FloatToStrF(A5, ffFixed, 6, 2));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label6_6.Caption, 45) +
        ' : ' + FloatToStrF(A6, ffFixed, 6, 3));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label6_7.Caption, 45) +
        ' : ' + FloatToStrF(A7, ffFixed, 6, 3));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label6_8.Caption, 45) +
        ' : ' + FloatToStrF(A8, ffFixed, 6, 3));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label6_9.Caption, 45) +
        ' : ' + FloatToStrF(A9, ffFixed, 6, 2));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label6_10.Caption, 45) +
        ' : ' + FloatToStrF(A10, ffFixed, 6, 2));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label6_11.Caption, 45) +
        ' : ' + FloatToStrF(A11, ffFixed, 6, 0));
      ResultsMemo.Lines.Add(Space(14) + Makelength(Label17.Caption, 45) +
        ' : ' + FloatToStrF(Ks, ffFixed, 6, 0));
      ResultsMemo.Lines.Add('');
    end;
    ResultsMemo.Lines.Add('');
    ResultsMemo.Lines.Add(Space(10) + rs_Calculated_variables {UserString[263]});
    ResultsMemo.Lines.Add(
      '          ----------------------------------------------------------------------------------');
    ResultsMemo.Lines.Add('');

    if WettedSurface <= 0 then
    begin
      S := Lwl * (2 * Tc + Bwl) * sqrt(Cm) *
        (0.453 + 0.442 * Cp * Cm - 0.2862 * Cm - 0.003467 * Bwl / Tc + 0.3696 * Cwp) + 2.38 * KBulb / Cp * Cm;
      ResultsMemo.Lines.Add(Space(14) + 'S             = ' + FloatToStrF(
        S, ffFixed, 6, 2) + #32 + AreaStr(Units) + ' (' + rs_Estimated {UserString[266]} + ' ' + rs_by_a_method_Holtrop_1988 {UserString[630]} + ')');
      S_ := S;
    end;

    Lpp := FFreeship.ProjectSettings.ProjectLength;
    Tmax := FFreeship.ProjectSettings.ProjectDraft;
    if (Lpp = 1) and (Tmax = 1) then
    begin
      MessageDlg(rs_Do_not_set_main_dimensions_Lpp__B_and_T____ {UserString[1145]}, mtError, [mbOK], 0);
      exit;
    end;
    Cb := Cp * Cm;
    Ta := Tc - Draft / 2.;
    Bt := ZBulb;
    DLR := Displacement / power(Lwl / 100 / 0.3054545, 3) * 1.01633;
    Lr := Lpp * (1 - Cp + 0.06 * Cp * lcb / (4 * Cp - 1));
    Tarea := BA / (Bwl * Tc * Cm);
    Tw := Bt / Bwl;
    if Cwp = 0 then
      Cwp := Cp * Cm / (0.471 + 0.551 * Cp * Cm);
    if kbulb < 2.6 then
      kbulb := 1. + 89. * exp(-power(Lwl / Bwl, 0.80856) * power((1 - Cwp), 0.30484) * power(
        (1 - Cp - 0.0225 * lcb), 0.6367) * power((Lr / Bwl), 0.34574) * power(
        (100 * Displacement / Lwl / Lwl / Lwl), 0.16302));
    ResultsMemo.Lines.Add(Space(14) + 'Cp            = ' + FloatToStrF(Cp, ffFixed, 6, 4));
    ResultsMemo.Lines.Add(Space(14) + 'Cb            = ' + FloatToStrF(Cp * Cm, ffFixed, 6, 4));
    ResultsMemo.Lines.Add(Space(14) + 'Cwp           = ' + FloatToStrF(Cwp, ffFixed, 6, 4));
    ResultsMemo.Lines.Add(Space(14) + 'Cm            = ' + FloatToStrF(Cm, ffFixed, 6, 4));
    ResultsMemo.Lines.Add(Space(14) + 'Am            = ' + FloatToStrF(
      Am, ffFixed, 6, 2) + #32 + AreaStr(Units));
    ResultsMemo.Lines.Add(Space(14) + 'L/B           = ' + FloatToStrF(
      Lpp / Bwl, ffFixed, 6, 3));
    ResultsMemo.Lines.Add(Space(14) + 'ie            = ' + FloatToStrF(
      Kbulb, ffFixed, 6, 3) + ' ' + rs_degr {UserString[455]});
    ResultsMemo.Lines.Add(Space(14) + 'Tc            = ' + FloatToStrF(Tc, ffFixed, 6, 3) + ' m');
    ResultsMemo.Lines.Add(Space(14) + 'B/T           = ' + FloatToStrF(
      Bwl / Tc, ffFixed, 6, 3));
    ResultsMemo.Lines.Add(Space(14) + 'Lwl/T         = ' + FloatToStrF(
      Lwl / Tc, ffFixed, 6, 3));
    ResultsMemo.Lines.Add(Space(14) + 'DLR           = ' + FloatToStrF(DLR, ffFixed, 6, 3));
    ResultsMemo.Lines.Add(Space(14) + 'Ta            = ' + FloatToStrF(Tarea, ffFixed, 6, 3));
    ResultsMemo.Lines.Add(Space(14) + 'Tw            = ' + FloatToStrF(Tw, ffFixed, 6, 3));
    ResultsMemo.Lines.Add(Space(14) + 'Np            = ' + FloatToStrF(Np, ffFixed, 6, 0));
    ResultsMemo.Lines.Add('');

    // Проверка на валидность

    // Отношение длины к ширине, L/B          |  2.520 ... 17.935    |
    // Отношение ширины к осадке, B/T         |  1.696 ... 10.204    |
    // Призматический коэффициент, Cp         |  0.526 ... 0.774     |
    // Коэффициент полноты миделя, Cm         |  0.556 ... 0.994     |
    // Коэффициент полноты ватерлинии Cwp     |  0.662 ... 0.841     |
    // 1/2 угла носового заострения,  ie      |    2.6 ... 31.73 град|
    // Отношение V к L^3, DLR                 | 16.239 ... 359.18    |
    // Число Фруда, Fr                        |   0.15 ... 0.9       |

    Fr_max := Vs[5] * 0.514444 / sqrt(9.81 * (Lwl + Lpp) / 2);
    if (Lwl / Bwl > 18) or (Lwl / Bwl < 2.52) then
      ResultsMemo.Lines.Add(Space(14) + 'L/B        ' + rs_is_outside_valid_domain {UserString[476]} + '  2,52 ... 18');
    if (Bwl / Tc > 10.2) or (Bwl / Tc < 1.7) then
      ResultsMemo.Lines.Add(Space(14) + 'B/T        ' + rs_is_outside_valid_domain {UserString[476]} + '   1,7 ... 10,2');
    if (Cb / Cm > 0.774) or (Cb / Cm < 0.526) then
      ResultsMemo.Lines.Add(Space(14) + 'Cp         ' + rs_is_outside_valid_domain {UserString[476]} + ' 0,526 ... 0,774');
    if (Cm > 0.994) or (Cm < 0.556) then
      ResultsMemo.Lines.Add(Space(14) + 'Cm         ' + rs_is_outside_valid_domain {UserString[476]} + ' 0,556 ... 0,994');
    if (Cwp > 0.841) or (Cwp < 0.662) then
      ResultsMemo.Lines.Add(Space(14) + 'Cwp        ' + rs_is_outside_valid_domain {UserString[476]} + ' 0,662 ... 0,841');
    if (Kbulb > 32) or (Kbulb < 2.6) then
      ResultsMemo.Lines.Add(Space(14) + 'ie         ' + rs_is_outside_valid_domain {UserString[476]} +
        '   2,6 ... 31,73' + ' ' + rs_degr {UserString[455]});
    if (Tarea > 0.74) or (Tarea < 0) then
      ResultsMemo.Lines.Add(Space(14) + 'Ta         ' + rs_is_outside_valid_domain {UserString[476]} + '   0,0 ... 0,74');
    if (Tw > 1.0) or (Tw < 0) then
      ResultsMemo.Lines.Add(Space(14) + 'Tw         ' + rs_is_outside_valid_domain {UserString[476]} + '   0,0 ... 1,00');
    //     if (Tt>0.77) or (Tt<0) then                     ResultsMemo.Lines.Add(Space(14)+'Tt         '+rs_is_outside_valid_domain {UserString[476]}+'   0,0 ... 0,77');
    if (Fr_max > 0.9) or (Fr_max < 0.15) then
      ResultsMemo.Lines.Add(Space(14) + 'Fr         ' + rs_is_outside_valid_domain {UserString[476]} + '  0,15 ... 0,9');
    if (DLR > 360) or (DLR < 16) then
      ResultsMemo.Lines.Add(Space(14) + '10^4*V/L^3 ' + rs_is_outside_valid_domain {UserString[476]} + ' 16,24 ... 359,18');
    //         if (Np=0) or (Np>2) then ResultsMemo.Lines.Add(Space(14)+'Np        '+rs_is_outside_valid_domain {UserString[476]}+' 1 ... 2');
    Speed := Vs[4];
    ConvertedSpeed := Speed * 0.514444;

    PathFileOld := GetCurrentDir;
    ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
    SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);

    CalculateResistanceFungLeib(ConvertedSpeed, LCB, Cp, Rf, Rr, w, t0, nr);

    if (Lwl > 0) and (Bwl > 0) and (Dp >= 0) and (Np >= 0) and (Ta > 0) then
    begin
      PathFileOld := FFreeship.Preferences.InitDirectory; // каталог Freeshipa
      //  Определяем каталог с программой FungLeib.exe
      FExecDirectory := FFreeship.Preferences.ExecDirectory;

      //  Определяем текущий каталог с проектами и с данными для расчета IN.

      //  Определяем текущий каталог с проектами и с данными для расчета fungdata.dat
      FileToFind := FileSearchUTF8(ParamFileName, GetCurrentDir);
      { *Converted from FileSearch* }
      if FileToFind <> ParamFileName then
      begin
        MessageDlg(rs_Have_not_input_file_for_calculation_is_here___ {UserString[1229]}, mtError, [mbOK], 0);
        exit;
      end;

      //SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory + DirectorySeparator+'fungleib.EXE'), '', []);
      ExecFullName := FExecDirectory + DirectorySeparator+'fungleib.EXE';
      ExecuteFreePlugin(FFreeship.Preferences.TempDirectory, ExecFullName);

      //  Определяем есть ли файл с результатами расчета FUNGLEIB.RES. Если fungdata.dat присутствует значит расчет не закончен
      i := 1;
      NewSearch:
        FileToFind := FileSearchUTF8(ResultFileName, GetCurrentDir);
      { *Converted from FileSearch* }
      if FileToFind = PAramFIleName then
      begin
        sleep(200);
        i := i + 1;
        if i < 25 then
          goto NewSearch
        else
        begin
          MessageDlg(rs_Do_NOT_calculate_this_task__because_ {UserString[1138]} + #13#10#13#10 + Userstring(
            1139) + ' ' + FExecDirectory + DirectorySeparator+'fungleib.EXE' + ' ' + #13#10#13#10 + Userstring(
            1140) + #13#10#13#10 + rs_3__This_is_very_slow_computer__Fcpu___800_MHz_ {UserString[1141]} + #13#10#13#10 + rs_4__CPU_is_loaded_more_80__another_processes_ {UserString[1142]}, mtError, [mbOK], 0);
          if FileExistsUTF8('fungdata.dat') { *Converted from FileExists* } then
            DeleteFileUTF8('fungdata.dat'); { *Converted from DeleteFile* }
          exit;
        end;
      end;


      Assignfile(FFile, ResultFileName);
      //{$I-}
      Reset(FFile);
//{$I+}
      II := 0;
      Chart.Title.Text.Text := rs_Resistance_and_power_are_calculated {UserString[265]} + ' ' + rs_by_Fung_Leibman_s_method_1995 {UserString[1247]};
      while (not EOF(FFile)) and (II < 10) do
      begin
        II := II + 1;
        for I := 1 to 8 do
          try
            Read(FFile, res[I, II]);
          except
            res[I, II] := 0;
          end;
        if IsNaN(res[6, II]) then continue;
        if (res[6, II] < 0) then
        begin
          ResultsMemo.Lines.Add(Space(14) + rs_ATTENTION____This_is_series_NOT_correct_for_this_project_ {UserString[487]});
          MessageDlg(rs_ATTENTION____This_is_series_NOT_correct_for_this_project_ {UserString[487]}, mtError, [mbOK], 0);
          ResultsMemo.Visible := True;
          CloseFile(FFile);
          if FileExistsUTF8(ResultFileName)
          { *Converted from FileExists* } then
            DeleteFileUTF8(ResultFileName); { *Converted from DeleteFile* }
          exit;
        end;
      end;
      CloseFile(FFile);

      if FileExistsUTF8(ResultFileName) { *Converted from FileExists* } then
        DeleteFileUTF8(ResultFileName); { *Converted from DeleteFile* }

      //================================================================================
{ if Ke=0 then begin
    Vsr:=Vs[4]*0.51444;
    C:=res[7,9]/(0.5*Density*sqr(Vsr)*WettedSurface);
    FSex_fact(Vsr,C,Ke_);
    Ke:=Ke_;
   end;
}
      //================================================================================
      Rtotal := res[7, 9];
      //==========================================================================

      if Ke = 0 then
      begin
        //  Определяем каталог с программой Freeship.EXE
        FExecDirectory := FFreeship.Preferences.ExecDirectory;

        //  Определяем текущий каталог с проектами и с данными для расчета IN.
        FileToFind := FileSearchUTF8('TMPke.txt', GetCurrentDir);
        { *Converted from FileSearch* }
        if FileToFind <> 'TMPke.txt' then
        begin
          MessageDlg('Нет файла исходных данных для расчета!!!', mtError, [mbOK], 0);
          exit;
        end;

        // Запускаем программу расчета

        //SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory + DirectorySeparator+'SeaMargn.EXE'), '', []);
        ExecFullName := FExecDirectory + DirectorySeparator+'SeaMargn.EXE';
        ExecuteFreePlugin(FFreeship.Preferences.TempDirectory, ExecFullName);

        FileName := 'OUT.TXT';
        //  Определяем есть ли файл с результатами расчета OUT. Если TMPke.txt присутствует значит расчет не закончен
        i := 1;
        NewSearch1:
          FileToFind := FileSearchUTF8('TMPke.txt', GetCurrentDir); { *Converted from FileSearch* }
        if FileToFind = 'TMPke.txt' then
        begin
          sleep(200);
          i := i + 1;
          if i < 25 then
            goto NewSearch1
          else
          begin
            if FileExistsUTF8('TMPke.txt') { *Converted from FileExists* } then
              DeleteFileUTF8('TMPke.txt'); { *Converted from DeleteFile* }
            MessageDlg(rs_Do_NOT_calculate_this_task__because_ {UserString[1138]} +
              #13#10#13#10 + rs_1__Do_not_found_into__Exec_or_was_damaged_file {UserString[1139]} + ' SeaMargn.EXE ' + #13#10#13#10 + Userstring(
              1140) + #13#10#13#10 + rs_3__This_is_very_slow_computer__Fcpu___800_MHz_ {UserString[1141]} + #13#10#13#10 + rs_4__CPU_is_loaded_more_80__another_processes_ {UserString[1142]}, mtError, [mbOK], 0);
            Ke := 1;
            exit;
          end;
        end;

        Assignfile(FFile, 'OUT.TXT');
      //{$I-}
        Reset(FFile);
//{$I+}
        Readln(FFile, Ke_);
        CloseFile(FFile);
        if FileExistsUTF8('OUT.TXT') { *Converted from FileExists* } then
          DeleteFileUTF8('OUT.TXT'); { *Converted from DeleteFile* }
        Ke := Ke_;
      end;
      //=====================================================================================

      if Ke < 0.2 then
        Ke := 1;
      for II := 1 to 10 do
      begin
        res[9, II] := res[7, II] * Ke;
        res[10, II] := res[8, II] * Ke;
        if res[1, II] > 1 then
        begin
          if Ke > 1 then
          begin
            Series2.AddXY(res[1, II], res[9, II] * 10, '', clTeeColor);   // Сопротивление
            Series4.AddXY(res[1, II], res[10, II], '', clTeeColor);
            // Мощность
          end;
          Series1.AddXY(res[1, II], res[7, II] * 10, '', clTeeColor);
          // Сопротивление
          Series3.AddXY(res[1, II], res[8, II], '', clTeeColor);
          // Мощность
        end;
      end;
    end;
    // Записываем результаты расчета в Resistp.dat для 10 скоростей
    Assignfile(FFile, 'RESISTp.dat');
         //{$I-}
    Rewrite(FFile);
//{$I+}
    Writeln(FFile, '#     Nser      Np       Wt        t       Eta_R     Dp');
    Write(FFile, '       54 ');
    Write(FFile, Np: 10: 0);
    Write(FFile, w: 10: 4);
    Write(FFile, t0: 10: 4);
    Write(FFile, nr: 10: 4);
    Writeln(FFile, Dp: 10: 4);
    Writeln(FFile, '#      Vs       Rt        Rte       Pe       Pee');
    for ii := 2 to 10 do
    begin
      Write(FFile, res[1, II]: 10: 2);
      Write(FFile, res[7, II]: 10: 2);
      Write(FFile, res[9, II]: 10: 2);
      Write(FFile, res[8, II]: 10: 2);
      Writeln(FFile, res[10, II]: 10: 2);
    end;
    CloseFile(FFile);
    // Записываем результаты расчета в Resist.dat для 5 скоростей
    Assignfile(FFile, 'RESIST.dat');
          //{$I-}
    Rewrite(FFile);
//{$I+}
    for I := 1 to 5 do
    begin
      if i = 1 then
        II := 2;
      if i = 2 then
        II := 4;
      if i = 3 then
        II := 6;
      if i = 4 then
        II := 9;
      if i = 5 then
        II := 10;
      Write(FFile, res[1, II]: 10: 2);
      Write(FFile, res[7, II]: 10: 2);
      Write(FFile, res[9, II]: 10: 2);
      Write(FFile, res[8, II]: 10: 2);
      Writeln(FFile, res[10, II]: 10: 2);
    end;
    CloseFile(FFile);

    // Определяем масштабный коэффициент для вывода
    flag := 0.001;
    if res[9, 10] > 5 then
      flag := 1.;
    // Масштабируем R и P
    for II := 1 to 10 do
      for I := 1 to 10 do
        if i > 4 then
          res[i, ii] := res[i, ii] / flag;

    // Основной расчет по Fung-Leibman
    ResultsMemo.Lines.Add('');
    ResultsMemo.Lines.Add('');
    ResultsMemo.Lines.Add(Space(10) + rs_Final_calculations_of_resistance_and_power_by_method_Fung_Leibman_1995 {UserString[1249]});
    ResultsMemo.Lines.Add(Space(10) + rs_t________t________t________t________t________t________t________t________t________t {UserString[1202]});
    ResultsMemo.Lines.Add(Space(10) + rs_l___Vs___l___Vms__l___Fr___l___R_f__l___R_r__l___R_T__l___Pe___l__R_T_e_l__Pe_e__l {UserString[1203]});
    Chart.LeftAxis.Title.Caption :=
      rs_Resistance {UserString[272]} + '*10, ' + rs_kN {UserString[324]} + ';   ' + rs_Power {UserString[300]} + ', ' + rs_kW {UserString[325]};
    //       Chart.LeftAxis.Title.Caption:=rs_Resistance {UserString[272]}+'*10, '+rs_N {UserString[330]}+';   '+rs_Power {UserString[300]}+', '+rs_W {UserString[331]};
    if flag = 1 then
      ResultsMemo.Lines.Add(Space(10) + rs_l___kn___l___m_s__l________l___kN___l___kN___l___kN___l___kW___l___kN___l___kW___l {UserString[1205]})
    else
      ResultsMemo.Lines.Add(Space(10) + rs_l___kn___l___m_s__l________l____N___l____N___l____N___l___W____l____N___l___W____l {UserString[1204]});

(*         if flag=1 then  begin  ResultsMemo.Lines.Add(Space(10)+rs_l___kn___l___m_s__l________l___kN___l___kN___l___kN___l___kW___l___kN___l___kW___l {UserString[1205]});
                   Chart.LeftAxis.Title.Caption:=rs_Resistance {UserString[272]}+'*10, '+rs_kN {UserString[324]}+';   '+rs_Power {UserString[300]}+', '+rs_kW {UserString[325]}
                   end
                   else  begin ResultsMemo.Lines.Add(Space(10)+rs_l___kn___l___m_s__l________l____N___l____N___l____N___l___W____l____N___l___W____l {UserString[1204]});
                   Chart.LeftAxis.Title.Caption:=rs_Resistance {UserString[272]}+'*10, '+rs_N {UserString[330]}+';   '+rs_Power {UserString[300]}+', '+rs_W {UserString[331]}
         end;
*)
    ResultsMemo.Lines.Add(Space(10) + rs_t________t________t________t________t________t________t________t________t________t {UserString[1202]});
    Index := 0;
    Setlength(CpOpt, Trunc((EndSpeed - StartSpeed) / StepSpeed) + 10);
    Setlength(LCBOpt, Trunc((EndSpeed - StartSpeed) / StepSpeed) + 10);

    for II := 1 to 10 do
    begin
      for I := 1 to 10 do
      begin
        if IsNaN(res[i, II]) then begin ss[i]:='   NaN  | '; continue; end;

        if res[i, II] >= 10000 then
          ss[i] := FloatToStrF(res[i, II], ffFixed, 6, 0) + '  | ';
        if (res[i, II] >= 1000) and (res[i, II] < 10000) then
          ss[i] := FloatToStrF(res[i, II], ffFixed, 6, 0) + '.  | ';
        if (res[i, II] >= 100) and (res[i, II] < 1000) then
          ss[i] := FloatToStrF(res[i, II], ffFixed, 6, 1) + '  | ';
        if (res[i, II] >= 10) and (res[i, II] < 100) then
          ss[i] := FloatToStrF(res[i, II], ffFixed, 6, 2) + '  | ';
        if res[i, II] < 10 then
          ss[i] := FloatToStrF(res[i, II], ffFixed, 6, 3) + '  | ';
      end;
      if ii > 1 then
        ResultsMemo.Lines.Add(Space(10) + '| ' + ss[1] + ss[2] + ss[3] + ss[5] + ss[6] +
          ss[7] + ss[8] + ss[9] + ss[10]);
    end;
    ResultsMemo.Lines.Add(Space(10) + rs_t________t________t________t________t________t________t________t________t________t {UserString[1202]});
    ResultsMemo.Lines.Add('');

    Tb := res[7, 9] * ke / (1 - t0) * flag;
    Vms := res[2, 9];

    if Np > 0 then
    begin
      P_D := 0.9;
      Z := 4;
      K := 0.1;
      Ta := Tc - Draft / 2.;
      Dp_ := Dp;
      Tb := Tb / Np;
      if Tb = 0 then
        Tb := 0.01;
      if (Dp = 0) and (Np = 1) then
        Dp_ := 0.7 * Ta;
      if (Dp = 0) and (Np = 2) then
        Dp_ := 0.65 * Ta;
      if Np = 2 then
        K := 0.2;
      Kdt := Vms * (1 - w) * Dp_ * sqrt(Density / Tb);
      if Kdt >= 2 then
        Z := 3;
      x := Kdt;
      Ae_Ao := K + (1.3 + 0.3 * Z) * Tb * 1000. /
        (Dp_ * Dp_ * (99047. + Density * 1000. * 9.81 * (Dp_ / 2. + Ta * 0.2)));
      Ae0 := Ae_Ao;
      if Ae0 > 1.1 then
        ResultsMemo.Lines.Add(Space(14) + 'Ae/Ao   =  ' + FloatToStrF(Ae0, ffFixed, 6, 4));
      if (Ae0 > 1.1) and (Np <= 2) then
      begin
        ResultsMemo.Lines.Add(Space(10) + rs_ATTENTION____Increase_number_of_propellers_or_decrease_estimate_speed_ {UserString[971]});
        ResultsMemo.Visible := True;
        exit;
      end;
      if Z = 2 then
        P_D := -0.0030823 * x * x * x + 0.0142337 * x * x + 0.1859161 * x + 0.4598835;
      // Для 2 лопастей  Teta=0.30
      if Z = 3 then
      begin
        if Ae0 <= 0.35 then
        begin
          P_D := 0.0081576 * x * x * x - 0.0083761 * x * x + 0.3275126 * x + 0.3668222;
          // Для 3 лопастей Teta=0.35
          Ae_Ao := 0.35;
        end;
        if (Ae0 <= 0.5) and (Ae0 > 0.35) then
        begin
          P_D := -0.0240385 * x * x * x + 0.13499 * x * x + 0.1599401 * x + 0.498986;
          // Для 3 лопастей Teta=0.50
          Ae_Ao := 0.5;
        end;
        if (Ae0 <= 0.65) and (Ae0 > 0.5) then
        begin
          P_D := 0.0236147 * x * x * x * x * x - 0.2234597 * x * x * x * x + 0.8044646 * x *
            x * x - 1.3956004 * x * x + 1.3848023 * x + 0.3433638; // Для 3 лопастей Teta=0.65
          Ae_Ao := 0.65;
        end;
      end;
      if Z = 4 then
      begin
        if Ae0 <= 0.4 then
        begin
          //                ResultsMemo.Lines.Add(Space(14)+'Ae/Ao   =  '+FloatToStrF(Ae0,ffFixed,6,3));
          P_D := 0.039967 * x * x * x - 0.2388861 * x * x + 0.7556191 * x + 0.2328998;
          // Для 4 лопастей Teta=0.40
          Ae_Ao := 0.4;
          //                ResultsMemo.Lines.Add(Space(14)+'P/D 0.4 =  '+FloatToStrF(P_D,ffFixed,6,3));
        end;
        if (Ae0 <= 0.55) and (Ae0 > 0.4) then
        begin
          //               ResultsMemo.Lines.Add(Space(14)+'Ae/Ao   =  '+FloatToStrF(Ae0,ffFixed,6,3));
          P_D := 0.0353248 * x * x * x - 0.2223617 * x * x + 0.6754278 * x + 0.4570168;
          // Для 4 лопастей Teta=0.55
          Ae_Ao := 0.55;
          //               ResultsMemo.Lines.Add(Space(14)+'P/D 0.55=  '+FloatToStrF(P_D,ffFixed,6,3));
        end;
        if (Ae0 <= 0.7) and (Ae0 > 0.55) then
        begin
          //              ResultsMemo.Lines.Add(Space(14)+'Ae/Ao   =  '+FloatToStrF(Ae0,ffFixed,6,3));
          P_D := 0.0074646 * x * x * x * x * x - 0.0783429 * x * x * x * x + 0.2946838 *
            x * x * x - 0.5441205 * x * x + 0.8133517 * x + 0.3989677;     // Для 4 лопастей Teta=0.70
          Ae_Ao := 0.7;
          //              ResultsMemo.Lines.Add(Space(14)+'P/D 0.7 =  '+FloatToStrF(P_D,ffFixed,6,3));
        end;
      end;
      //      ResultsMemo.Lines.Add(Space(14)+'Ta        =  '+FloatToStrF(Ta,ffFixed,6,3)+' '+LengthStr(Units));
      if flag = 1 then
        ResultsMemo.Lines.Add(Space(14) + 'Tb        =  ' + FloatToStrF(Tb, ffFixed, 6, 3) +
          ' ' + rs_kN {UserString[324]})
      else
        ResultsMemo.Lines.Add(Space(14) + 'Tb        =  ' + FloatToStrF(
          Tb / flag, ffFixed, 6, 0) + ' ' + rs_N {UserString[330]});
      ResultsMemo.Lines.Add(Space(14) + 'Kdt       =  ' + FloatToStrF(Kdt, ffFixed, 6, 3));
      ResultsMemo.Lines.Add(Space(14) + 'Dp        =  ' + FloatToStrF(
        Dp_, ffFixed, 6, 3) + ' ' + LengthStr(Units));
      ResultsMemo.Lines.Add(Space(14) + 'Z         =  ' + FloatToStrF(Z, ffFixed, 6, 0));
      ResultsMemo.Lines.Add(Space(14) + 'Ae/Ao     =  ' + FloatToStrF(
        Ae0, ffFixed, 6, 3) + ' ' + rs___calculated {UserString[972]});
      P_D0 := P_D;
      Ae0 := Ae_Ao;
      ResultsMemo.Lines.Add(Space(14) + 'Ae/Ao     =  ' + FloatToStrF(
        Ae0, ffFixed, 6, 3) + ' ' + rs___for_selection_of_propeller_diagram {UserString[973]});
      ResultsMemo.Lines.Add(Space(14) + 'P/Dp      =  ' + FloatToStrF(
        P_D0, ffFixed, 6, 3) + ' ' + rs___by_curve_of_propeller_optimal_frequency {UserString[974]});
    end;
    if Np = 1 then
      nr := 0.9922 - 0.05908 * Ae0 + 0.07424 * (Cp - 0.0225 * lcb)
    else
      nr := 0.9737 + 0.111 * (Cp - 0.0225 * lcb) - 0.06325 * P_D0;
    ResultsMemo.Lines.Add(Space(14) + 'Ke        =  ' + FloatToStrF(Ke, ffFixed, 6, 3));
    ResultsMemo.Lines.Add(Space(14) + 'Wt        =  ' + FloatToStrF(w, ffFixed, 6, 4));
    ResultsMemo.Lines.Add(Space(14) + 't         =  ' + FloatToStrF(t0, ffFixed, 6, 4));
    ResultsMemo.Lines.Add(Space(14) + 'EtaR      =  ' + FloatToStrF(nr, ffFixed, 6, 4));
    ResultsMemo.Lines.Add(Space(14) + 'EtaH      =  ' + FloatToStrF(
      (1 - t0) / (1 - w), ffFixed, 6, 4));
    ResultsMemo.Lines.Add(Space(14) + 'EtaH*EtaR =  ' + FloatToStrF(
      (1 - t0) / (1 - w) * nr, ffFixed, 6, 4));
    ResultsMemo.Lines.Add('');
    ResultsMemo.Lines.Add(Space(14) + rs_NOTE__Coefficients_Wt__t_and_EtaR_were_calculated_by_formulaes_of_method_Holtrop_ {UserString[1026]} + FloatToStrF(
      Nver, ffFixed, 6, 0));
    ResultsMemo.Lines.Add('');
    ResultsMemo.Lines.Add('');
    ResultsMemo.Lines.Add(Space(10) + 'Copyright (c) 2010, Timoshenko V.F.');

    ResultsMemo.Visible := True;
    PrintButton.Enabled := True;
  end;
end;{TFreeResistance_FungLeib.Calculate}


function TFreeResistance_FungLeib.FGetStartSpeed: single;
begin
  Result := StartSpeedbox.Value;
end;{TFreeResistance_FungLeib.FGetStartSpeed}

procedure TFreeResistance_FungLeib.FSetStartSpeed(val: single);
begin
  StartSpeedbox.Value := Val;
end;{TFreeResistance_FungLeib.FSetStartSpeed}

function TFreeResistance_FungLeib.FGetEndSpeed: single;
begin
  Result := EndSpeedbox.Value;
end;{TFreeResistance_FungLeib.FGetEndSpeed}

procedure TFreeResistance_FungLeib.FSetEndSpeed(val: single);
begin
  EndSpeedbox.Value := val;
end;{TFreeResistance_FungLeib.FSetEndSpeed}

function TFreeResistance_FungLeib.FGetDensity: single;
begin
  Result := Densitybox.Value;
end;{TFreeResistance_FungLeib.FGetDensity}

procedure TFreeResistance_FungLeib.FSetDensity(val: single);
begin
  Densitybox.Value := val;

end;{TFreeResistance_FungLeib.FSetDensity}

function TFreeResistance_FungLeib.FGetDisplacement: single;
begin
  Result := Displacementbox.Value;
end;{TFreeResistance_FungLeib.FGetDisplacement}

procedure TFreeResistance_FungLeib.FSetDisplacement(val: single);
begin
  Displacementbox.Value := val;
end;{TFreeResistance_FungLeib.FSetDisplacement}

function TFreeResistance_FungLeib.FGetDraft: single;
begin
  Result := Draftbox.Value;
end;{TFreeResistance_FungLeib.FGetDraft}

procedure TFreeResistance_FungLeib.FSetDraft(val: single);
begin
  Draftbox.Value := val;
end;{TFreeResistance_FungLeib.FSetDraft}

function TFreeResistance_FungLeib.FGetDraftTotal: single;
begin
  Result := DraftTotalbox.Value;
end;{TFreeResistance_FungLeib.FGetDraftTotal}

procedure TFreeResistance_FungLeib.FSetDraftTotal(val: single);
begin
  DraftTotalbox.Value := val;
end;{TFreeResistance_FungLeib.FSetDraftTotal}

function TFreeResistance_FungLeib.FGetLwl: single;
begin
  Result := Lwlbox.Value;
end;{TFreeResistance_FungLeib.FGetLwl}

procedure TFreeResistance_FungLeib.FSetLwl(val: single);
begin
  Lwlbox.Value := val;
end;{TFreeResistance_FungLeib.FSetLwl}

function TFreeResistance_FungLeib.FGetLCB: single;
begin
  Result := LCBbox.Value;
end;{TFreeResistance_FungLeib.FGetLCB}

procedure TFreeResistance_FungLeib.FSetLCB(val: single);
begin
  LCBbox.Value := val;
end;{TFreeResistance_FungLeib.FSetLCB}

procedure TFreeResistance_FungLeib.FSetKe(val: single);
begin
  Kebox.Value := val;
  if Kebox.Value = 0 then
    Kebox.Value := 1;
end;{TFreeResistance_FungLeib.FSetKe}

function TFreeResistance_FungLeib.FGetKe: single;
begin
  Result := Kebox.Value;
end;{TFreeResistance_FungLeib.FGetKe}

procedure TFreeResistance_FungLeib.FSetK1(val: single);
begin
  K1box.Value := val;
end;{TFreeResistance_FungLeib.FSetK1}

function TFreeResistance_FungLeib.FGetK1: single;
begin
  Result := K1box.Value;
end;{TFreeResistance_FungLeib.FGetK1}

procedure TFreeResistance_FungLeib.FSetK2(val: single);
begin
  K2box.Value := val;
end;{TFreeResistance_FungLeib.FSetK2}

function TFreeResistance_FungLeib.FGetK2: single;
begin
  Result := K2box.Value;
end;{TFreeResistance_FungLeib.FGetK2}

procedure TFreeResistance_FungLeib.FSetK3(val: single);
begin
  K3box.Value := val;
end;{TFreeResistance_FungLeib.FSetK3}

function TFreeResistance_FungLeib.FGetK3: single;
begin
  Result := K3box.Value;
end;{TFreeResistance_FungLeib.FGetK3}

procedure TFreeResistance_FungLeib.FSetK4(val: single);
begin
  K4box.Value := val;
end;{TFreeResistance_FungLeib.FSetK4}

function TFreeResistance_FungLeib.FGetK4: single;
begin
  Result := K4box.Value;
end;{TFreeResistance_FungLeib.FGetK4}

procedure TFreeResistance_FungLeib.FSetK5(val: single);
begin
  K5box.Value := val;
end;{TFreeResistance_FungLeib.FSetK5}

function TFreeResistance_FungLeib.FGetK5: single;
begin
  Result := K5box.Value;
end;{TFreeResistance_FungLeib.FGetK5}

procedure TFreeResistance_FungLeib.FSetK6(val: single);
begin
  K6box.Value := val;
end;{TFreeResistance_FungLeib.FSetK6}

function TFreeResistance_FungLeib.FGetK6: single;
begin
  Result := K6box.Value;
end;{TFreeResistance_FungLeib.FGetK6}

procedure TFreeResistance_FungLeib.FSetK7(val: single);
begin
  K7box.Value := val;
end;{TFreeResistance_FungLeib.FSetK7}

function TFreeResistance_FungLeib.FGetK7: single;
begin
  Result := K7box.Value;
end;{TFreeResistance_FungLeib.FGetK7}

procedure TFreeResistance_FungLeib.FSetA1(val: single);
begin
  A1box.Value := val;
end;{TFreeResistance_FungLeib.FSetA1}

function TFreeResistance_FungLeib.FGetA1: single;
begin
  Result := A1box.Value;
end;{TFreeResistance_FungLeib.FGetA1}

procedure TFreeResistance_FungLeib.FSetA2(val: single);
begin
  A2box.Value := val;
end;{TFreeResistance_FungLeib.FSetA2}

function TFreeResistance_FungLeib.FGetA2: single;
begin
  Result := A2box.Value;
end;{TFreeResistance_FungLeib.FGetA2}

procedure TFreeResistance_FungLeib.FSetA3(val: single);
begin
  A3box.Value := val;
end;{TFreeResistance_FungLeib.FSetA3}

function TFreeResistance_FungLeib.FGetA3: single;
begin
  Result := A3box.Value;
end;{TFreeResistance_FungLeib.FGetA3}

procedure TFreeResistance_FungLeib.FSetA4(val: single);
begin
  A4box.Value := val;
end;{TFreeResistance_FungLeib.FSetA4}

function TFreeResistance_FungLeib.FGetA4: single;
begin
  Result := A4box.Value;
end;{TFreeResistance_FungLeib.FGetA4}

procedure TFreeResistance_FungLeib.FSetA5(val: single);
begin
  A5box.Value := val;
end;{TFreeResistance_FungLeib.FSetA5}

function TFreeResistance_FungLeib.FGetA5: single;
begin
  Result := A5box.Value;
end;{TFreeResistance_FungLeib.FGetA5}

procedure TFreeResistance_FungLeib.FSetA6(val: single);
begin
  A6box.Value := val;
end;{TFreeResistance_FungLeib.FSetA6}

function TFreeResistance_FungLeib.FGetA6: single;
begin
  Result := A6box.Value;
end;{TFreeResistance_FungLeib.FGetA6}

procedure TFreeResistance_FungLeib.FSetA7(val: single);
begin
  A7box.Value := 1.226; //val;
end;{TFreeResistance_FungLeib.FSetA7}

function TFreeResistance_FungLeib.FGetA7: single;
begin
  Result := A7box.Value;
end;{TFreeResistance_FungLeib.FGetA7}

procedure TFreeResistance_FungLeib.FSetA8(val: single);
begin
  A8box.Value := val;
end;{TFreeResistance_FungLeib.FSetA8}

function TFreeResistance_FungLeib.FGetA8: single;
begin
  Result := A8box.Value;
end;{TFreeResistance_FungLeib.FGetA8}

procedure TFreeResistance_FungLeib.FSetA9(val: single);
begin
  A9box.Value := val;
end;{TFreeResistance_FungLeib.FSetA9}

function TFreeResistance_FungLeib.FGetA9: single;
begin
  Result := A9box.Value;
end;{TFreeResistance_FungLeib.FGetA9}

procedure TFreeResistance_FungLeib.FSetA10(val: single);
begin
  A10box.Value := val;
end;{TFreeResistance_FungLeib.FSetA10}

function TFreeResistance_FungLeib.FGetA10: single;
begin
  Result := A10box.Value;
end;{TFreeResistance_FungLeib.FGetA10}

procedure TFreeResistance_FungLeib.FSetA11(val: integer);
begin
  A11box.ItemIndex := val - 1;
end;{TFreeResistance_FungLeib.FSetA11}

function TFreeResistance_FungLeib.FGetA11: integer;
begin
  Result := A11box.ItemIndex + 1;
end;{TFreeResistance_FungLeib.FGetA11}

procedure TFreeResistance_FungLeib.FSetBA(val: single);
begin
  BAbox.Value := val; // Площадь транца
end;{TFreeResistance_FungLeib.FSetBA}

function TFreeResistance_FungLeib.FGetBA: single;
begin
  Result := BAbox.Value;
end;{TFreeResistance_FungLeib.FGetBA}

procedure TFreeResistance_FungLeib.FSetKBulb(val: single);
begin
  KBulbbox.Value := val;
end;{TFreeResistance_FungLeib.FSetKBulb}

function TFreeResistance_FungLeib.FGetKBulb: single;
begin
  Result := KBulbbox.Value;
end;{TFreeResistance_FungLeib.FGetKBulb}

procedure TFreeResistance_FungLeib.FSetZBulb(val: single);
begin
  ZBulbbox.Value := val;   // Ширина транца
end;{TFreeResistance_FungLeib.FSetZBulb}

function TFreeResistance_FungLeib.FGetZBulb: single;
begin
  Result := ZBulbbox.Value;
end;{TFreeResistance_FungLeib.FGetZBulb}


procedure TFreeResistance_FungLeib.FSetCstrn(val: single);
begin
  Cstrnbox.Value := val;
  // Тип движителя 2- ВФШ (FPP) 1- ВРШ(CPP) 0- не учитывается
end;{TFreeResistance_FungLeib.FSetCstrn}

function TFreeResistance_FungLeib.FGetCstrn: single;
begin
  Result := Cstrnbox.Value;
  if (Cstrnbox.Value = 0) and (Np <> 0) then
    Cstrnbox.Value := 2;
end;{TFreeResistance_FungLeib.FGetCstrn}

procedure TFreeResistance_FungLeib.FSetNp(val: single);
begin
  Npbox.Value := val;
end;{TFreeResistance_FungLeib.FSetNp}

function TFreeResistance_FungLeib.FGetNp: single;
begin
  Result := Npbox.Value;
  if Npbox.Value > 2 then
    Npbox.Value := 2;
end;{TFreeResistance_FungLeib.FGetNp}

procedure TFreeResistance_FungLeib.FSetDp(val: single);
begin
  Dpbox.Value := Val;
end;{TFreeResistance_FungLeib.FSetDp}

function TFreeResistance_FungLeib.FGetDp: single;
begin
  Result := Dpbox.Value;
  if (Dpbox.Value = 0) and (Np = 1) then
    Dpbox.Value := DraftTotal * 0.75;
  if (Dpbox.Value = 0) and (Np = 2) then
    Dpbox.Value := DraftTotal * 0.7;
end;{TFreeResistance_FungLeib.FGetDp}

procedure TFreeResistance_FungLeib.FSetKs(val: single);
begin
  Ksbox.Value := val;
end;{TFreeResistance_FungLeib.FSetKs}

function TFreeResistance_FungLeib.FGetKs: single;
begin
  Result := Ksbox.Value;
end;{TFreeResistance_FungLeib.FGetKs}


function TFreeResistance_FungLeib.FGetKeelChordLength: single;
begin
  Result := KeelChordLengthbox.Value;
end;{TFreeResistance_FungLeib.FGetKeelChordLength}

procedure TFreeResistance_FungLeib.FSetKeelChordLength(val: single);
begin
  KeelChordLengthbox.Value := val;
end;{TFreeResistance_FungLeib.FSetKeelChordLength}

function TFreeResistance_FungLeib.FGetKeelArea: single;
begin
  Result := KeelAreabox.Value;
end;{TFreeResistance_FungLeib.FGetKeelArea}

procedure TFreeResistance_FungLeib.FSetKeelArea(val: single);
begin
  KeelAreabox.Value := val;
end;{TFreeResistance_FungLeib.FSetKeelArea}

function TFreeResistance_FungLeib.FGetRudderChordLength: single;
begin
  Result := RudderChordLengthbox.Value;
end;{TFreeResistance_FungLeib.FGetRudderChordLength}

procedure TFreeResistance_FungLeib.FSetRudderChordLength(val: single);
begin
  RudderChordLengthbox.Value := val;
end;{TFreeResistance_FungLeib.FSetRudderChordLength}

function TFreeResistance_FungLeib.FGetRudderArea: single;
begin
  Result := RudderAreabox.Value;
end;{TFreeResistance_FungLeib.FGetRudderArea}

procedure TFreeResistance_FungLeib.FSetRudderArea(val: single);
begin
  RudderAreabox.Value := val;
end;{TFreeResistance_FungLeib.FSetRudderArea}

function TFreeResistance_FungLeib.FGetBwl: single;
begin
  Result := Bwlbox.Value;
end;{TFreeResistance_FungLeib.FGetBwl}

procedure TFreeResistance_FungLeib.FSetBwl(val: single);
begin
  Bwlbox.Value := val;
end;{TFreeResistance_FungLeib.FSetBwl}

function TFreeResistance_FungLeib.FGetCp: single;
begin
  Result := Cpbox.Value;
end;{TFreeResistance_FungLeib.FGetCp}

procedure TFreeResistance_FungLeib.FSetCp(val: single);
begin
  Cpbox.Value := val;
end;{TFreeResistance_FungLeib.FSetCp}

function TFreeResistance_FungLeib.FGetViscosity: single;
begin
  Result := Viscositybox.Value;
end;{TFreeResistance_FungLeib.FGetViscosity}

procedure TFreeResistance_FungLeib.FSetViscosity(val: single);
begin
  Viscositybox.Value := val;
end;{TFreeResistance_FungLeib.FSetViscosity}

function TFreeResistance_FungLeib.FGetWettedSurface: single;
var
  C23, ScbFact, Am, Cm, Cwp: single;
begin
  if EstimateBox.Checked then
  begin
    if (Lwl > 0) and (Bwl > 0) and (Cp > 0) and (Displacement > 0) then
    begin
      Am := Displacement / (Lwl * Cp);
      Cm := Am / (Bwl * Tc);
      Cwp := WlArea / (Lwl * Bwl);
      C23 := 0.453 + 0.443 * (Cp * Cm) - 0.286 * Cm - 0.00347 * (Bwl / Tc) + 0.37 * Cwp;
      ScbFact := 0.616 * C23 + 0.111 * Cm * Cm * Cm + 0.245 * (C23 / Cm) - 0.0228;
      Result := ScbFact * Lwl * (2 * Tc + Bwl) * sqrt(Cm);
    end
    else
      Result := 0;
  end
  else
    Result := WettedSurfacebox.Value;
end;{TFreeResistance_FungLeib.FGetWettedSurface}

procedure TFreeResistance_FungLeib.FSetWettedSurface(val: single);
begin
  WettedSurfacebox.Value := val;
end;{TFreeResistance_FungLeib.FSetWettedSurface}

function TFreeResistance_FungLeib.FGetLos: single;
begin
  Result := Losbox.Value;
end;{TFreeResistance_FungLeib.FGetLos}

procedure TFreeResistance_FungLeib.FSetLos(val: single);
begin
  Losbox.Value := val;
end;{TFreeResistance_FungLeib.FSetLos}

function TFreeResistance_FungLeib.FGetStepSpeed: single;
begin
  Result := StepSpeedbox.Value;
end;{TFreeResistance_FungLeib.FGetStepSpeed}

procedure TFreeResistance_FungLeib.FSetStepSpeed(val: single);
begin
  StepSpeedbox.Value := val;
end;{TFreeResistance_FungLeib.FSetStepSpeed}

function TFreeResistance_FungLeib.FGetExtractFromHull: boolean;
begin
  Result := CheckBox2.Checked;
end;{TFreeResistance_FungLeib.FGetExtractFromHull}

procedure TFreeResistance_FungLeib.FSetExtractFromHull(Val: boolean);
begin
  if Checkbox2.Checked <> val then
    Checkbox2.Checked := val;
end;{TFreeResistance_FungLeib.FSetExtractFromHullVal}

function TFreeResistance_FungLeib.Execute(Freeship: TFreeship;
  AutoExtract: boolean): boolean;
var
  Units: TFreeUnitType;
  Temper: single;
  RightAxis: TChartAxis;
begin
  FFreeship := Freeship;

  ToolBar1.ButtonWidth := Freeship.Preferences.ToolIconSize;
  ToolBar1.ButtonHeight := Freeship.Preferences.ToolIconSize;

  Freeship.Preferences.LoadImageIntoList(MenuImages, 0, 'Cancel');
  Freeship.Preferences.LoadImageIntoList(MenuImages, 1, 'Ok');
  Freeship.Preferences.LoadImageIntoList(MenuImages, 2, 'Print');
  Freeship.Preferences.LoadImageIntoList(MenuImages, 3, 'Calculate');

  Units := FFreeship.ProjectSettings.ProjectUnits;
  if Units = fuImperial then
  begin
    MessageDlg(rs_This_is_version_of_program_work_with_metric_system_units_only {UserString[754]}, mtInformation, [mbOK], 0);
    exit;
  end;
  //   Chart.Title.Text.Text:=rs_Resistance_and_power_are_calculated {UserString[265]};
  Chart.Title.Text.Text := ' ';
  Chart.BottomAxis.Title.Caption := rs_Speed {UserString[273]} + ', ' + rs_kn {UserString[326]};
   {$ifNdef FPC}
  RightAxis := Chart.RightAxis;
   {$else}
  RightAxis := Chart.AxisList.GetAxisByAlign(calRight);
   {$endif}
  RightAxis.Title.Caption := rs_Power {UserString[300]} + ', ' + rs_kW {UserString[325]};
  Units := FFreeship.ProjectSettings.ProjectUnits;
  Temper := FFreeship.ProjectSettings.ProjectWaterTemper;
  DensityBox.Enabled := False;
  //   ViscosityBox.Enabled:=False;
  K1Box.Enabled := False;
  K2Box.Enabled := False;
  K3Box.Enabled := False;
  K4Box.Enabled := False;
  K5Box.Enabled := False;
  K6Box.Enabled := False;
  K7Box.Enabled := False;
  KeelChordLengthbox.Enabled := False;
  KeelAreaBox.Enabled := False;
  ZBulbBox.Enabled := True;
  BABox.Enabled := True;
  DraftBox.Enabled := False;
  CstrnBox.Enabled := True;
  RudderChordLengthbox.Enabled := False;
  RudderAreaBox.Enabled := False;
  Checkbox2.Enabled := FFreeship.Surface.NumberOfControlFaces > 1;
  Checkbox2.Checked := AutoExtract;
  if Checkbox2.Checked then
    CheckBox2Click(self);
  _Label31.Caption := rs_knots {UserString[457]};
  _Label32.Caption := rs_knots {UserString[457]};
  _Label33.Caption := rs_knots {UserString[457]};
  Label34.Caption := DensityStr(Units);
  _Label8.Caption := LengthStr(Units);
  _Label9.Caption := LengthStr(Units);
  _Label10.Caption := LengthStr(Units);
  _Label11.Caption := LengthStr(Units);
  _Label12.Caption := AreaStr(Units);
  _Label14.Caption := AreaStr(Units);
  _Label15.Caption := VolStr(Units);
  _Label16.Caption := '%';
{   _Label17.Caption:=AreaStr(Units);
   _Label18.Caption:=AreaStr(Units);
   _Label19.Caption:=AreaStr(Units);
   _Label30.Caption:=AreaStr(Units); }

  // Skip translation
  if Units = fuMetric then
    _Label36.Caption := ' ' + rs_m_2_s {UserString[472]}
  else
    _Label36.Caption := ' ' + rs_ft_2_s {UserString[471]};
  // End Skip translation
  Viscosity := FindWaterViscosity(Temper, Units);
  Calculate;
  ShowModal;
  Result := ModalResult = mrOk;
end;{TFreeResistance_FungLeib.Execute}

procedure TFreeResistance_FungLeib.CheckBox2Click(Sender: TObject);
begin
  LwlBox.Enabled := not Checkbox2.Checked;
  BwlBox.Enabled := not Checkbox2.Checked;
  WettedSurfaceBox.Enabled := not Checkbox2.Checked;
  LosBox.Enabled := not Checkbox2.Checked;
  DisplacementBox.Enabled := not Checkbox2.Checked;
  LCBBox.Enabled := not Checkbox2.Checked;
  CpBox.Enabled := not Checkbox2.Checked;
  KbulbBox.Enabled := not Checkbox2.Checked;
  //   CstrnBox.Enabled:= False;
  //   BABox.Enabled:= False;
  DraftBox.Enabled := False;
  EstimateBox.Enabled := not Checkbox2.Checked;
  if CheckBox2.Checked then
  begin
    EstimateBox.Checked := False;
    if DraftTotal = 0.0 then
      DraftTotal := FFreeship.ProjectSettings.ProjectDraft;
    if FFreeship <> nil then
      Density := FFreeship.ProjectSettings.ProjectWaterDensity;
    DraftTotalBoxAfterSetValue(self);
  end;
end;{TFreeResistance_FungLeib.CheckBox2Click}

procedure TFreeResistance_FungLeib.ToolButton25Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TFreeResistance_FungLeib.ToolButton25Click}

procedure TFreeResistance_FungLeib.ToolButton7Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TFreeResistance_FungLeib.ToolButton7Click}

procedure TFreeResistance_FungLeib.ToolButton20Click(Sender: TObject);
begin
  Calculate;
end;{TFreeResistance_FungLeib.ToolButton20Click}

procedure TFreeResistance_FungLeib.PrintButtonClick(Sender: TObject);
var
  Line: integer;
  PrintText: TextFile;
begin
  if PrintDialog.Execute then
  begin
    AssignPrn(PrintText);
    Rewrite(PrintText);
    Printer.Canvas.Font.Assign(ResultsMemo.Font);
    for Line := 0 to ResultsMemo.Lines.Count - 1 do
      Writeln(PrintText, ResultsMemo.Lines[Line]);
    CloseFile(PrintText);
    //Chart.PrintLandscape;
    Chart.PaintOnCanvas(Printer.Canvas, Rect(0, 0, Chart.Width, Chart.Height));
  end;
end;{TFreeResistance_FungLeib.ToolButton1Click}

procedure TFreeResistance_FungLeib.DraftTotalBoxAfterSetValue(Sender: TObject);
var
  HydObject: TFreeHydrostaticCalc;
begin
  if (CheckBox2.Checked) and (FFreeship <> nil) then
  begin
    HydObject := TFreeHydrostaticCalc.Create(FFreeship);
    HydObject.Draft := DraftTotal;
    HydObject.Calculate;

    Lwl := HydObject.Data.LengthWaterline;
    Los := HydObject.Data.SubMax.X - HydObject.Data.SubMin.X;
    Bwl := HydObject.Data.BeamWaterline;
    Z0 := HydObject.Data.ModelMin.Z;
    Tc := DraftTotal + Z0;
    WettedSurface := HydObject.Data.WettedSurface;
    WlArea := HydObject.Data.Waterplanearea;
    Ie_ := HydObject.Data.WaterplaneEntranceAngle;
    Kbulb := Ie_;
    Displacement := HydObject.Data.Volume;
    //      if Lwl>0 then LCB:=100*(HydObject.Data.CenterOfBuoyancy.X-FFreeship.ProjectSettings.ProjectLength*0.5)/Lwl
    if Lwl > 0 then
      LCB := 100 * (HydObject.Data.CenterOfBuoyancy.X - (HydObject.Data.WlMin.X + 0.5 * Lwl)) / Lwl
    else
      LCB := 0;
    Cp := HydObject.Data.PrismCoefficient;
    FreeAndNil(HydObject);
  end;
  Calculate;
end;{TFreeResistance_FungLeib.DraftTotalBoxAfterSetValue}

procedure TFreeResistance_FungLeib.StartSpeedBoxAfterSetValue(Sender: TObject);
begin
  Calculate;
end;{TFreeResistance_FungLeib.StartSpeedBoxAfterSetValue}

procedure TFreeResistance_FungLeib.LwlBoxAfterSetValue(Sender: TObject);
begin
  Calculate;
end;{TFreeResistance_FungLeib.LwlBoxAfterSetValue}

procedure TFreeResistance_FungLeib.KeelChordLengthboxAfterSetValue(Sender: TObject);
begin
  Calculate;
end;{TFreeResistance_FungLeib.KeelChordLengthboxAfterSetValue}

procedure TFreeResistance_FungLeib.EstimateBoxClick(Sender: TObject);
begin
  WettedSurfaceBox.Enabled := not EstimateBox.Checked;
  Calculate;
end;{TFreeResistance_FungLeib.EstimateBoxClick}

procedure TFreeResistance_FungLeib.Estimate2BoxClick(Sender: TObject);
begin
  if A1 = 0 then
    A1 := 6.;
  if A2 = 0 then
    A2 := 1.;
  if A4 = 0 then
    A4 := 1.;
  if A6 = 0 then
    A6 := 0.5;
  if A7 = 0 then
    A7 := 1.226;
  if A8 = 0 then
    A8 := 1.;
  if A9 = 0 then
    A9 := 100.;
  A10 := Cm * Bwl * Tc;
  if A11 = 0 then
    A11 := 1;
  Calculate;
end;{TFreeResistance_FungLeib.Estimate2BoxClick}


procedure TFreeResistance_FungLeib.CalculateResistanceFungLeib(
  ConvertedSpeed, LCB, Cp: single;
  var Rf, Rr, w, t0, nr: single);
var
  Rn_keel, Cf_keel, Rf_keel: single;
  Vs, Ke_, C_: single;
  Rn_rudder, Cf_Rudder, Rf_rudder: single;
  fraction, FroudeNumber, T: single;
  dat: array[1..30] of single;
  dan: array[1..15] of single;
  factors: array[0..9] of extended;
  m: array [1..4] of single;
  C: array [1..20] of single;
  a, Lower: integer;
  Nscrew: single;
  L, B, D, Ta, Tf, Cb, S, Cp1, Cv, nh, Cf, Re, Ca, k1_, Lr, V: single;
  L_B, B_T, B_L, L3v, B_Ta, Ta_D, Tf_L, T_L, T_B: single;
  Keel_chord, Keel_Area, K_1, K_2, K_3, K_4, K_5, K_6, K_7: TFloatType;
  A_1, A_2, A_3, A_4, A_5, A_6, A_7, A_8, A_9, A_10, A_11: TFloatType;
  Rudder_chord, Rudder_Area: TFloatType;
  LengthWaterline: TFloatType;
  WaterDensity: TFloatType;
  WaterViscosity: TFloatType;
  Displ: TFloatType;
  WetArea: TFloatType;
begin
  Rf := 0.0;
  Rr := 0.0;
  Nscrew := Np;
  if Np < 0 then
    Nscrew := 0;
  if Np > 2 then
    Nscrew := 2;

  if Convertedspeed <= 0 then
    exit;

  if FFreeship.ProjectSettings.ProjectUnits = fuImperial then
  begin
    LengthWaterline := Lwl * Foot;
    WaterDensity := Density / WeightConversionFactor;
    Keel_chord := KeelChordLength * Foot;
    Keel_Area := KeelArea * Foot * Foot;
    K_1 := K1 * Foot * Foot;
    K_2 := K2 * Foot * Foot;
    K_3 := K3 * Foot * Foot;
    K_4 := K4 * Foot * Foot;
    K_5 := K5 * Foot * Foot;
    K_6 := K6 * Foot * Foot;
    K_7 := K7 * Foot * Foot;
    Rudder_chord := RudderChordLength * Foot;
    Rudder_Area := RudderArea * Foot * Foot;
    WaterViscosity := Viscosity * 0.3048 * 0.3048 * 1e-6;
    Displ := Displacement * foot * foot * foot;
    WetArea := WettedSurface * Foot * Foot;
    A_1 := A1;
    A_2 := A2 * Foot;
    A_3 := A3;
    A_4 := A4 * Foot;
    A_5 := A5;
    A_6 := A6 * Foot;
    A_7 := A7 / (Foot * Foot * Foot);
    A_8 := A8 * Foot;
    A_9 := A9 * Foot;
    A_10 := A10 * Foot * Foot;
    A_11 := A11;
    S_ := S_ * Foot * Foot;
  end
  else
  begin
    LengthWaterline := Lwl;
    WaterDensity := Density;
    Keel_chord := KeelChordLength;
    Keel_Area := KeelArea;
    K_1 := K1;
    K_2 := K2;
    K_3 := K3;
    K_4 := K4;
    K_5 := K5;
    K_6 := K6;
    K_7 := K7;
    Rudder_chord := RudderChordLength;
    Rudder_Area := RudderArea;
    WaterViscosity := Viscosity * 1e-6;
    Displ := Displacement;
    WetArea := WettedSurface;
    A_1 := A1;
    A_2 := A2;
    A_3 := A3;
    A_4 := A4;
    A_5 := A5;
    A_6 := A6;
    A_7 := A7;
    A_8 := A8;
    A_9 := A9;
    A_10 := A10;
    A_11 := A11;
  end;

  //  Для расчета влияния эксплуатационных факторов

  if ke = 0 then
  begin
    if (A_9 <= 0) or (A_8 <= 0) or (A_6 <= 0) or (A_7 <= 0) or (A_11 <= 0) then
    begin
      MessageDlg('Data for Ke calcs need!!!', mtInformation, [mbOK], 0);
      exit;
    end;
    dat[1] := Lwl;        // Lpp
    dat[2] := Lwl;
    dat[3] := Bwl;
    dat[4] := Tc + Draft / 2.;      // Tf;
    dat[5] := Tc - Draft / 2.;      // Ta;
    dat[6] := Displacement; // V;
    dat[7] := WetArea;      // S;
    if dat[7] = 0 then
      dat[7] := S_;
    dat[8] := LCB;
    dat[9] := Cp * Cm;     // Cb;
    dat[10] := Cp;
    dat[11] := Cwp;
    dat[12] := Cm;
    dat[13] := BA;        // At;
    dat[14] := KBulb;     // Abt;
    dat[15] := ZBulb;     // Bt;
    dat[16] := Dp;         // At;
    dat[17] := WaterDensity;
    dat[18] := Viscosity; // Nu;
    dat[19] := Cstrn;     // TypeProp; не надо для расчета
    dat[20] := Np;        // Np;
    dat[21] := Ke;        // ke;
    dat[22] := Ks;        // ks;
    dat[23] := 0;
    dat[24] := 0;
    dat[25] := 0;
    dat[26] := 0;
    dat[27] := 0;
    dat[28] := 0;
    Vs := ConvertedSpeed;
    C_ := Rtotal / (0.5 * WaterDensity * sqr(Vs) * WetArea);
    dat[29] := C_;
    dat[30] := ConvertedSpeed; // Vs в м/с
    dan[1] := A_1;        // Time ship in water;
    dan[2] := A_2;        // Height 3%;
    dan[3] := A_3;        // angle wave;
    dan[4] := A_4;        // Speed wind;
    dan[5] := A_5;        // angle wind;
    dan[6] := A_6;        // height board;
    dan[7] := A_7;        // air density;
    dan[8] := A_8;        // Height COG superstructure;
    dan[9] := A_9;        // Depth water;
    dan[10] := A_10;      // w middle;
    dan[11] := A_11;      // Type ship;
    dan[12] := A_11;        // reserved
    dan[13] := A_11;        // reserved
    dan[14] := A_11;        // reserved
    dan[15] := A_11;        // reserved

    File_ExportDataKe(dat, dan);
  end;

  //  Структура исходных данных: L,Bwl,T,V,Cm,Ie,Sw,At,Tb,Rho,Nu,Vsn,Vsr,Vsk,Np,Ip
  dat[1] := Lwl;
  //  dat[1]:=FFreeship.ProjectSettings.ProjectLength;        // Lpp
  dat[2] := Bwl;
  dat[3] := Tc;
  dat[4] := Displacement;
  dat[5] := Cm;
  dat[6] := Kbulb;       // ie
  dat[7] := WetArea;     // S;
  if dat[7] <= 0 then
    dat[7] := S_;
  if BA < 0 then
    BA := -BA;
  dat[8] := BA / (Bwl * Tc * Cm); // At относительная площадь транца
  if dat[8] > 0.74 then
    dat[8] := 0.74;
  dat[9] := ZBulb / Bwl;              // Tb относительная ширина транца
  if dat[9] > 1 then
    dat[9] := 1;
  dat[10] := WaterDensity;
  dat[11] := Viscosity;  // Nu;
  dat[12] := Startspeed; // Vs, kn
  dat[13] := Endspeed;
  dat[14] := Stepspeed;
  dat[15] := Nscrew;
  if dat[15] > 2 then
    dat[15] := 2;
  if dat[15] < 1 then
    dat[15] := 0;
  Np := dat[15];
  dat[16] := Cstrn; // TypeProp;
  if dat[16] > 2 then
    dat[16] := 2;
  if dat[16] < 1 then
    dat[16] := 0;
  Cstrn := dat[16];
  dat[17] := 3;
  dat[18] := 4;
  dat[19] := 5;
  dat[20] := 6;
  dat[21] := 7;
  dat[22] := 8;
  dat[23] := 9;
  dat[24] := 0;
  dat[25] := 0;
  dat[26] := 0;
  dat[27] := 0;
  dat[28] := 0;
  dat[29] := 0;
  dat[30] := 0;


  File_ExportData(dat, dan);

  //Определение коэффициентов взаимодействия по методу Холтропа
  L := FFreeship.ProjectSettings.ProjectLength;
  B := Bwl;
  T := Tc;
  Tf := Tc + Draft / 2.;      // Tf;
  Ta := Tc - Draft / 2.;      // Ta;
  V := Displacement;
  D := Dp;
  if D = 0 then
    D := 0.7 * T;
  Cb := Cp * Cm;
  S := WetArea;
  Re := Convertedspeed * L / Viscosity * 1e6;
  Cf := 0.075 / sqr(log10(Re) - 2);
  L_B := L / B;
  B_L := B / L;
  B_Ta := B / Ta;
  Ta_D := Ta / D;
  Tf_L := Tf / L;
  T_L := T / L;
  T_B := T / B;
  Lr := L * (1 - Cp + 0.06 * Cp * lcb / (4 * Cp - 1));
  C[14] := 1. + 0.011 * Cstrn;
  k1_ := 0.93 + 0.487118 * C[14] * power(B_L, 1.06806) * power(T_L, 0.46106) * power(
    (L / Lr), 0.121563) * power(L * L * L / V, 0.36486) * power((1 - Cp), -0.604247);
  if B_Ta > 5 then
    C[8] := S * (7 * B_Ta - 25) / (L * D * (B_Ta - 3))
  else
    C[8] := B * S / (L * D * Ta);

  if C[8] > 28 then
    C[9] := 32 - 16 / (C[8] - 24)
  else
    C[9] := C[8];

  if Ta_D > 2 then
    C[11] := 0.0833333 * power(Ta_D, 3) + 1.33333
  else
    C[11] := Ta_D;

  if Cp > 0.7 then
    C[19] := 0.18567 / (1.3571 - Cm) - 0.71276 + 0.38648 * Cp
  else
    C[19] := 0.12997 / (0.95 - Cb) - 0.11056 / (0.95 - Cp);

  C[20] := 1 + 0.0015 * Cstrn;
  Ca := 0.00001;
  Ca := 0.006 * power((L + 100), (-0.16)) - 0.00205 + 0.003 * sqrt(L / 7.5) * sqr(sqr(Cb)) * C[2] *
    (0.04 - C[4]);
  if ks >= 150. then
    Ca := Ca + (0.105 * power(ks / 1000000., 0.333333) - 0.005579) / power(L, 0.33333);
  Cp1 := 1.45 * Cp - 0.315 - 0.0225 * lcb;
  Cv := k1_ * Cf + Ca;
  w := 0.1;
  Nver := 1988;
  ResultsMemo.Lines.Add(Space(14) + 'Cv            = ' + FloatToStrF(
    Cv, ffFixed, 6, 5));
  ResultsMemo.Lines.Add(Space(14) + 'Cp1           = ' + FloatToStrF(
    Cp1, ffFixed, 6, 5));
  ResultsMemo.Lines.Add(Space(14) + 'Cf            = ' + FloatToStrF(
    Cf, ffFixed, 6, 5));
  ResultsMemo.Lines.Add(Space(14) + 'Ca            = ' + FloatToStrF(
    Ca, ffFixed, 6, 5));
  ResultsMemo.Lines.Add(Space(14) + '(1+k)         = ' + FloatToStrF(
    k1_, ffFixed, 6, 5));

  if Np = 1 then
  begin
    // по методу Холтропа 1988
    w := C[20] * (C[9] * Cv * L / Ta * (0.050776 + 0.93405 * C[11] * Cv /
      (1 - Cp1)) + 0.27915 * sqrt(B / (L * (1 - Cp1))) + C[19]);
    t0 := 0.25014 * power(B_L, 0.28956) * power((sqrt(B * T) / D), 0.2624) / power(
      (1 - Cp + 0.0225 * lcb), 0.01762) + 0.0015 * Cstrn;
    nr := 0.9922 - 0.05908 * Ae0 + 0.07424 * (Cp - 0.0225 * lcb);
    nh := (1 - t0) / (1 - w) * nr;
    // по методу Холтропа 1984
    if nh > 1.33 then
    begin
      w := C[9] * Cv * L / Ta * (0.0661875 + 1.21756 * C[11] * Cv / (1 - Cp1)) +
        0.24558 * sqrt(B / (L * (1 - Cp1))) - 0.09726 / (0.95 - Cp) + 0.11434 / (0.95 - Cb) + Cstrn * (0.75 * Cv + 0.002);
      if L_B >= 5.2 then
        C[10] := B_L
      else
        C[10] := 0.25 - 0.003328402 / (B_L - 0.134615385);
      t0 := 0.001979 * L / B / (1 - Cp1) + 1.0585 * C[10] - 0.00524 - 0.1418 * D * D / B / T + 0.0015 * Cstrn;
      nr := 0.9922 - 0.05908 * Ae0 + 0.07424 * (Cp - 0.0225 * lcb);
      nh := (1 - t0) / (1 - w) * nr;
      //            ResultsMemo.Lines.Add('Wt     =  '+FloatToStrF(w,ffFixed,6,4));
      //            ResultsMemo.Lines.Add('t0     =  '+FloatToStrF(t0,ffFixed,6,4));
      //            ResultsMemo.Lines.Add('ETAr   =  '+FloatToStrF(nr,ffFixed,6,4));
      //            ResultsMemo.Lines.Add('ETAh   =  '+FloatToStrF(nh,ffFixed,6,4));
      Nver := 1984;
    end;
  end;
  if Np = 2 then
  begin
    w := 0.3095 * Cb + 10 * Cv * Cb - 0.23 * D / sqrt(B * T);
    t0 := 0.325 * Cb - 0.1885 * D / sqrt(B * T);
    nr := 0.9737 + 0.111 * (Cp - 0.0225 * lcb) - 0.06325 * P_D0;
  end;

end;{TFreeResistance_FungLeib.CalculateResistanceFungLeib}

procedure TFreeResistance_FungLeib.File_ExportData(dat, dan: array of single);
var
  I: integer;
  ffile: textfile;
begin
  if FileExistsUTF8(PathFileOld + 'fungdata.dat') { *Converted from FileExists* } then
    DeleteFileUTF8(PChar(PathFile + '\fungdata.dat')); { *Converted from DeleteFile* }
  if FileExistsUTF8(PathFileOld + 'fungdata.res') { *Converted from FileExists* } then
    DeleteFileUTF8(PChar(PathFile + '\fungdata.res')); { *Converted from DeleteFile* }
  Assignfile(FFile, 'fungdata.dat');
      //{$I-}
  Rewrite(FFile);
//{$I+}
  for I := 0 to 29 do
    Writeln(FFile, dat[I]);
  CloseFile(FFile);
end;{TFreeResistance_FungLeib.File_ExportData}

procedure TFreeResistance_FungLeib.File_ExportDataKe(dat, dan: array of single);
var
  I: integer;
  ffile: textfile;
begin
  Assignfile(FFile, 'TMPke.txt');
      //{$I-}
  Rewrite(FFile);
//{$I+}
  for I := 0 to 29 do
    Writeln(FFile, dat[I]);
  for I := 0 to 14 do
    Writeln(FFile, dan[I]);
  CloseFile(FFile);
end;{TFreeResistance_FungLeib.File_ExportDataKe}

end.
