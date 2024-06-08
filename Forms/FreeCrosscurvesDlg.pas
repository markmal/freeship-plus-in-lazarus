{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }

{    Copyright © 2005, by Martijn van Engeland                                                }
{    Copyright © 2007-2012, by Timoshenko Victor F.                                           }
{    e-mail                  : vftim@rambler.ru                                               }
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
unit FreeCrosscurvesDlg;

{$MODE Delphi}

interface

uses
{$IFNDEF LCL}
  Windows,
  TeEngine,
  Series,
  TeeProcs,
  Chart,
{$ELSE}
  LCLIntf, LCLType, 
  TATypes, TATools, TASeries, TACustomSeries, TAGraph, TAChartUtils,
  TAChartAxis, TAChartAxisUtils, TASources,
  PrintersDlgs, Printer4Lazarus, FreePrinter,
{$ENDIF}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  FreeTypes,
  FreeGeometry,
  ExtCtrls,
  FreeshipUnit,
  Spin,
  ComCtrls,
  ImgList,
  Printers,
  Grids,
  ColorFactory;

type

  { TFreeCrosscurvesDialog }

  TFreeCrosscurvesDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Chart: TChart;
    Chart0: TChart;
    CheckBox1: TCheckBox;
    ComboBox: TComboBox;
    ComboBox1: TComboBox;
    DisplBox: TListBox;
    FreeNumInput1: TFloatSpinEdit;
    FreeNumInput10_: TFloatSpinEdit;
    FreeNumInput12_: TFloatSpinEdit;
    FreeNumInput13_: TFloatSpinEdit;
    FreeNumInput2: TFloatSpinEdit;
    FreeNumInput3: TFloatSpinEdit;
    FreeNumInput4: TFloatSpinEdit;
    FreeNumInput5: TFloatSpinEdit;
    FreeNumInput5_: TFloatSpinEdit;
    FreeNumInput6_: TFloatSpinEdit;
    FreeNumInput7_: TFloatSpinEdit;
    FreeNumInput8_: TFloatSpinEdit;
    FreeNumInput9_: TFloatSpinEdit;
    Grid: TStringGrid;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    HeelBox: TListBox;
    Label1: TLabel;
    Label12_: TLabel;
    Label13_: TLabel;
    Label1_: TLabel;
    Label2: TLabel;
    Label2_: TLabel;
    Label3: TLabel;
    Label4_: TLabel;
    Label5_: TLabel;
    Label6_: TLabel;
    Label7_: TLabel;
    Label8_: TLabel;
    Label9_: TLabel;
    MenuImages: TImageList;
    PageControl1: TPageControl;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Resultsmemo: TMemo;
    Series1: TLineSeries;
    Series2: TLineSeries;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    ToolBar1: TToolBar;
    _ToolButton10: TToolButton;
    PrintButton: TToolButton;
    _ToolButton14: TToolButton;
    ToolButton25: TToolButton;
    ToolButton7: TToolButton;
    PrintDialog: TPrintDialog;
    Series: TLineSeries;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    SaveButton: TToolButton;
    SaveDialog: TSaveDialog;
    procedure FreeNumInput5_AfterSetValue(
      Sender: TObject);
    procedure FreeNumInput6_AfterSetValue(
      Sender: TObject);
    procedure ToolButton25Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBoxClick(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure DisplBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure HeelBoxClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure GridDrawCell(Sender: TObject;
      ACol, ARow: integer; Rect: TRect; State: TGridDrawState);
    procedure PrintButtonClick(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private{ Private declarations }
    FFreeship: TFreeship;
    FAbortCalculation: boolean;
  public { Public declarations }
    Nr, RR: integer;
    function Execute(Freeship: TFreeship): boolean;
    procedure CheckDisplacements(var Data: TFloatArray;
      var NData: integer);
    procedure GetDisplacements(var Data: TFloatArray;
      var NData: integer);
    procedure SetDisplacements(Data: TFloatArray;
      NData: integer);
    procedure UpdateDisplacementData;
    procedure UpdateHeelingAngleData;
    procedure CheckHeelingAngles(var Data: TFloatArray;
      var NData: integer);
    procedure GetHeelingAngles(var Data: TFloatArray;
      var NData: integer);
    procedure SetHeelingAngles(Data: TFloatArray;
      NData: integer);
  end;

var
  FreeCrosscurvesDialog: TFreeCrosscurvesDialog;

implementation

uses FreeStringsUnit;

{$R *.lfm}

resourcestring Amplitude_of_heel_angle__2_0_degrees____ = 'Amplitude of heel angle <2.0 degrees !!!';

function TFreeCrosscurvesDialog.Execute(Freeship: TFreeship): boolean;
begin
  FFreeship := Freeship;

  ToolBar1.ButtonWidth := Freeship.Preferences.ToolIconSize;
  ToolBar1.ButtonHeight := Freeship.Preferences.ToolIconSize;

  Freeship.Preferences.LoadImageIntoList(MenuImages, 0, 'Cancel');
  Freeship.Preferences.LoadImageIntoList(MenuImages, 1, 'Ok');
  Freeship.Preferences.LoadImageIntoList(MenuImages, 2, 'Print');
  Freeship.Preferences.LoadImageIntoList(MenuImages, 3, 'Calculate');

  Pagecontrol1.ActivePage := Tabsheet1;
  Chart.Title.Text.Text := rs_Cross_curves {UserString[293]};
  Chart.LeftAxis.Title.Caption :=
    rs_KN_sin_Psi_ {UserString[996]} + ', ' + LengthStr(FFreeship.ProjectSettings.ProjectUnits);
  Chart.BottomAxis.Title.Caption :=
    rs_Displacement {UserString[4]} + ', ' + WeightStr(FFreeship.ProjectSettings.ProjectUnits);
  Chart0.Title.Text.Text := rs_Static_and_Dynamic_Stability_Diagrams__SSD___DSD_ {UserString[766]};
  Chart0.LeftAxis.Title.Caption := rs_GZ___m___GZd____m_ {UserString[767]};
  Chart0.BottomAxis.Title.Caption := rs_Heeling_angle___degr_ {UserString[768]};
  UpdateDisplacementData;
  UpdateHeelingAngleData;
  ShowModal;
  Result := modalResult = mrOk;
end;{TFreeCrosscurvesDialog.Execute}

procedure TFreeCrosscurvesDialog.CheckDisplacements(var Data: TFloatArray;
  var NData: integer);
begin
  SortFloatArray(Data, NData);
  if NData = 0 then
  begin
    Setlength(Data, NData + 1);
    Data[NData] := 0.0;
    Inc(NData);
  end;
end;{TFreeCrosscurvesDialog.CheckDisplacements}

procedure TFreeCrosscurvesDialog.GetDisplacements(var Data: TFloatArray;
  var NData: integer);
var
  I: integer;
begin
  Setlength(Data, DisplBox.Items.Count);
  NData := DisplBox.Items.Count;
  for I := 1 to NData do
    Data[I - 1] := StrToFloat(DisplBox.Items[I - 1]);
  CheckDisplacements(Data, NData);
end;{TFreeCrosscurvesDialog.GetDisplacements}

procedure TFreeCrosscurvesDialog.SetDisplacements(Data: TFloatArray; NData: integer);
var
  I: integer;
begin
  CheckDisplacements(Data, NData);
  DisplBox.Items.BeginUpdate;
  DisplBox.Clear;
  try
    for I := 1 to NData do
      DisplBox.Items.Add(FloatToStrF(Data[I - 1], ffFixed, 7, NumberOfDecimals(Data[I - 1])));
  finally
    DisplBox.Items.EndUpdate;
  end;
end;{TFreeCrosscurvesDialog.SetDisplacements}

procedure TFreeCrosscurvesDialog.ToolButton25Click(Sender: TObject);
begin
  ModalResult := mrOk;
end;{TFreeCrosscurvesDialog.ToolButton25Click}

procedure TFreeCrosscurvesDialog.FreeNumInput5_AfterSetValue(Sender: TObject);
begin
  if (FreeNuminput5_.Value > 0) then
    FreeNuminput5_.Color := clDefault
  else
    FreeNuminput5_.Color := clYellow;
end;

procedure TFreeCrosscurvesDialog.FreeNumInput6_AfterSetValue(Sender: TObject);
begin
  if (FreeNuminput6_.Value > 0) then
    FreeNuminput6_.Color := clDefault
  else
    FreeNuminput6_.Color := clYellow;
end;

procedure TFreeCrosscurvesDialog.ToolButton7Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;{TFreeCrosscurvesDialog.ToolButton7Click}

procedure TFreeCrosscurvesDialog.Button1Click(Sender: TObject);
var
  Data: TFloatArray;
  NData: integer;
begin
  GetDisplacements(Data, NData);
  setlength(Data, NData + 1);
  Data[NData] := FreeNumInput1.Value;
  Inc(NData);
  SetDisplacements(Data, NData);
  UpdateDisplacementData;
  FreeNumInput1.SetFocus;
end;{TFreeCrosscurvesDialog.Button1Click}

procedure TFreeCrosscurvesDialog.Button2Click(Sender: TObject);
var
  Index: integer;
begin
  Index := DisplBox.ItemIndex;
  if (Index <> -1) and (DisplBox.Count > 0) then
  begin
    DisplBox.Items.Delete(Index);
    if Index > DisplBox.Count - 1 then
      Index := DisplBox.Count - 1;
    DisplBox.ItemIndex := Index;
    UpdateDisplacementData;
  end;
end;{TFreeCrosscurvesDialog.Button2Click}

procedure TFreeCrosscurvesDialog.UpdateDisplacementData;
begin
  FreeNuminput1.Enabled := not checkbox1.Checked;
  Button1.Enabled := not checkbox1.Checked;
  Button2.Enabled := (not checkbox1.Checked) and (Displbox.ItemIndex <> -1);
  FreeNuminput2.Enabled := checkbox1.Checked;
  FreeNuminput3.Enabled := checkbox1.Checked;
  FreeNuminput4.Enabled := checkbox1.Checked;
  DisplBox.Enabled := not Checkbox1.Checked;
  if Checkbox1.Checked then
    DisplBox.Color := clBtnFace
  else
    DisplBox.Color := clWindow;
  if FreeNuminput5_.Value > 0.0 then
    FreeNuminput5_.Color := clDefault
  else
    FreeNuminput5_.Color := clYellow;

  if FreeNuminput6_.Value > 0.0 then
    FreeNuminput6_.Color := clDefault
  else
    FreeNuminput6_.Color := clYellow;
end;{TFreeCrosscurvesDialog.UpdateDisplacementData}

procedure TFreeCrosscurvesDialog.UpdateHeelingAngleData;
begin
  Button4.Enabled := Heelbox.ItemIndex <> -1;
end;{TFreeCrosscurvesDialog.UpdateHeelingAngleData}

procedure TFreeCrosscurvesDialog.CheckBox1Click(Sender: TObject);
begin
  UpdateDisplacementData;
end;{TFreeCrosscurvesDialog.CheckBox1Click}

procedure TFreeCrosscurvesDialog.DisplBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  UpdateDisplacementData;
end;{TFreeCrosscurvesDialog.DisplBoxMouseDown}

procedure TFreeCrosscurvesDialog.CheckHeelingAngles(var Data: TFloatArray;
  var NData: integer);
var
  I: integer;
begin
  SortFloatArray(Data, NData);
  while (NData > 0) and (Data[0] < 0) do
  begin
    for I := 2 to NData do
      Data[I - 2] := Data[I - 1];
    Dec(NData);
  end;
  if NData > 0 then
    if Data[0] > 1e-5 then
    begin
      Setlength(Data, NData + 1);
      Data[NData] := 0.0;
      Inc(NData);
      SortFloatArray(Data, NData);
    end;
  if NData = 0 then
  begin
    Setlength(Data, NData + 1);
    Data[NData] := 0.0;
    Inc(NData);
  end;
end;{TFreeCrosscurvesDialog.CheckHeelingAngles}

procedure TFreeCrosscurvesDialog.GetHeelingAngles(var Data: TFloatArray;
  var NData: integer);
var
  I: integer;
begin
  Setlength(Data, HeelBox.Items.Count);
  NData := HeelBox.Items.Count;
  for I := 1 to NData do
    Data[I - 1] := StrToFloat(HeelBox.Items[I - 1]);
  CheckHeelingAngles(Data, NData);
end;{TFreeCrosscurvesDialog.GetHeelingAngles}

procedure TFreeCrosscurvesDialog.SetHeelingAngles(Data: TFloatArray; NData: integer);
var
  I: integer;
begin
  CheckHeelingAngles(Data, NData);
  HeelBox.Items.BeginUpdate;
  HeelBox.Clear;
  try
    for I := 1 to NData do
      HeelBox.Items.Add(FloatToStrF(Data[I - 1], ffFixed, 7, 2));
  finally
    HeelBox.Items.EndUpdate;
  end;
end;{TFreeCrosscurvesDialog.SetHeelingAngles}

procedure TFreeCrosscurvesDialog.Button3Click(Sender: TObject);
var
  Data: TFloatArray;
  NData: integer;
begin
  GetHeelingAngles(Data, NData);
  setlength(Data, NData + 1);
  Data[NData] := FreeNumInput5.Value;
  Inc(NData);
  SetHeelingAngles(Data, NData);
  UpdateHeelingAngleData;
  FreeNumInput5.SetFocus;
end;{TFreeCrosscurvesDialog.Button3Click}

procedure TFreeCrosscurvesDialog.Button4Click(Sender: TObject);
var
  Index: integer;
begin
  Index := HeelBox.ItemIndex;
  if (Index <> -1) and (HeelBox.Count > 0) then
  begin
    HeelBox.Items.Delete(Index);
    if Index > HeelBox.Count - 1 then
      Index := HeelBox.Count - 1;
    HeelBox.ItemIndex := Index;
    UpdateHeelingAngleData;
  end;
end;{TFreeCrosscurvesDialog.Button4Click}

procedure TFreeCrosscurvesDialog.HeelBoxClick(Sender: TObject);
begin
  UpdateHeelingAngleData;
end;{TFreeCrosscurvesDialog.HeelBoxClick}

procedure TFreeCrosscurvesDialog.ComboBoxClick(Sender: TObject);
begin
  // Неограниченный район плавания
  if Combobox.ItemIndex = 0 then
    Nr := 0;
  // Ограниченный R1
  if Combobox.ItemIndex = 1 then
    Nr := 1;
  // Ограниченный R2, R3, R2-RSN R3-RSN
  if Combobox.ItemIndex >= 2 then
    Nr := 2;
end;{TFreeCrosscurvesDialog.ComboBoxClick}

procedure TFreeCrosscurvesDialog.ComboBox1Click(Sender: TObject);
begin
  //    RR : integer;  // Правила Регистра СССР и Росcии до 2010 - RR=0   после 2010 года RR=1
  // Правила Морского Регистра в редакции 2010
  if Combobox1.ItemIndex = 0 then
    RR := 0;
  // Правила Морского Регистра в редакции 1996
  if Combobox1.ItemIndex = 1 then
    RR := 1;
  // По требованиям ГИМС
  if Combobox1.ItemIndex = 2 then
    RR := 2;
  // По РРСУ
  if Combobox1.ItemIndex = 3 then
    RR := 3;
end;{TFreeCrosscurvesDialog.ComboBox1Click}




procedure TFreeCrosscurvesDialog.ToolButton1Click(Sender: TObject);
var
  NHeel: integer;
  NDispl: integer;
  I, J, N, JJ, II, I0, I1: integer;
  Dr, Di, Zgr, h0: single;
  Lmax, Tet0: single;
  L_Teta0, Lpp: single;
  d_Teta, d_Teta0, Tet0d, dteta, Teta_z: single;
  Lmax0, Teta_max, TetaZak, Teta1, T0, Te1: single;
  Value: TFloatType;
  Angles: TFloatArray;
  Displacements: TFloatArray;
  HydObject: TFreehydrostaticCalc;
  Calculation: TFreehydrostaticCalc;
  Data: TFreeCrosscurvesData;
  Units: TFreeUnitType;
  Series: TLineSeries;
  Series1: TLineSeries;
  Series2: TLineSeries;
  PrevCursor: TCursor;
  Total: TLayerProperties;
  l_Teta: array[0..100] of single;
  d_Teta_: array[0..100] of single;
  T_sum: array[0..100] of single;
  l_sum: array[0..100] of single;
  x, X1, X2, Y, hokb: single;
  B, Draft, Cb: single;
  Teta, Ak, k, Zg, r, B_d, l0, Teta_i, L_i, Kw: single;
  Teta0r, Teta1r, S, d, ho, dh, c, Lwl, Tk: single;
  x0, y0, x01, y01, x02, y02, k0, b0, Sa, Sb: single;
  Pv, Zp, Mv, Av, Av_, Zp_, L30, S30, S40: single;
  P0, P1, TetaP, Vb, Vb_, Nraz, Zp2, Zp3: single;
  Zmin, Lmin, Lgr, sin_a: single;
  iii, jjj: integer;
  ColorFactory, ColorFactory1: TColorFactory;

const
  X1_: array[1..14] of single =
    (1, 0.96, 0.93, 0.9, 0.86, 0.82, 0.8, 0.79, 0.78, 0.76, 0.72, 0.68, 0.64, 0.62);
  B_d_: array[1..14] of
    single = (2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.5, 3.6, 4, 4.5, 5, 5.5, 6, 6.5);
  X2_: array[1..6] of single = (0.75, 0.83, 0.89, 0.95, 0.97, 1);
  Cb_: array[1..6] of single = (0.45, 0.5, 0.55, 0.6, 0.65, 0.7);
  S_0: array[1..10] of
    single = (0.1, 0.1, 0.098, 0.093, 0.079, 0.065, 0.053, 0.044, 0.038, 0.035);
  S_1: array[1..10] of
    single = (0.1, 0.093, 0.083, 0.073, 0.053, 0.040, 0.035, 0.035, 0.035, 0.035);
  T_: array[1..10] of
    single = (5, 6, 7, 8, 10, 12, 14, 16, 18, 20);
  Aklb_: array[1..8] of single = (0, 1, 1.5, 2, 2.5, 3, 3.5, 4);
  Ak_: array[1..8] of single = (1, 0.98, 0.95, 0.88, 0.79, 0.74, 0.72, 0.7);
  //const hkb   : array[1..10] of single=(0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12, 0.13);
  //      yneog : array[1..10] of single=(24,25,27,29,30.7,32,33.4,34.4,35.3,36);
  //  ((((((2750661700*x-1695316700)*x+426303140)*x-56031151)*x+4075919.4)*x-158879.9324273)*x+3048.0653733)*x+1.6925274
  //      yI_II : array[1..10] of single=(16,17,19.7,22.8,25.4,27.6,29.2,30.5,31.4,32);
  //  ((((((3024016600*x-1928761900)*x+504136380)*x-69096902)*x+5245882.6)*x-212994.0998629)*x+4239.7519862)*x-16.0136265
const
  clTeeColor = clTAColor;
label
  exit1, exit2, exit3, exit4, exit5;
begin
  sin_a := 0.0;
  ResultsMemo.Clear;
  ResultsMemo.Visible := False;
  II := 1;
  iii := 1;
  GetHeelingAngles(Angles, NHeel);
  if CheckBox1.Checked then
  begin
    if FreeNuminput4.Value > 0 then
    begin
      N := round((FreeNuminput3.Value - FreeNuminput2.Value) / FreeNuminput4.Value) + 10;
      if N > 0 then
        setlength(Displacements, N);
      NDispl := 0;
      Value := FreeNuminput2.Value;
      while (Value <= FreeNuminput3.Value) or (abs(Value - FreeNuminput3.Value) < 1e-4) do
      begin
        Displacements[NDispl] := Value;
        Inc(NDispl);
        Value := Value + FreeNuminput4.Value;
      end;
    end
    else
      MessageDlg(rs_Displacement_step_can_not_be_zero_ {UserString[292]} + '!', mtError, [mbOK], 0);
  end
  else
    GetDisplacements(Displacements, NDispl);

  HydObject := TFreeHydrostaticCalc.Create(FFreeship);
  HydObject.Draft := FFreeship.ProjectSettings.ProjectDraft;
  HydObject.Calculate;
  Zmin := HydObject.Data.ModelMin.Z;
  //          MessageDlg('New Zmin = '+FloatToStrF(Zmin,ffFixed,6,3),mtInformation,[mbOk],0);
  FreeAndNil(HydObject);

  JJ := 0;
  Dr := 0;
  Zgr := 0;
  Di := 0;
  Units := FFreeship.ProjectSettings.ProjectUnits;
  if (FreeNuminput5_.Value > 0) and (FreeNuminput6_.Value <> 0) then
  begin
    Dr := FreeNuminput5_.Value;
    // расчетное водоизмещение в тоннах
    Zgr := FreeNuminput6_.Value;  // ожидаемая Zg в метрах
    if Units = fuImperial then
      Zgr := Zgr / 0.3048;
    Inc(NDispl);
    Displacements[NDispl - 1] := Dr;
    JJ := 1;
    II := NDispl;
    FreeNuminput5_.Color := clDefault;
    FreeNuminput6_.Color := clDefault;
  end
  else
  begin
    FreeNuminput5_.Color := clYellow;
    FreeNuminput6_.Color := clYellow;
  end;


  if (NDispl = 1) and (JJ = 0) then
  begin
    Zgr := FreeNuminput6_.Value;
    HydObject := TFreeHydrostaticCalc.Create(FFreeship);
    HydObject.Draft := FFreeship.ProjectSettings.ProjectDraft;
    HydObject.Calculate;
    HydObject.CalculateGravity;
    //          MessageDlg('D    = '+FloatToStrF(HydObject.Data.Weight_,ffFixed,6,1),mtInformation,[mbOk],0);
    //          MessageDlg('Zg   = '+FloatToStrF(HydObject.Data.CenterOfGravity_.Z,ffFixed,6,3),mtInformation,[mbOk],0);
    Zmin := HydObject.Data.ModelMin.Z;
    //          MessageDlg('Zmin = '+FloatToStrF(Zmin,ffFixed,6,3),mtInformation,[mbOk],0);


    if HydObject.Data.Weight_ > 0 then
    begin
      Di := HydObject.Data.Weight_;
      // Imported D from hydrostatic calc
      Zgr := HydObject.Data.CenterOfGravity_.Z + Zmin;
      // Imported Zg from hydrostatic calc
      Displacements[1] := Di;
      FreeNuminput5_.Value := Di;
      FreeNuminput6_.Value := Zgr;
    end;
    FreeAndNil(HydObject);
    //        MessageDlg('D>0 Zmin = '+FloatToStrF(Zmin,ffFixed,6,3)+' '+'Zgr = '+FloatToStrF(Zgr,ffFixed,6,3),mtInformation,[mbOk],0);

    Chart.Title.Text.Text := rs_GZ_Psi_ {UserString[997]};
    Chart.LeftAxis.Title.Caption :=
      rs_GZ_Psi_ {UserString[997]} + ', ' + LengthStr(FFreeship.ProjectSettings.ProjectUnits);
    if JJ = 0 then
    begin
      if Di > 10 then
        iii := 1
      else
        iii := 2;
      Chart.Title.Text.Text :=
        rs_GZ_Psi_ {UserString[997]} + ' ' + rs_for_D_ {UserString[999]} + FloatToStrF(Displacements[0], ffFixed, 6, iii) +
        ' ' + rs_tonnes_and_Z_CoG__ {UserString[1010]} + FloatToStrF(Zgr, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]};
    end;
  end
  else
  begin
    Chart.Title.Text.Text := rs_Cross_curves {UserString[293]};
    Chart.LeftAxis.Title.Caption :=
      rs_KN_sin_Psi_ {UserString[996]} + ', ' + LengthStr(FFreeship.ProjectSettings.ProjectUnits);
  end;

  if Zmin <> 0 then
    Zgr := Zgr - Zmin;
  //          MessageDlg('if Zmin<>0 Zmin = '+FloatToStrF(Zmin,ffFixed,6,3)+' '+'Zgr = '+FloatToStrF(Zgr,ffFixed,6,3),mtInformation,[mbOk],0);

  PrevCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FAbortCalculation := False;
    ToolButton2.Enabled := True;
    Calculation := TFreehydrostaticCalc.Create(FFreeship);
    for I := Chart.SeriesCount downto 1 do
      Chart.Series[I - 1].Destroy;
    if NDispl = 1 then
    begin
      Grid.ColCount := 2;
      Grid.RowCount := NHeel + 1;
    end
    else
    begin
      Grid.ColCount := NHeel + 1;
      Grid.RowCount := NDispl + 1;
    end;
    for I := 1 to Grid.Rowcount do
      for J := 1 to Grid.Colcount do
        Grid.Cells[J - 1, I - 1] := '';
    I := 1;
    if NDispl = 1 then
      Chart.BottomAxis.Title.Caption := rs_Heeling_angle___degr_ {UserString[768]}
    else
      Chart.BottomAxis.Title.Caption :=
        rs_Displacement {UserString[4]} + ', ' + WeightStr(FFreeship.ProjectSettings.ProjectUnits);
    //Chart.UndoZoom;
    Chart.ZoomFull;

    ColorFactory := TColorFactory.Create;
    ColorFactory1 := TColorFactory.Create;

    while (I <= NHeel) and (not FAbortCalculation) do
    begin
      Calculation.HeelingAngle := Angles[I - 1];
      Calculation.Trim := 0;
      if (NDispl = 1) and (I = 1) then
      begin
        Series := TLineSeries.Create(Chart);
        Chart.AddSeries(Series);
        Series1 := TLineSeries.Create(Chart0);
        Chart0.AddSeries(Series1);
        if Zgr = 0 then
          Series.Title := 'KN sin(Psi)'
        else
          Series.Title := 'GZ,m';
        Series.LinePen.Color := ColorFactory.getNextColorFarFrom(Chart.Color, 199);
        Series.LinePen.Width := 2;
        //Series.LinePen.Visible:=True;
        Series.LinePen.Style := psSolid;
        Series.ShowInLegend := False;
        Series1.Title := 'GZ(Psi), m';
        Series1.LinePen.Color := ColorFactory1.getNextColorFarFrom(Chart.Color, 199);
        //Series1.LinePen.Visible:=True;
        Series1.LinePen.Style := psSolid;
        Series1.LinePen.Width := 2;
        Series1.ShowInLegend := False;
        Pagecontrol1.ActivePage := Tabsheet1;
        Grid.Cells[I, 0] := FloatToStrF(Angles[I - 1], ffFixed, 7, 1) + '°';
        //            MessageDlg(rs_SSD_calculation_for_imported_D_and_Zg {UserString[1124]},mtInformation,[mbOk],0);
        //            MessageDlg(rs_SSD_and_DSD_calculation_for_input_D_and_Zg {UserString[1125]},mtInformation,[mbOk],0);
      end
      else if NDispl <> 1 then
      begin
        Series := TLineSeries.Create(Chart);
        //Series.AllowSinglePoint:=True;
        Series.LinePen.Color := ColorFactory.getNextColorFarFrom(Chart.Color, 199);
        Series.LinePen.Width := 2;
        Chart.AddSeries(Series);
        Series.Title := FloatToStrF(Angles[I - 1], ffFixed, 7, 1) + '°';
        Grid.Cells[I, 0] := FloatToStrF(Angles[I - 1], ffFixed, 7, 1) + '°';
        Series1 := TLineSeries.Create(Chart0);
        Series1.LinePen.Color := ColorFactory1.getNextColorFarFrom(Chart.Color, 199);
        Series1.LinePen.Width := 2;
        //Series1.AllowSinglePoint:=True;
        Series1.Title := rs_GZ_for_Zg_ {UserString[1048]} + FloatToStrF(Zgr + Zmin, ffFixed, 7, 3) + 'm';
      end;

      J := 1;
      while (j <= NDispl) and (not FAbortCalculation) do
      begin
        if I = 1 then
          if NDispl = 1 then
            Grid.Cells[1, 0] := 'KN sin(Psi)'
          else
            Grid.Cells[0, J] := FloatToStrF(Displacements[J - 1], ffFixed, 7, NumberOfDecimals(
              Displacements[J - 1]));
        if NDispl = 1 then
          Grid.Cells[0, I] := FloatToStrF(Angles[I - 1], ffFixed, 7, 1) + '°';

        // Расчет равнообъемного наклонения

        if Calculation.Balance(Displacements[J - 1], False, Data) then
        begin
          if NDispl = 1 then
          begin
            ///                  if Zgr=0 then Series.AddXY(Angles[I-1],Data.CenterOfBuoyancy.y)
            ///                           else begin
            //                                    l_Teta[i-1]:=Data.CenterOfBuoyancy.Y-Zgr*sin(Angles[I-1]/57.29);
            l_Teta[i - 1] := Data.CenterOfBuoyancy.Y - Zgr * sin_a;
            Series.AddXY(Angles[I - 1], l_Teta[i - 1]);
            ///                  end;
            Grid.Cells[1, I] :=
              FloatToStrF(Data.CenterOfBuoyancy.Y + Zmin * sin(Angles[I - 1] / 57.29), ffFixed, 7, 3);
          end
          else
          begin
            //*****************
            Series.AddXY(Data.Displacement, Data.CenterOfBuoyancy.Y + Zmin * sin(Angles[I - 1] / 57.29));
            Grid.Cells[I, J] := FloatToStrF(Data.CenterOfBuoyancy.Y + Zmin * sin(
              Angles[I - 1] / 57.29), ffFixed, 7, 3);
            if (JJ = 1) and (J = II) then
              l_Teta[i - 1] := Data.CenterOfBuoyancy.Y - Zgr * sin(Angles[I - 1] / 57.29);
          end;
          TabSheet1.Visible := False;
          Chart.Refresh;
          Grid.Repaint;
        end
        else
        if NDispl = 1 then
          Grid.Cells[1, I] := '<->'
        else
          Grid.Cells[I, J] := '<->';
        Application.ProcessMessages;
        Inc(J);
      end;
      Inc(I);
      if Series.Count = 0 then
        FreeAndNil(series);
    end;
    TabSheet1.Visible := True;
    FreeAndNil(Calculation);
  finally
    Screen.Cursor := PrevCursor;
    ToolButton2.Enabled := False;
    PrintButton.Enabled := True;
    SaveButton.Enabled := True;
    if FAbortCalculation then
      MessageDlg(Userstring(0294) + '!', mtInformation, [mbOK], 0);
  end;
  // Построение ДСО
  if (Units = fuImperial) and (Dr > 0) and (Zgr > 0) then
  begin
    MessageDlg(rs_Calculation_SSD__DSD_and_weather_criteria_is_available_in_metric_system_ {UserString[1437]}, mtInformation, [mbOK], 0);
    exit;
  end;
  if (Dr > 0) and (Zgr > 0) then
  begin
    Series2 := TLineSeries.Create(Chart0);
    //          MessageDlg(rs_SSD_and_DSD_are_calculated_ {UserString[1126]},mtInformation,[mbOk],0);
    if jj = 1 then
    begin
      Chart0.AddSeries(Series1);
      Chart0.AddSeries(Series2);
      Series1.LinePen.Width := 1;
      Series2.LinePen.Width := 1;
      Series1.Pointer.Style := psCircle;
      Series1.Pointer.VertSize := 3;
      Series1.Pointer.Visible := True;
      Series2.Pointer.Style := psCircle;
      Series2.Pointer.VertSize := 3;
      Series2.Pointer.Visible := True;
      Series1.Pointer.HorizSize := 3;
      //Series1.Pointer.InflateMargins := False;
      Series2.Pointer.HorizSize := 3;
      //Series2.Pointer.InflateMargins := False;

      // added by MM
      Series1.LinePen.Color := clRed;
      Series1.Pointer.Pen.Color := clBlack;
      Series1.Pointer.Pen.Style := psSolid;
      Series1.Pointer.Brush.Color := clRed;
      Series1.Pointer.Brush.Style := bsSolid;
      Series1.ShowPoints := True;

      Series2.LinePen.Color := clGreen;
      Series2.Pointer.Pen.Color := clBlack;
      Series2.Pointer.Pen.Style := psSolid;
      Series2.Pointer.Brush.Color := clGreen;
      Series2.Pointer.Brush.Style := bsSolid;
      Series2.ShowPoints := True;
      Series2.Marks.Style := smsLabel;
      Series2.Marks.Visible := False;  //turn on labels for debugging
      Series2.Source := TListChartSource.Create(Series2);
      TListChartSource(Series2.Source).Sorted := True;
      // end added by MM
    end;
    Pagecontrol1.ActivePage := Tabsheet3;
    Lmax := 0;
    Teta_max := 0;
    Tet0 := 180;
    Tet0d := 180;
    L_Teta0 := 0;
    d_Teta := 0;
    d_Teta0 := 0;
    L30 := 0;
    Series2.AddXY(d_Teta0, d_Teta, 'Init', clTeeColor);
    Series2.Title := rs_GZd_for_Zg_ {UserString[1049]} + FloatToStrF(Zgr + Zmin, ffFixed, 7, 3) + 'm';
    h0 := 0;
    I0 := 0;
    if length(Angles) < (Grid.Colcount - 1)    // by MM
    then
      MessageDlg('Angles count ' + IntToStr(length(Angles)) +
        ' is less than table gol count ' + IntToStr(Grid.Colcount) + '. Will be error',
        mtInformation, [mbOK], 0);


    for I := 1 to Grid.Colcount - 2 do
    begin
      if Lmax < L_Teta[I - 1] then
      begin
        Lmax := L_Teta[I - 1];
        Teta_max := Angles[I - 1];
      end;
      d_Teta0 := d_Teta;
      d_Teta := d_Teta + (L_Teta[I] + L_Teta[I - 1]) * (Angles[I] - Angles[I - 1]) / 2. / 57.3;
      if (d_Teta < 0) and (d_Teta0 > 0) then
        Tet0d := Angles[I];
      if (Angles[I] > 1) and (Angles[I] < Tet0d) then
      begin
        Series2.AddXY(Angles[I], d_Teta, 'DSO', clTeeColor);
        d_Teta_[i] := d_Teta;
      end;
      //        MessageDlg(FloatToStrF(Angles[I],ffFixed,6,3)+' '+FloatToStrF(Angles[I-1],ffFixed,6,3)+' '+FloatToStrF(L_Teta[I],ffFixed,6,3)+' '+FloatToStrF(L_Teta[I-1],ffFixed,6,3)+' '+FloatToStrF(d_Teta,ffFixed,6,5),mtInformation,[mbOk],0);
      if i = 2 then
        h0 := (L_Teta[I - 1] - L_Teta[I - 2]) / Angles[I - 1] * 57.29;
      if L_Teta[I - 1] >= 0 then
      begin
        Series1.AddXY(Angles[I - 1], L_Teta[I - 1], '', clTeeColor);
        if Angles[I - 1] = 30 then
          L30 := L_Teta[I - 1];
      end
      else
      begin
        //          if L_Teta[I-2]-L_Teta[I-1]<>0 then Tet0:=Angles[I-2]+ L_Teta[I-2]/(-L_Teta[I-1]+L_Teta[I-2])*(Angles[I-1]-Angles[I-2]);
        Tet0 := Angles[I - 2] + L_Teta[I - 2] / (-L_Teta[I - 1] + L_Teta[I - 2]) *
          (Angles[I - 1] - Angles[I - 2]);
        if Angles[I - 1] < 50 then
          Series1.AddXY(Angles[I - 1], L_Teta[I - 1], '', clTeeColor);
        if (L_Teta[I - 1] < 0) and (L_Teta[I - 2] > 0) then
        begin
          Series1.AddXY(Tet0, L_Teta0, '', clTeeColor);
          I0 := I - 1;
          TetaZak := Tet0;
        end;
        // Teta_zak<60 град
        if (L_Teta[I - 1] < 0) and (L_Teta[I - 2] > 0) and (Tet0 <= 60) then
          MessageDlg(Userstring(0951), mtInformation, [mbOK], 0);
        // Угол заката ДСО меньше 60°.
      end;
    end;
    if jj > 0 then
    begin
      Lpp := FFreeship.ProjectSettings.ProjectLength;
      if (L_Teta[I0] = 0.0) and (Angles[I0] < 180) then
        MessageDlg(Userstring(0952), mtInformation, [mbOK], 0);
      if (Lmax < 0.25) and (Lpp <= 80) then
        MessageDlg(Userstring(0953) + ' 0,25 ' + rs_m {UserString[451]}, mtInformation, [mbOK], 0);
      if (Lmax < 0.2) and (Lpp >= 105) then
        MessageDlg(Userstring(0953) + ' 0,2 ' + rs_m {UserString[451]}, mtInformation, [mbOK], 0);
      Lmax0 := 0.41 - 0.002 * Lpp;
      if (Lpp > 80) and (Lmax < Lmax0) and (Lpp < 105) then
        MessageDlg(Userstring(0953) + ' ' + FloatToStrF(Lmax0, ffFixed, 6, 2) + ' ' +
          rs_m {UserString[451]}, mtInformation, [mbOK], 0);
      if h0 < 0.0 then
      begin
        MessageDlg(Userstring(0954) + ' ' + FloatToStrF(
          h0, ffFixed, 6, 3) + ' ' + LengthStr(FFreeship.ProjectSettings.ProjectUnits) +
          ' < 0 !', mtInformation, [mbOK], 0);
        exit;
      end;
      ///   MessageDlg('RR='+IntToStr(RR),mtInformation,[mbOk],0);

      if RR = 1 then
      begin
        // Регистр СССР и России до 2010 года

        // Расчет амплитуды качки
        B := FFreeship.ProjectSettings.ProjectBeam;
        hokb := sqrt(h0) / B;
        x := hokb;
        //  Для судна с неограниченным районом плавания
        if Nr = 0 then
        begin
          Y := ((((((2750661700 * x - 1695316700) * x + 426303140) * x - 56031151) * x + 4075919.4) *
            x - 158879.9324273) * x + 3048.0653733) * x + 1.6925274;
          if x < 0.04 then
            Y := 24;
          if x >= 0.13 then
            Y := 36;
        end;
        //  Для судна ограниченного I и II района плавания
        if Nr > 0 then
        begin
          Y := ((((((3024016600 * x - 1928761900) * x + 504136380) * x - 69096902) * x + 5245882.6) *
            x - 212994.0998629) * x + 4239.7519862) * x - 16.0136265;
          if x < 0.04 then
            Y := 16;
          if x >= 0.13 then
            Y := 32;
        end;

        // Расчет кренящего момента от ветра
        HydObject := TFreeHydrostaticCalc.Create(FFreeship);
        HydObject.Draft := FFreeship.ProjectSettings.ProjectDraft;
        HydObject.Calculate;
        Av_ := HydObject.Data.SDP;                  // Imported Sdp from hydrostatic calc
        Zp_ := HydObject.Data.Zsdp;                 // Imported Zsdp from hydrostatic calc
        Zmin := HydObject.Data.ModelMin.Z;
        //     if abs(Zmin)>0.01*HydObject.Draft then begin
        //      MessageDlg('Calculation is impossible, because Zmin='+FloatToStrF(Zmin,ffFixed,6,4)+' is not equal to zero! Move model to baseline...',mtInformation,[mbOk],0);
        //    exit;
        //   end;
        //    MessageDlg('D1>0 Zmin = '+FloatToStrF(Zmin,ffFixed,6,3)+' '+'Zgr = '+FloatToStrF(Zgr,ffFixed,6,3),mtInformation,[mbOk],0);
        FreeAndNil(HydObject);
        Zp := FreeNuminput10_.Value;
        Av := FreeNuminput9_.Value;
        if Av < 0 then
          Av := 0;
        if Zp < 0 then
          Zp := 0.001;
        if Av = 0 then
        begin
          Av := Av_;
          FreeNuminput9_.Value := Av;
        end;
        if Zp = 0 then
        begin
          Zp := Zp_ + Zmin;
          if Zp < 0 then
            Zp := 0.001;
          FreeNuminput10_.Value := Zp;
        end;
        x := Zp;
        if Zp > 7 then
          x := 7;
        if Zp < 0 then
          x := 0;
        Pv := (((((0.020227 * x - 0.6353735) * x + 7.3179725) * x - 37.7133427) * x + 72.6167952) * x +
          109.7547598) * x + 554.0769231;
        Mv := Pv * Zp * Av / 1000;
        if Mv <= 0 then
          exit;
        if Nr = 1 then
          Mv := Mv * 0.567;  // для ограниченного района I
        if Nr = 2 then
          Mv := Mv * 0.275;  // для ограниченного района II
        Draft := FFreeship.ProjectSettings.ProjectDraft;
        x := B / Draft;
        X1 := -0.177972 * x + 1.4275175;
        if x < 2.4 then
          X1 := 1;
        if x >= 3.5 then
          X1 := 0.8;

        Cb := Dr / Lpp / B / Draft / FFreeship.ProjectSettings.ProjectWaterDensity;
        x := Cb;
        X2 := (((((-4000.0023077 * x + 13235.9054069) * x - 17928.2164944) * x + 12690.0260448) *
          x - 4935.7699682) * x + 997.7592868) * x - 80.9816374;
        if x < 0.45 then
          X2 := 0.75;
        if x >= 0.7 then
          X2 := 1;
        // Влияние скуловых килей и/или брускового киля
        Ak := FreeNuminput7_.Value + FreeNuminput8_.Value;
        x := Ak / Lpp / B * 100;  // в %
        k := ((((((0.00040336134 * x - 0.0061699) * x + 0.0337255) * x - 0.0745802) * x + 0.0493198) *
          x - 0.0132194) * x - 0.0042672) * x + 1.0004237;
        if x < 0 then
          k := 1;
        if x >= 4 then
          k := 0.7;
        Teta := Y * X1 * X2 * k;
      end
      else
      begin

        //*************************************************************************************************************************************************

        // По Регистру России после 2010 для судов более 24 м      RR = 0

        // Расчет амплитуды качки
        B := FFreeship.ProjectSettings.ProjectBeam;
        Lwl := FFreeship.ProjectSettings.ProjectLength;    // Lpp=Lwl
        Lpp := Lwl;
        d := FFreeship.ProjectSettings.ProjectDraft;

        //  Расчет исправленной метацентрической высоты (h0-dh)
        dh := FreeNuminput12_.Value;
        Teta_z := FreeNuminput13_.Value;
        if Teta_z <= 0 then
          Teta_z := 90;
        k := 1; // если нет килей
        r := 0.73 + 0.6 * (Zgr - d) / d;
        if r > 1 then
          r := 1;
        c := 0.373 + 0.023 * B / d - 0.043 * Lwl / 100;
        if (h0 - dh) > 0 then
          Tk := 2 * c * B / sqrt(h0 - dh)
        else
        begin
          MessageDlg('Calculation is impossible: ho-dh<0 !', mtInformation, [mbOK], 0);
          exit;
        end;
        //    S=f(Tk)
        if Tk <= 5 then
          S := 0.1;
        if Tk >= 20 then
          S := 0.035;
        if (Tk < 20) and (Tk > 5) then
        begin
          N := 10;
          if Nr = 0 then
            SFINEX1(N, T_, S_0, Tk, S)
          else
            SFINEX1(N, T_, S_1, Tk, S);
        end;
        B_d := B / d;
        if B_d <= 2.4 then
          X1 := 1;
        if B_d >= 6.5 then
          X1 := 0.62;
        if (B_d < 6.5) and (B_d > 2.4) then
        begin
          N := 14;
          SFINEX1(N, B_d_, X1_, B_d, X1);
        end;
        Cb := Dr / Lwl / B / d / FFreeship.ProjectSettings.ProjectWaterDensity;
        if Cb <= 0.45 then
          X2 := 0.75;
        if Cb >= 0.7 then
          X2 := 1;
        if (Cb < 0.7) and (Cb > 0.45) then
        begin
          N := 6;
          SFINEX1(N, Cb_, X2_, Cb, X2);
        end;

        // Влияние скуловых килей и/или брускового киля
        Ak := FreeNuminput7_.Value + FreeNuminput8_.Value;
        x := Ak / Lwl / B * 100;  // в %
        if x <= 0 then
          k := 1.0;
        if x >= 4 then
          k := 0.7;
        if (x < 4) and (x > 0) then
        begin
          N := 8;
          SFINEX1(N, Aklb_, Ak_, x, k);
        end;
        // Если остроскулые обводы то k=0.7

        Teta1r := 109 * k * X1 * X2 * sqrt(r * S);

        // Расчет кренящего момента от ветра
        HydObject := TFreeHydrostaticCalc.Create(FFreeship);
        HydObject.Draft := FFreeship.ProjectSettings.ProjectDraft;
        HydObject.Calculate;
        Av_ := HydObject.Data.SDP;                  // Imported Sdp from hydrostatic calc
        Zp_ := HydObject.Data.Zsdp;                 // Imported Zsdp from hydrostatic calc
        Zmin := HydObject.Data.ModelMin.Z;
        //     if abs(Zmin)>0.01*HydObject.Draft then begin
        //      MessageDlg('Calculation is impossible, because Zmin='+FloatToStrF(Zmin,ffFixed,6,4)+' is not equal to zero! Move model to baseline...',mtInformation,[mbOk],0);
        //      MessageDlg('Zmin='+FloatToStrF(Zmin,ffFixed,6,4),mtInformation,[mbOk],0);
        //    exit;
        //   end;
        //        MessageDlg('D2>0 Zmin = '+FloatToStrF(Zmin,ffFixed,6,3)+' '+'Zgr = '+FloatToStrF(Zgr,ffFixed,6,3),mtInformation,[mbOk],0);
        FreeAndNil(HydObject);

        Zp := FreeNuminput10_.Value;
        Av := FreeNuminput9_.Value;
        if Av < 0 then
          Av := 0;
        if Zp < 0 then
          Zp := 0.001;
        if Av = 0 then
        begin
          Av := Av_;
          FreeNuminput9_.Value := Av;
        end;
        if Zp = 0 then
        begin
          Zp := Zp_ + Zmin;
          if Zp < 0 then
            Zp := 0.001;
          FreeNuminput10_.Value := Zp;
        end;

        Zp2 := Zp + d / 2;  //  Zп + T/2   - плечо кренящего момента
        Zp3 := Zp + d / 4;
        //  Zм/2 + T/4 - плечо кренящего момента при крене 30 град (надо высоту мачты вместо Zп)

        Pv := 504; // для неограниченного района плавания
        Mv := Pv * (Zp + d / 2) * Av / 1000;
        if Mv <= 0 then
          exit;
        if Nr = 1 then
          Mv := Mv * 0.7;  // для ограниченного района I Pv:=353;
        if Nr = 2 then
          Mv := Mv * 0.5;  // для ограниченного района II Pv:=252;
        // Расчет плеча кренящего момента от постоянного ветра без крена
        l0 := Mv / 9.81 / Dr;
        Teta0r := l0 / h0 * 57.29; // в градусах  по линейной теории
        Teta := int(Teta1r + 0.5) - Teta0r;

        // Заполнение массива l(teta)
        // определяем количество точек с углами меньше 0
        for i := 1 to 100 do
          if Angles[I] > Teta then
            goto exit2///       MessageDlg('массив  I='+IntToStr(I)+'Teta='+FloatToStrF(Angles[I],ffFixed,6,3),mtInformation,[mbOk],0);
        ;
        exit2:
          iii := i;

        if NHeel > 100 then
        begin
          MessageDlg('Number of heel angles are more than 100 !!!', mtInformation, [mbOK], 0);
          exit;
        end;

        // Уточняем максимум угла крена на ЛБ и определяем угол входа кромки палубы в воду
        TetaP := 180;
        P0 := 0;
        P1 := 0;
        for i := 0 to NHeel - 1 do
        begin
          T_sum[i] := Angles[I];
          L_sum[i] := l_teta[I];
          if i > 1 then
          begin
            P1 := (L_sum[i] - L_sum[i - 1]) / (T_sum[i] - T_sum[i - 1]);
            if ((P0 > 0) and (P1 < 0)) and (L_sum[i] <= 0) then
              TetaP := T_sum[i - 1];
            ///      MessageDlg('I='+IntToStr(I)+' P0='+FloatToStrF(P0,ffFixed,6,3)+' P1='+FloatToStrF(P1,ffFixed,6,3)+' TetaP='+FloatToStrF(TetaP,ffFixed,6,3),mtInformation,[mbOk],0);
            P0 := P1;
          end;
          ///    MessageDlg('I='+IntToStr(I)+' Teta='+FloatToStrF(T_sum[i],ffFixed,6,3)+' Lteta='+FloatToStrF(L_sum[i],ffFixed,6,3),mtInformation,[mbOk],0);
        end;
        if TetaP < 180 then
        begin
          Teta_z := TetaP;
          FreeNuminput13_.Value := Teta_z;
        end;
        N := 6;
        SFINEX1(N, L_sum, T_sum, l0, T0);
        ///     MessageDlg('Teta_wl='+FloatToStrF(T0,ffFixed,6,3),mtInformation,[mbOk],0);

        // Расчет статического угла крена на ПБ Teta_wl
        if Teta0r < T0 * 1.01 then
          Teta0r := T0;
        if Teta0r > 16 then
          Teta0r := 16;
        if Teta0r > 0.8 * Teta_z then
          Teta0r := 0.8 * Teta_z;
        ///            MessageDlg('Откорректированное Teta0r='+FloatToStrF(Teta0r,ffFixed,6,3),mtInformation,[mbOk],0);
        Teta := int(Teta1r + 0.5) - Teta0r;
        ///            MessageDlg('Teta='+FloatToStrF(Teta,ffFixed,6,3),mtInformation,[mbOk],0);
        if Teta <= 2 then
        begin
          MessageDlg(Amplitude_of_heel_angle__2_0_degrees____,
            mtInformation, [mbOK], 0);
          exit;
        end;

        // Заполняем массивы

        ///   MessageDlg('Lo='+FloatToStrF(1.5*l0,ffFixed,6,3),mtInformation,[mbOk],0);

        for i := 0 to III - 1 do
        begin
          T_sum[i] := -Angles[III - I];
          L_sum[i] := -l_teta[III - I] - 1.5 * l0;
          ///    MessageDlg('I='+IntToStr(I)+' Teta='+FloatToStrF(T_sum[i],ffFixed,6,3)+' Lteta='+FloatToStrF(L_sum[i],ffFixed,6,3),mtInformation,[mbOk],0);
          //    Series2.AddXY(T_sum[i],L_sum[i],'',clTeeColor);
        end;
        for i := III to 40 do
        begin
          T_sum[i] := Angles[I - III];
          L_sum[i] := l_teta[I - III] - 1.5 * l0;
          ///   MessageDlg('I='+IntToStr(I)+' Teta='+FloatToStrF(T_sum[i],ffFixed,6,3)+' Lteta='+FloatToStrF(L_sum[i],ffFixed,6,3),mtInformation,[mbOk],0);
          //    Series2.AddXY(T_sum[i],L_sum[i],'',clTeeColor);
          if (T_sum[i] > Teta_z) or (i > NHeel + 1) then
            goto exit3;
        end;
        exit3:
          iii := i;
        //    MessageDlg('I='+IntToStr(III),mtInformation,[mbOk],0);

        // определяем площади а и в с шагом 1 град
        Sa := 0;
        Te1 := 50;
        I1 := 0;
        for i := 1 to 200 do
        begin
          N := III;
          Teta_i := -Teta + (i - 1) * 1; // углы крена с шагом 1 град
          SFINEX1(N, T_sum, L_sum, Teta_i, l_i);
          if i = 1 then
            Sa := Sa + L_i / 2
          else
            Sa := Sa + L_i;
          //     MessageDlg('Ia='+IntToStr(I)+' Teta='+FloatToStrF(Teta_i,ffFixed,6,3)+' Lteta='+FloatToStrF(L_i,ffFixed,6,3)+' Sa='+FloatToStrF(Sa,ffFixed,6,3),mtInformation,[mbOk],0);
          if L_i >= 0.0 then
          begin
            Teta1 := Teta_i;
            goto exit4;
          end;
        end;
        exit4:
          Sa := -Sa;
        Sb := 0;
        for i := 1 to 200 do
        begin
          N := III;
          Teta_i := Teta1 + (i - 1) * 1; // углы крена с шагом 1 град
          SFINEX1(N, T_sum, L_sum, Teta_i, l_i);
          Sb := Sb + L_i;
          //     MessageDlg('Ib='+IntToStr(I)+' Teta='+FloatToStrF(Teta_i,ffFixed,6,3)+' Lteta='+FloatToStrF(L_i,ffFixed,6,3)+' Sb='+FloatToStrF(Sb,ffFixed,6,3),mtInformation,[mbOk],0);
          if (Teta_i >= Te1) or (Teta_i >= Teta_z * 0.8) then
          begin
            Sb := Sb - L_i / 2;
            goto exit5;
          end;
        end;
        exit5:
          //     MessageDlg(' Sa='+FloatToStrF(Sa,ffFixed,6,3)+' Sb='+FloatToStrF(Sb,ffFixed,6,3),mtInformation,[mbOk],0);
          Kw := Sb / Sa;
        //     MessageDlg(' K='+FloatToStrF(Sb/Sa,ffFixed,6,3),mtInformation,[mbOk],0);
      end;

      //*******************************************************************************************************************************
      // По правилам ГИМС дополнительные данные
      if RR = 2 then
      begin
        Vb := 119 * sqrt(L30 * Dr / Zp3 / Av);
        Vb_ := Vb * 0.6666;
      end;

      // Определение площади под кривой ДСО методом трапеций
      S30 := 0;
      for i := 1 to 30 do
        if Angles[I] <= 30 then
        begin
          S30 := S30 + (Angles[I] - Angles[I - 1]) / 2 * (L_Teta[I - 1] + L_Teta[I]);
          S40 := S30;
          ///         MessageDlg('Teta='+FloatToStrF(Angles[I],ffFixed,6,3)+' '+FloatToStrF(Angles[I-1],ffFixed,6,3),mtInformation,[mbOk],0);
          ///         MessageDlg('L_Teta='+FloatToStrF(L_Teta[I],ffFixed,6,3)+' '+FloatToStrF(L_Teta[I-1],ffFixed,6,3),mtInformation,[mbOk],0);
          ///         MessageDlg('S30='+FloatToStrF(S30,ffFixed,6,3),mtInformation,[mbOk],0);
        end
        else
        if (Angles[I] <= 40) and (Angles[I] > 30) then
          S40 := S40 + (Angles[I] - Angles[I - 1]) / 2 * (L_Teta[I - 1] + L_Teta[I])
          ///         MessageDlg('Teta='+FloatToStrF(Angles[I],ffFixed,6,3)+' '+FloatToStrF(Angles[I-1],ffFixed,6,3),mtInformation,[mbOk],0);
          ///         MessageDlg('L_Teta='+FloatToStrF(L_Teta[I],ffFixed,6,3)+' '+FloatToStrF(L_Teta[I-1],ffFixed,6,3),mtInformation,[mbOk],0);
          ///         MessageDlg('S40='+FloatToStrF(S40,ffFixed,6,3),mtInformation,[mbOk],0);
        else if Angles[I] > 40 then
          goto exit1;
      exit1:
          ///           MessageDlg('S30_Teta='+FloatToStrF(S30/57.29,ffFixed,6,3),mtInformation,[mbOk],0);
          ///           MessageDlg('S40_Teta='+FloatToStrF(S40/57.29,ffFixed,6,3),mtInformation,[mbOk],0);



          // Определение максимальной работы кренящего момента
        for i := 1 to 10 do
          if Angles[I] < Teta then
            Series2.AddXY(-Angles[I], d_Teta_[I], 'Max Work: A<T', clTeeColor)
          else
          begin
            N := i;
            SFINEX1(N, Angles, d_Teta_, Teta, dteta);
            Series2.AddXY(-Teta, dteta, 'Max Work: A>=T', clTeeColor);
          end;
      // Расчет уравнения касательной
      x0 := -Teta;
      y0 := dteta;
      for I := 1 to Grid.Colcount do
      begin
        x01 := Angles[i];
        x02 := Angles[i + 1];
        y01 := d_Teta_[i];
        y02 := d_Teta_[i + 1];
        k0 := (y02 - y01) / (x02 - x01);
        b0 := k0 * (x0 - x01) + y01;
        if b0 > y0 then
        begin
          if (x01 < 50) and (I1 = 0) then
          begin
            Te1 := x01;
            Sa := -Sa;
            if Sa = 0 then
              Sa := 0.01;
            I1 := 1;
            goto exit4;
          end;
          b0 := (y01 - y0) / (x01 - x0) * 57.3;
          b0 := b0 * Dr * 9.81;
          if b0 > 10 then
            iii := 1
          else
            iii := 2;
          if Mv < 1 then
            iii := 3;
          if Mv > 10 then
            jjj := 1
          else
            jjj := 2;
          if Mv < 1 then
            jjj := 3;
          if (TetaZak = 0.0) or (TetaZak >= 180.0) then
            TetaZak := 180.0;
          if RR = 1 then
            Kw := b0 / Mv;
          if RR = 2 then
            Kw := 1;
          if Kw < 1 then
            MessageDlg(rs_Unsatisfactory_stability__because_Weather_criteria_K__ {UserString[1127]} + FloatToStrF(Kw, ffFixed, 6, 3) + '<1 !!!',
              mtInformation, [mbOK], 0);
          ResultsMemo.Lines.Add(' ');
          if RR < 2 then
            ResultsMemo.Lines.Add(Space(4) + rs_Stability_calculation_results {UserString[1438]} + ' ' + rs_by_Russian_Registry_for_sea_ships {UserString[1439]} + ':');
          if RR = 2 then
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Caclulation_added_SSD_parameters_by_GIMS__Russia__ {UserString[1459]}, 50));
          if RR = 3 then
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Stability_calculation_according_to_the_rules_of_the_River_Register_of_Shipping_of_Ukraine_ {UserString[1470]}, 50));
          ResultsMemo.Lines.Add(' ');

          // Вывод исходных данных
          ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Estimated_displacement_D {UserString[1440]}, 50) +
            ': ' + FloatToStrF(Dr, ffFixed, 6, 3) + ' ' + rs_tonnes {UserString[474]});
          ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Z_coordinate_of_CoG {UserString[1441]}, 50) +
            ': ' + FloatToStrF(Zgr + Zmin, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]});
          ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Bilge_keels_area {UserString[1460]}, 50) +
            ': ' + FloatToStrF(FreeNuminput7_.Value, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]} + '^2');
          ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Skeg_area {UserString[1461]}, 50) +
            ': ' + FloatToStrF(FreeNuminput8_.Value, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]} + '^2');
          ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Lateral_wind_area {UserString[1462]}, 50) +
            ': ' + FloatToStrF(Av, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]} + '^2');
          ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Distance_from_wind_area_CoG_to_DWL {UserString[1135]}, 50) +
            ': ' + FloatToStrF(Zp, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]});
          ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Wind_area_righting_lever {UserString[1472]}, 50) +
            ': ' + FloatToStrF(Zp + d / 2, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]});
          // Вывод результатов расчета ДСО и ДДО
          if RR = 0 then
          begin
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Heeling_angle_for_constant_wind_speed {UserString[1468]}, 50) +
              ': ' + FloatToStrF(Teta0r, ffFixed, 6, 1) + ' ' + rs_degr {UserString[455]});
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Amplitude_of_roll {UserString[1128]}, 50) +
              ': ' + FloatToStrF(int(Teta1r + 0.5), ffFixed, 6, 1) + ' ' + rs_degr {UserString[455]});
          end;
          ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Initial_transverse_metacentric_height_ho {UserString[1030]}, 50) +
            ': ' + FloatToStrF(h0, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]});
          if Lpp <= 80 then
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Maximal_GZ {UserString[1446]}, 50) + ': ' +
              Makelength(FloatToStrF(Lmax, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]}, 11) + ' ( 0.25)');
          if Lpp >= 105 then
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Maximal_GZ {UserString[1446]}, 50) + ': ' +
              Makelength(FloatToStrF(Lmax, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]}, 11) + ' ( 0.20)');
          if (Lpp > 80) and (Lpp < 105) then
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Maximal_GZ {UserString[1446]}, 50) + ': ' +
              Makelength(FloatToStrF(Lmax, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]}, 11) + ' (' +
              FloatToStrF(Lmax0, ffFixed, 6, 3) + ')');
          if RR = 2 then
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Heeling_angle_for_GZ_maximum {UserString[1447]}, 50) + ': ' +
              Makelength(FloatToStrF(Teta_max, ffFixed, 6, 1) + ' ' + rs_degr {UserString[455]}, 11) + ' ( 25° )')
          else
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Heeling_angle_for_GZ_maximum {UserString[1447]}, 50) + ': ' +
              Makelength(FloatToStrF(Teta_max, ffFixed, 6, 1) + ' ' + rs_degr {UserString[455]}, 11) + ' ( 30° )');
          ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Heeling_angle_at_which_righting_lever_0_again {UserString[1448]}, 50) +
            ': ' + Makelength(FloatToStrF(TetaZak, ffFixed, 6, 1) + ' ' + rs_degr {UserString[455]}, 11) + ' ( 60° )');
          if RR = 1 then
          begin
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Maximum_dynamic_heeling_angle {UserString[1129]}, 50) +
              ': ' + FloatToStrF(x01, ffFixed, 6, 1) + ' ' + rs_degr {UserString[455]});
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Maximal_heeling_moment {UserString[1130]}, 50) +
              ': ' + FloatToStrF(b0, ffFixed, 7, iii) + ' ' + rs_kNom {UserString[1133]});
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Maximal_wind_moment {UserString[1131]}, 50) +
              ': ' + FloatToStrF(Mv, ffFixed, 6, jjj) + ' ' + rs_kNom {UserString[1133]});
          end;
          if RR <> 2 then
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Weather_criteria_K {UserString[1132]}, 50) + ': ' +
              Makelength(FloatToStrF(Kw, ffFixed, 6, 3), 11) + ' ( 1.0 )');
          if RR = 0 then
          begin
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Corrected_metacentric_height {UserString[1466]}, 50) +
              ': ' + Makelength(FloatToStrF(h0 - dh, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]}, 11) + ' (0.150)');
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Required_area_of_GZ_curve_from_0__to_30_ {UserString[1463]}, 50) +
              ': ' + Makelength(FloatToStrF(S30 / 57.29, ffFixed, 6, 3) + ' ' + rs_morad {UserString[1467]}, 11) + ' (0.055)');
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Required_area_of_GZ_curve_from_0__to_40_ {UserString[1464]}, 50) +
              ': ' + Makelength(FloatToStrF(S40 / 57.29, ffFixed, 6, 3) + ' ' + rs_morad {UserString[1467]}, 11) +
              ' (0.090)');
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Required_area_of_GZ_curve_from_30__to_40_ {UserString[1465]}, 50) +
              ': ' + Makelength(FloatToStrF((S40 - S30) / 57.29, ffFixed, 6, 3) + ' ' + rs_morad {UserString[1467]}, 11) +
              ' (0.030)');
          end;

          if (L30 > 0) and (RR = 2) then
          begin
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Corrected_metacentric_height {UserString[1466]}, 50) +
              ': ' + Makelength(FloatToStrF(h0 - dh, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]}, 11) + ' ( 0.50)');
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Transverse_righting_lever_for_heeling_angle_30_ {UserString[1457]}, 50) +
              ': ' + FloatToStrF(L30, ffFixed, 6, 3) + ' ' + rs_m {UserString[451]});
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Transverse_moment_of_stability_for_heel_angle_30_ {UserString[1458]}, 50) +
              ': ' + FloatToStrF(L30 * Dr * 9.81, ffFixed, 6, iii) + ' ' + rs_kNom {UserString[1133]});
            ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Permissible_wind_speed {UserString[1471]}, 50) +
              ': ' + FloatToStrF(Vb_, ffFixed, 6, 2) + ' ' + rs_m_s {UserString[1405]});
            Nraz := 0;
            if Vb_ >= 10 then
              Nraz := 1;
            if (Vb_ >= 8.5) and (Vb_ < 10) then
              Nraz := 2;
            if (Vb_ >= 6) and (Vb_ < 8.5) then
              Nraz := 3;
            if (Vb_ >= 4) and (Vb_ < 6) then
              Nraz := 4;
            if (Vb_ >= 2) and (Vb_ < 4) then
              Nraz := 5;
            if Nraz > 0 then
              ResultsMemo.Lines.Add(Space(4) + Makelength(rs_Corresponds_to_category_navigation {UserString[1473]}, 50) + ': ' +
                FloatToStrF(Nraz, ffFixed, 6, 0))
            else
              ResultsMemo.Lines.Add(Space(4) + Makelength(rs_The_vessel_does_not_correspond_to_any_category_of_navigation____ {UserString[1474]}, 50));
          end;


          ///            ResultsMemo.Lines.Add(Space(4)+Makelength('X coord Wind Area',50)+': '+FloatToStrF(HydObject.Data.XWindAreaMax,ffFixed,6,3));
          ///            ResultsMemo.Lines.Add(Space(4)+Makelength('Y coord Wind Area',50)+': '+FloatToStrF(HydObject.Data.YWindAreaMax,ffFixed,6,3));
          ResultsMemo.Lines.Add('');
          ResultsMemo.Lines.Add(Space(4) + rs_Note__In_parentheses_the_minimum_values_of_demanded_parameters_are_specified {UserString[1469]});
          ResultsMemo.Lines.Add('');
          ResultsMemo.Lines.Add(Space(4) + 'Copyright (C) 2010-2012, Timoshenko V.F.');
          ResultsMemo.Visible := True;
          exit;
        end;
      end;
    end;
  end;
end;{TFreeCrosscurvesDialog.ToolButton1Click}

procedure TFreeCrosscurvesDialog.GridDrawCell(Sender: TObject;
  ACol, ARow: integer; Rect: TRect; State: TGridDrawState);
var
  W: integer;
  Str: string;
  Back: TColor;
begin
  if (ACol = 0) or (ARow = 0) then
  begin
    if Grid.Canvas.Font.Style = [] then
      Grid.Canvas.Font.Style := [fsBold];
  end
  else
  if Grid.Canvas.Font.Style = [fsBold] then
    Grid.Canvas.Font.Style := [];
  if (ARow > 0) and (ACol > 0) then
  begin
    if gdSelected in state then
    else if not Odd(ARow) then
    begin
      Back := RGB(235, 235, 235);
      if Grid.Canvas.Brush.Color <> Back then
        Grid.Canvas.Brush.Color := Back;
    end
    else
    if Grid.Canvas.Brush.Color <> clWindow then
      Grid.Canvas.Brush.Color := clWindow;
    Grid.Canvas.Rectangle(Rect);
  end;
  Str := Grid.Cells[ACol, ARow];
  W := Grid.Canvas.TextWidth(Str);
  if (ARow in [0, 1]) or (ACol = 0) then
  begin
    Grid.Canvas.FillRect(Rect);
    Grid.Canvas.TextRect(Rect, (Rect.Left + Rect.Right - W) div 2, Rect.Top + 3, Str);
  end
  else
    Grid.Canvas.TextRect(Rect, Rect.Right - W - 5, Rect.Top + 3, Str);
end;{TFreeCrosscurvesDialog.GridDrawCell}

procedure TFreeCrosscurvesDialog.PrintButtonClick(Sender: TObject);
var
  PrintText: TextFile;
  MaxWidth: array of integer;
  I, J, L, N: integer;
  Str, Tmp: ansistring;
begin
  if PrintDialog.Execute then
  begin
    AssignPrn(PrintText);
    Rewrite(PrintText);
    Setlength(MaxWidth, Grid.ColCount);
    for I := 1 to Grid.ColCount do
    begin
      MaxWidth[I - 1] := 0;
      for J := 1 to Grid.RowCount do
      begin
        L := Length(Grid.Cells[I - 1, J - 1]);
        if L > MaxWidth[I - 1] then
          MaxWidth[I - 1] := L;
      end;
    end;
    Writeln(PrintText);
    Writeln(PrintText);
    Printer.Canvas.Font.Size := Printer.Canvas.Font.Size - 1;
    for I := 1 to Grid.RowCount do
    begin
      Str := '';
      for J := 1 to Grid.ColCount do
      begin
        Tmp := Grid.Cells[J - 1, I - 1];
        L := Length(Tmp);
        for N := L + 1 to MaxWidth[J - 1] do
          Tmp := #32 + Tmp;
        if J = 1 then
          Str := Tmp
        else
          Str := Str + #32 + Tmp;
      end;
      Writeln(PrintText, #32#32, Str);
    end;
    Printer.Canvas.Font.Assign(ResultsMemo.Font);
    for N := 0 to ResultsMemo.Lines.Count - 1 do
      Writeln(PrintText, ResultsMemo.Lines[N]);

    CloseFile(PrintText);
    //Chart.Print;
    //Chart0.Print;
    Chart.PaintOnCanvas(Printer.Canvas, Rect(0, 0, Chart.Width, Chart.Height));
    Chart0.PaintOnCanvas(Printer.Canvas, Rect(0, 0, Chart.Width, Chart.Height));

  end;
end;{TFreeCrosscurvesDialog.PrintButtonClick}

procedure TFreeCrosscurvesDialog.ToolButton2Click(Sender: TObject);
begin
  FAbortCalculation := True;
end;{TFreeCrosscurvesDialog.ToolButton2Click}

procedure TFreeCrosscurvesDialog.SaveButtonClick(Sender: TObject);
var
  Strings: TStringList;
  MaxWidth: array of integer;
  I, J, L, N: integer;
  Str, Tmp: ansistring;
begin
  SaveDialog.InitialDir := FFreeship.Preferences.ExportDirectory;
  SaveDialog.FileName := ChangeFileExt(rs_Cross_curves {UserString[293]}, '.txt');
  if SaveDialog.Execute then
  begin
    strings := TStringList.Create;
    Setlength(MaxWidth, Grid.ColCount);
    for I := 1 to Grid.ColCount do
    begin
      MaxWidth[I - 1] := 0;
      for J := 1 to Grid.RowCount do
      begin
        L := Length(Grid.Cells[I - 1, J - 1]);
        if L > MaxWidth[I - 1] then
          MaxWidth[I - 1] := L;
      end;
    end;
    Strings.Add('');
    Strings.Add('');
    for I := 1 to Grid.RowCount do
    begin
      Str := '';
      for J := 1 to Grid.ColCount do
      begin
        Tmp := Grid.Cells[J - 1, I - 1];
        L := Length(Tmp);
        for N := L + 1 to MaxWidth[J - 1] do
          Tmp := #32 + Tmp;
        if J = 1 then
          Str := Tmp
        else
          Str := Str + #32 + Tmp;
      end;
      Strings.Add(Str);
    end;
    for N := 0 to ResultsMemo.Lines.Count - 1 do
    begin
      Str := ResultsMemo.Lines[N];
      Strings.Add(Str);
    end;
    // Skip translation
    Strings.SaveToFile(ChangeFileExt(SaveDialog.Filename, '.txt'));
    // End Skip translation
    FreeAndNil(Strings);
  end;
end;{TFreeCrosscurvesDialog.SaveButtonClick}

end.
