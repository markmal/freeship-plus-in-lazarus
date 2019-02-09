{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2008-2009, by Timoshenko Victor F.                                           }
{    e-mail                  : vftim@rambler.ru, tvf@pisem.net                                }
{    FREE!ship project page  : http://freeship-plus.land.ru                                   }
{    FREE!ship homepage      : http://freeship-plus.pisem.su                                  }
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
unit FreeAddMassOutputDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  jpeg, Windows,
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
     Buttons,
     ExtCtrls,
     ComCtrls,
     FreeShipUnit,
     StdCtrls,
     FreeTypes,
     FreeGeometry,
     ActnList,
     Spin,
     FreeLanguageSupport;
	 
type TFreeAddMassOutputDialog  = class(TForm)
                                    Panel1: TPanel;
                                    OKbutton: TSpeedButton;
                                    CancelButton: TSpeedButton;
                                    Panel2: TPanel;
                                    Panel3: TPanel;
                                    ActionList1: TActionList;
                                    SaveFile: TAction;
                                    PageControl1: TPageControl;
                                    TabSheet1: TTabSheet;
                                    Panel4: TPanel;
                                    Label1: TLabel;
                                    Label2: TLabel;
                                    Label3: TLabel;
                                    Label4: TLabel;
                                    Label5: TLabel;
    _Label18: TLabel;
    _Label19: TLabel;
    _Label20: TLabel;
                                    Edit1: TSpinEdit;
                                    Edit2: TSpinEdit;
                                    Edit3: TFloatSpinEdit;
                                    Edit4: TFloatSpinEdit;
                                    Edit5: TFloatSpinEdit;
                                    TabSheet3: TTabSheet;
                                    Panel7: TPanel;
                                    Label6: TLabel;
                                    Label7: TLabel;
    _Label8: TLabel;
    _Label9: TLabel;
                                    Label10: TLabel;
    _Label11: TLabel;
                                    Label12: TLabel;
    _Label13: TLabel;
                                    Edit6: TFloatSpinEdit;
                                    Edit7: TFloatSpinEdit;
                                    Edit8: TFloatSpinEdit;
                                    Edit9: TFloatSpinEdit;
                                    TabSheet2: TTabSheet;
                                    Resultsmemo: TMemo; 									
                                    TabSheet4: TTabSheet;
                                    Resultsmemo2: TMemo; 																		
                                    GroupBox3: TGroupBox;
                                    RadioButton1: TRadioButton;
                                    RadioButton2: TRadioButton;
                                    RadioButton3: TRadioButton;
                                    Label14: TLabel;
                                    Distance: TFloatSpinEdit;
                                    _Label15: TLabel;
                                    _Label16: TLabel;
                                    _Label17: TLabel;
                                    Label20: TLabel;
                                    Label21: TLabel;
                                    _Label21: TLabel;
                                    Label22: TLabel;
                                    Label23: TLabel;
                                    Label24: TLabel;
                                    Label25: TLabel;
                                    Label26: TLabel;
                                    Label27: TLabel;
                                    ComboBox: TComboBox;
                                    Edit20: TFloatSpinEdit;
                                    Edit21: TFloatSpinEdit;
                                    Edit22: TFloatSpinEdit;
                                    Edit23: TFloatSpinEdit;
                                    Edit24: TFloatSpinEdit;
                                    Edit25: TFloatSpinEdit;
                                    Edit26: TFloatSpinEdit;
                                    Panel6: TPanel;
                                    ScrollBar1: TScrollBar;
                                    ScrollBar2: TScrollBar;
                                    Viewport: TFreeViewport;
                                    procedure ComboBoxClick(Sender: TObject);
                                    procedure OKbuttonClick(Sender: TObject);
                                    procedure CancelButtonClick(Sender: TObject);
                                    procedure ViewportRequestExtents(Sender: TObject; var Min,Max: T3DCoordinate);
                                    procedure ViewportRedraw(Sender: TObject);
                                    procedure RadioButton1Click(Sender: TObject);
                                    procedure DistanceAfterSetValue(Sender: TObject);
                                    procedure Edit1AfterSetValue(Sender: TObject);
                                    procedure Edit2AfterSetValue(Sender: TObject);
                                    procedure Edit3AfterSetValue(Sender: TObject);
                                    procedure Edit4AfterSetValue(Sender: TObject);
                                    procedure Edit5AfterSetValue(Sender: TObject);
                                    procedure Edit6AfterSetValue(Sender: TObject);
                                    procedure Edit7AfterSetValue(Sender: TObject);
                                    procedure Edit8AfterSetValue(Sender: TObject);
                                    procedure Edit9AfterSetValue(Sender: TObject);
                                    procedure Edit20AfterSetValue(Sender: TObject);
                                    procedure Edit21AfterSetValue(Sender: TObject);
                                    procedure Edit22AfterSetValue(Sender: TObject);
                                    procedure Edit23AfterSetValue(Sender: TObject);
                                    procedure Edit24AfterSetValue(Sender: TObject);
                                    procedure Edit25AfterSetValue(Sender: TObject);
                                    procedure Edit26AfterSetValue(Sender: TObject);
    procedure FormResize(Sender: TObject);
                                 private { Private declarations }
                                    FFreeship     : TFreeShip;
//                                    NumberOfHulls : Integer;
                                    Offsets       : array of array of single;
                                    procedure FBuildOffsets;
                                    function FGetNumberOfStations:Integer;
                                    procedure FSetNumberOfStations(val:Integer);
                                    function FGetNumberOfWaterlines:Integer;
                                    procedure FSetNumberOfWaterlines(val:Integer);
                                    function FGetDraft:single;
                                    procedure FSetDraft(val:single);
                                    function FGetLength:single;
                                    procedure FSetLength(val:single);
                                    function FGetVolume:single;
                                    procedure FSetVolume(val:single);
                                    function FGetAngle:single;
                                    procedure FSetAngle(val:single);
                                    function FGetAngleBeta:single;
                                    procedure FSetAngleBeta(val:single);
                                    function FGetVs:single;
                                    procedure FSetVs(val:single);
                                    function FGetOm:single;
                                    procedure FSetOm(val:single);
                                    function FGetWaterDensity:single;
                                    procedure FSetWaterDensity(val:single);
                                    function FGetWaterViscosity:single;
                                    procedure FSetWaterViscosity(val:single);
                                    function FGetWaterDepth:single;
                                    procedure FSetWaterDepth(val:single);
                                    function FGetDat1:single;
                                    procedure FSetDat1(val:single);
                                    function FGetDat2:single;
                                    procedure FSetDat2(val:single);
                                    function FGetDat3:single;
                                    procedure FSetDat3(val:single);
                                    function FGetDat4:single;
                                    procedure FSetDat4(val:single);
                                    function FGetMultihull:boolean;
                                    procedure createViewport();
                                 public { Public declarations }
                                    NType                         : integer;
                                    function Execute(Freeship:TFreeShip):Boolean;
                                    procedure SaveToFile(Filename:string);
                                    property Draft                : single read FGetDraft write FSetDraft;
                                    property Angle                : single read FGetAngle write FSetAngle;
                                    property AngleBeta            : single read FGetAngleBeta write FSetAngleBeta;
                                    property Vs                   : single read FGetVs write FSetVs;
                                    property Om                   : single read FGetOm write FSetOm;
                                    property Length               : single read FGetLength write FSetLength;
                                    property Multihull            : Boolean read FGetMultihull;
                                    property NumberOfStations     : Integer read FGetNumberOfStations write FSetNumberOfStations;
                                    property NumberOfWaterlines   : Integer read FGetNumberOfWaterlines write FSetNumberOfWaterlines;
                                    property Volume               : single read FGetVolume write FSetVolume;
                                    property WaterDensity         : single read FGetWaterDensity write FSetWaterDensity;
                                    property WaterDepth           : single read FGetWaterDepth write FSetWaterDepth;
                                    property WaterViscosity       : single read FGetWaterViscosity write FSetWaterViscosity;
                                    property Dat1                 : single read FGetDat1 write FSetDat1;
                                    property Dat2                 : single read FGetDat2 write FSetDat2;
                                    property Dat3                 : single read FGetDat3 write FSetDat3;
                                    property Dat4                 : single read FGetDat4 write FSetDat4;
                                 end;
var FreeAddMassOutputDialog: TFreeAddMassOutputDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFreeAddMassOutputDialog.FBuildOffsets;
var I,J,K,L,N   : Integer;
    Min,Max,P   : T3DCoordinate;
    First       : Boolean;
    WlHeight    : TFloatType;
    StSpacing   : TFloatType;
    WlSpacing   : TFloatType;
    Dist        : TFloatType;
    Layer       : TFreesubdivisionLayer;
    Ctrlface    : TFreeSubdivisionControlface;
    Face        : TFreeSubdivisionface;
    Station     : TFreeIntersection;
    Spline      : TFreeSpline;
    Plane       : T3DPlane;
    Int         : TFreeIntersectionData;
    Hydrostat   : TFreeHydrostaticCalc;

begin
   if FFreeship=nil then exit;

   // Determine lowest point of the hull
   First:=True;
   for I:=1 to FFreeship.Surface.NumberOfLayers do
   begin
      Layer:=FFreeship.Surface.Layer[I-1];
      if Layer.UseInHydrostatics then for J:=1 to Layer.Count do
      begin
         Ctrlface:=Layer.Items[J-1];
         if First then
         begin
            Min:=Ctrlface.Min;
            Max:=Ctrlface.Max;
            First:=False;
         end else
         begin
            MinMax(Ctrlface.Min,Min,Max);
            MinMax(Ctrlface.Max,Min,Max);
         end;
      end;
   end;
   WlHeight:=Min.Z+Draft;

   // initialize offsets data
   Setlength(Offsets,NumberOfStations);
   for I:=1 to NumberOfStations do
   begin
      Setlength(Offsets[I-1],NumberOfWaterlines);
      for J:=1 to NumberOfWaterlines do Offsets[I-1][J-1]:=0.0;
   end;

   // Determine min/max underwatercoordinates
   First:=True;
   for I:=1 to FFreeship.Surface.NumberOfLayers do
   begin
      Layer:=FFreeship.Surface.Layer[I-1];
      if Layer.UseInHydrostatics then for J:=1 to Layer.Count do
      begin
         Ctrlface:=Layer.Items[J-1];
         for k:=1 to CtrlFace.ChildCount do
         begin
            face:=Ctrlface.Child[K-1];
            for L:=1 to face.NumberOfpoints do
            begin
               P:=Face.Point[L-1].Coordinate;
               if P.Z<=WlHeight then
               begin
                  if First then
                  begin
                     Min:=P;
                     Max:=P;
                     First:=False;
                  end else MinMax(P,Min,Max);
               end;
            end;
         end;
      end;
   end;
   if RadioButton3.Checked then Dist:=0.5*Distance.Value
                           else Dist:=0;
   // Build offsets table
   for I:=NumberOfStations-1 downto 1 do
   begin
      StSpacing:=Min.X+((I-1)/(NumberOfStations-1))*(Max.X-Min.X);
      Station:=TFreeIntersection.Create(FFreeship);
      Plane.a:=1.0;
      Plane.b:=0.0;
      Plane.c:=0.0;
      Plane.d:=-StSpacing;
      Station.Plane:=Plane;
      Station.Rebuild;
      Plane.a:=0.0;
      Plane.b:=0.0;
      Plane.c:=1.0;
      for N:=1 to Station.Count do
      begin
         Spline:=Station.Items[N-1];
         for J:=1 to NumberOfWaterlines do
         begin
            WlSpacing:=Min.Z+(J-1)/(NumberOfWaterlines-1)*Draft;
            Plane.d:=-WlSpacing;                                                  
            if Spline.IntersectPlane(Plane,Int) then
            begin
               for K:=1 to Int.NumberOfIntersections do if Int.Points[K-1].Y>=Dist-1e-4 then
               begin
                  Offsets[NumberOfStations-I][J-1]:=Int.Points[K-1].Y;
                  if RadioButton3.Checked then
                  begin
                     Offsets[NumberOfStations-I][J-1]:=Offsets[NumberOfStations-I][J-1]-Dist;
                     if Offsets[NumberOfStations-I][J-1]<0 then Offsets[NumberOfStations-I][J-1]:=0;
                  end;
               end;
            end;
         end;
      end;
      Station.Destroy;
   end;
   Viewport.ZoomExtents;
   Hydrostat:=TFreeHydrostaticCalc.Create(FFreeship);
   Hydrostat.Draft:=Draft;
   Hydrostat.Calculate;
   Volume:=Hydrostat.Data.Volume;
   Hydrostat.Destroy;

end;{TFreeAddMassOutputDialog.FBuildOffsets}

function TFreeAddMassOutputDialog.FGetDraft:single;
begin
   Result:=Edit3.value;
end;{TFreeAddMassOutputDialog.FGetDraft}

procedure TFreeAddMassOutputDialog.FSetDraft(val:single);
begin
   if Val<0 then Val:=0;
   if Edit3.Value<>val then Edit3.Value:=val;
   FBuildOffsets;
end;{TFreeAddMassOutputDialog.FSetDraft}

function TFreeAddMassOutputDialog.FGetAngle:single;
begin
   Result:=Edit9.Value;
end;{TFreeAddMassOutputDialog.FGetAngle}

procedure TFreeAddMassOutputDialog.FSetAngle(val:single);
begin
   Edit9.Value:=val;
end;{TFreeAddMassOutputDialog.FSetAngle}

function TFreeAddMassOutputDialog.FGetLength:single;
begin
   Result:=edit4.Value;
end;{TFreeAddMassOutputDialog.FGetLength}

procedure TFreeAddMassOutputDialog.FSetLength(val:single);
begin
   if val<0 then Val:=0;
   Edit4.Value:=val;
end;{TFreeAddMassOutputDialog.FSetLength}

function TFreeAddMassOutputDialog.FGetWaterViscosity:single;
begin
   Result:=Edit7.Value;
end;{TFreeAddMassOutputDialog.FGetWaterViscosity}

procedure TFreeAddMassOutputDialog.FSetWaterViscosity(val:single);
begin
   Edit7.Value:=Val;
end;{TFreeAddMassOutputDialog.FSetWaterViscosity}

function TFreeAddMassOutputDialog.FGetWaterDensity:single;
begin
   Result:=Edit6.Value;
end;{TFreeAddMassOutputDialog.FGetWaterDensity}

procedure TFreeAddMassOutputDialog.FSetWaterDensity(val:single);
begin
   Edit6.Value:=Val;
   WaterViscosity:=FindWaterViscosity(WaterDensity,fuMetric);
end;{TFreeAddMassOutputDialog.FSetWaterDensity}

function TFreeAddMassOutputDialog.FGetVolume:single;
begin
   Result:=Edit5.Value;
end;{TFreeAddMassOutputDialog.FGetVolume}

procedure TFreeAddMassOutputDialog.FSetVolume(val:single);
begin
   Edit5.Value:=val;
end;{TFreeAddMassOutputDialog.FSetVolume}

function TFreeAddMassOutputDialog.FGetWaterDepth:single;
begin
   Result:=Edit8.Value;
end;{TFreeAddMassOutputDialog.FGetWaterDepth}

procedure TFreeAddMassOutputDialog.FSetWaterDepth(val:single);
begin
   Edit8.Value:=val;
end;{TFreeAddMassOutputDialog.FSetWaterDepth}

function TFreeAddMassOutputDialog.FGetNumberOfStations:Integer;
begin
   Result:=Edit1.Value;
end;{TFreeAddMassOutputDialog.FGetNumberOfStations}

procedure TFreeAddMassOutputDialog.FSetNumberOfStations(val:Integer);
begin
   if not odd(val) then inc(val);
   if Edit1.Value<>Val then Edit1.Value:=Val;
   FBuildOffsets;
end;{TFreeAddMassOutputDialog.FSetNumberOfStations}


function TFreeAddMassOutputDialog.FGetNumberOfWaterlines:Integer;
begin
   Result:=Edit2.Value;
end;{TFreeAddMassOutputDialog.FGetNumberOfWaterlines}

procedure TFreeAddMassOutputDialog.FSetNumberOfWaterlines(val:Integer);
begin
   if not odd(val) then inc(val);
   if Edit2.Value<>Val then Edit2.Value:=Val;
   FBuildOffsets;
end;{TFreeAddMassOutputDialog.FSetNumberOfWaterlines}

function TFreeAddMassOutputDialog.FGetDat1:single;
begin
   Result:=Edit20.Value;
end;{TFreeAddMassOutputDialog.FGetDat1}

procedure TFreeAddMassOutputDialog.FSetDat1(val:single);
begin
   Edit20.Value:=val;
end;{TFreeAddMassOutputDialog.FSetDat1}

function TFreeAddMassOutputDialog.FGetDat2:single;
begin
   Result:=Edit21.Value;
end;{TFreeAddMassOutputDialog.FGetDat2}

procedure TFreeAddMassOutputDialog.FSetDat2(val:single);
begin
   Edit21.Value:=val;
end;{TFreeAddMassOutputDialog.FSetDat2}

function TFreeAddMassOutputDialog.FGetDat3:single;
begin
   Result:=Edit22.Value;
end;{TFreeAddMassOutputDialog.FGetDat3}

procedure TFreeAddMassOutputDialog.FSetDat3(val:single);
begin
   Edit22.Value:=val;
end;{TFreeAddMassOutputDialog.FSetDat3}

function TFreeAddMassOutputDialog.FGetDat4:single;
begin
   Result:=Edit23.Value;
end;{TFreeAddMassOutputDialog.FGetDat4}

procedure TFreeAddMassOutputDialog.FSetDat4(val:single);
begin
   Edit23.Value:=val;
end;{TFreeAddMassOutputDialog.FSetDat4}

function TFreeAddMassOutputDialog.FGetAngleBeta:single;
begin
   Result:=Edit24.Value;
end;{TFreeAddMassOutputDialog.FGetAngleBeta}

procedure TFreeAddMassOutputDialog.FSetAngleBeta(val:single);
begin
   Edit24.Value:=val;
end;{TFreeAddMassOutputDialog.FSetAngleBeta}

function TFreeAddMassOutputDialog.FGetVs:single;
begin
   Result:=Edit25.Value;
end;{TFreeAddMassOutputDialog.FGetVs}

procedure TFreeAddMassOutputDialog.FSetVs(val:single);
begin
   Edit25.Value:=val;
end;{TFreeAddMassOutputDialog.FSetVs}

function TFreeAddMassOutputDialog.FGetOm:single;
begin
   Result:=Edit26.Value;
end;{TFreeAddMassOutputDialog.FGetOm}

procedure TFreeAddMassOutputDialog.FSetOm(val:single);
begin
   Edit26.Value:=val;
end;{TFreeAddMassOutputDialog.FSetOm}

procedure TFreeAddMassOutputDialog.createViewport();
begin
  if assigned(Viewport) then exit;
  Viewport := TFreeViewport.Create(Self);
  with Viewport do
  begin
    Parent := Panel6;
    Left := 1;
    Height := 316;
    Top := 1;
    Width := 671;
    Angle := 20;
    Align := alClient;
    BackgroundImage.Alpha := 255;
    BackgroundImage.Owner := Viewport;
    BackgroundImage.Quality := 100;
    BackgroundImage.Scale := 1;
    BackgroundImage.ShowInView := fvBodyplan;
    BackgroundImage.Tolerance := 5;
    BackgroundImage.Transparent := False;
    BackgroundImage.TransparentColor := clBlack;
    BackgroundImage.Visible := True;
    CameraType := ftStandard;
    Color := clBlack;
    DoubleBuffer := True;
    Elevation := 20;
    HorScrollbar := ScrollBar1;
    Margin := 0;
    VertScrollbar := ScrollBar2;
    ViewType := fvPerspective;
    ViewportMode := vmWireFrame;
    OnRedraw := ViewportRedraw;
    OnRequestExtents := ViewportRequestExtents;
  end;
end;

function TFreeAddMassOutputDialog.Execute(Freeship:TFreeShip):Boolean;
var Units : TFreeUnitType;
begin
    createViewport();

   if NType=0 then begin
    Edit24.Enabled:=False;
    Edit25.Enabled:=False;
    Edit26.Enabled:=False;
   end; 
   ScrollBar1.Position:=Round(Viewport.Angle);
   ScrollBar2.Position:=Round(Viewport.Elevation);
   _Label18.Caption:=LengthStr(Freeship.ProjectSettings.ProjectUnits);
   _Label19.Caption:=LengthStr(Freeship.ProjectSettings.ProjectUnits);
   _Label20.Caption:=VolStr(Freeship.ProjectSettings.ProjectUnits);
   _Label15.Caption:=LengthStr(Freeship.ProjectSettings.ProjectUnits);
   _Label8.Caption:='*10^(-6)  '+ViscStr(Freeship.ProjectSettings.ProjectUnits);
   _Label9.Caption:=DensStr(Freeship.ProjectSettings.ProjectUnits);
   _Label11.Caption:=LengthStr(Freeship.ProjectSettings.ProjectUnits);
   _Label13.Caption:=DegrStr(Freeship.ProjectSettings.ProjectUnits); // град
   _Label21.Caption:='%'; // %
   FFreeship:=Freeship;
   Units:=FFreeship.ProjectSettings.ProjectUnits;
      if Units=fuImperial then begin
//        MessageDlg(Userstring(754),mtInformation,[mbOk],0);
        MessageDlg('Imperial system units in this program is NOT support !!!',mtInformation,[mbOk],0);
//        exit;
      end;	     
   Viewport.Color:=FFreeship.Preferences.ViewportColor;
   FBuildOffsets;
   Freeship.Preferences.LoadImageIntoBitmap(OKbutton.Glyph,'Ok');
   Freeship.Preferences.LoadImageIntoBitmap(CancelButton.Glyph,'Cancel');
   ShowTranslatedValues(Self);
   ShowModal;
   Result:=ModalResult=mrOK;
end;{TFreeAddMassOutputDialog.Execute}

procedure TFreeAddMassOutputDialog.OKbuttonClick(Sender: TObject);
begin
   ModalResult:=mrOK;
end;{TFreeAddMassOutputDialog.OKbuttonClick}

procedure TFreeAddMassOutputDialog.CancelButtonClick(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeAddMassOutputDialog.CancelButtonClick}

procedure TFreeAddMassOutputDialog.ViewportRequestExtents(Sender: TObject;var Min, Max: T3DCoordinate);
var I,J:Integer;
begin
   if FFreeship=nil then exit;
   Min.X:=0.0;
   Min.Y:=0.0;
   Min.Z:=0.0;
   Max.X:=FFreeship.ProjectSettings.ProjectLength;
   Max.Y:=0.0;
   Max.Z:=Draft;
   for I:=1 to NumberOfStations do
   begin
      for J:=1 to NumberOfWaterlines do
      begin
         if Offsets[I-1][J-1]>Max.Y then Max.Y:=Offsets[I-1][J-1];
      end;
   end;
   if Multihull then Max.Y:=Max.Y+Distance.Value;
   Min.Y:=-Max.Y;
end;{TFreeAddMassOutputDialog.ViewportRequestExtents}

procedure TFreeAddMassOutputDialog.ViewportRedraw(Sender: TObject);
var XSpacing   : TFloatType;
    ZSpacing   : TFloatType;
    I,J        : Integer;
    P1,P2      : T3DCoordinate;
    Pt         : TPoint;
    Dist,Tmp   : extended;
begin
   if FFreeship=nil then exit;
   P1.X:=0; P1.Y:=0; P1.Z:=0;
   XSpacing:=FFreeship.ProjectSettings.ProjectLength/(NumberOfStations-1);
   ZSpacing:=Draft/(NumberOfWaterlines-1);
   Viewport.PenColor:=FFreeship.Preferences.StationColor;
   Dist:=0.5*Distance.Value;
   for I:=1 to NumberOfStations do
   begin
      Tmp:=Dist+1e-4;
      for J:=1 to NumberOfWaterlines do
      begin
         P2.X:=(I-1)*XSpacing;
         P2.Y:=Dist+Offsets[NumberOfStations-I][J-1];
         P2.Z:=(J-1)*ZSpacing;
         Pt:=Viewport.Project(P2);
         if J=1 then Viewport.MoveTo(Pt.X,Pt.Y) else
         begin
            if (P1.Y>Tmp) or (P2.Y>Tmp) then Viewport.LineTo(Pt.X,Pt.Y)
                                        else Viewport.MoveTo(Pt.X,Pt.Y);
         end;
         P1:=P2;
      end;
      Tmp:=Dist-1e-4;
      for J:=1 to NumberOfWaterlines do
      begin
         P2.X:=(I-1)*XSpacing;
         P2.Y:=Dist-Offsets[NumberOfStations-I][J-1];
         P2.Z:=(J-1)*ZSpacing;
         Pt:=Viewport.Project(P2);
         if J=1 then Viewport.MoveTo(Pt.X,Pt.Y) else
         begin
            if (P1.Y<Tmp) or (P2.Y<Tmp) then Viewport.LineTo(Pt.X,Pt.Y)
                                        else Viewport.MoveTo(Pt.X,Pt.Y);
         end;
         P1:=P2;
      end;
      if multihull then // draw second hull
      begin
         Tmp:=-Dist+1e-4;
         for J:=1 to NumberOfWaterlines do
         begin
            P2.X:=(I-1)*XSpacing;
            P2.Y:=-Dist+Offsets[NumberOfStations-I][J-1];
            P2.Z:=(J-1)*ZSpacing;
            Pt:=Viewport.Project(P2);
            if J=1 then Viewport.MoveTo(Pt.X,Pt.Y) else
            begin
               if (P1.Y>Tmp) or (P2.Y>Tmp) then Viewport.LineTo(Pt.X,Pt.Y)
                                           else Viewport.MoveTo(Pt.X,Pt.Y);
            end;
            P1:=P2;
         end;
         Tmp:=-Dist-1e-4;
         for J:=1 to NumberOfWaterlines do
         begin
            P2.X:=(I-1)*XSpacing;
            P2.Y:=-Dist-Offsets[NumberOfStations-I][J-1];
            P2.Z:=(J-1)*ZSpacing;
            Pt:=Viewport.Project(P2);
            if J=1 then Viewport.MoveTo(Pt.X,Pt.Y) else
            begin
               if (P1.Y<tmp) or (P2.Y<tmp) then Viewport.LineTo(Pt.X,Pt.Y)
                                           else Viewport.MoveTo(Pt.X,Pt.Y);
            end;
            P1:=P2;
         end;
      end;
   end;

   Viewport.PenColor:=FFreeship.Preferences.WaterlineColor;
   for I:=1 to NumberOfWaterlines do
   begin
      Tmp:=Dist+1e-4;
      for J:=1 to NumberOfStations do
      begin
         P2.Z:=(I-1)*ZSpacing;
         P2.Y:=Dist+Offsets[NumberOfStations-J][I-1];
         P2.X:=(J-1)*XSpacing;
         Pt:=Viewport.Project(P2);
         if J=1 then Viewport.MoveTo(Pt.X,Pt.Y) else
         begin
            if (P1.Y>Tmp) or (P2.Y>Tmp) then Viewport.LineTo(Pt.X,Pt.Y)
                                        else Viewport.MoveTo(Pt.X,Pt.Y);
         end;
         P1:=P2;
      end;
      Tmp:=Dist-1e-4;
      for J:=1 to NumberOfStations do
      begin
         P2.Z:=(I-1)*ZSpacing;
         P2.Y:=Dist-Offsets[NumberOfStations-J][I-1];
         P2.X:=(J-1)*XSpacing;
         Pt:=Viewport.Project(P2);
         if J=1 then Viewport.MoveTo(Pt.X,Pt.Y) else
         begin
            if (P1.Y<Tmp) or (P2.Y<Tmp) then Viewport.LineTo(Pt.X,Pt.Y)
                                        else Viewport.MoveTo(Pt.X,Pt.Y);
         end;
         P1:=P2;
      end;
      if multihull then // draw second hull
      begin
         Tmp:=-Dist+1e-4;
         for J:=1 to NumberOfStations do
         begin
            P2.Z:=(I-1)*ZSpacing;
            P2.Y:=-Dist+Offsets[NumberOfStations-J][I-1];
            P2.X:=(J-1)*XSpacing;
            Pt:=Viewport.Project(P2);
            if J=1 then Viewport.MoveTo(Pt.X,Pt.Y) else
            begin
               if (P1.Y>Tmp) or (P2.Y>Tmp) then Viewport.LineTo(Pt.X,Pt.Y)
                                           else Viewport.MoveTo(Pt.X,Pt.Y);
            end;
            P1:=P2;
         end;
         Tmp:=-Dist-1e-4;
         for J:=1 to NumberOfStations do
         begin
            P2.Z:=(I-1)*ZSpacing;
            P2.Y:=-Dist-Offsets[NumberOfStations-J][I-1];
            P2.X:=(J-1)*XSpacing;
            Pt:=Viewport.Project(P2);
            if J=1 then Viewport.MoveTo(Pt.X,Pt.Y) else
            begin
               if (P1.Y<Tmp) or (P2.Y<Tmp) then Viewport.LineTo(Pt.X,Pt.Y)
                                           else Viewport.MoveTo(Pt.X,Pt.Y);
            end;
            P1:=P2;
         end;
      end;
   end;
   Viewport.PenColor:=FFreeship.Preferences.ButtockColor;
end;{TFreeAddMassOutputDialog.ViewportRedraw}

procedure TFreeAddMassOutputDialog.SaveToFile(Filename:string);
var HydObject: TFreeHydrostaticCalc;
    Strings : TStringList;
    I,J     : Integer;
//    N   : Integer;
    Str     : AnsiString;
    Scale   : TFloatType;
    B       : Single;
begin
    HydObject:=TFreeHydrostaticCalc.Create(FFreeship);
    HydObject.Draft:=Draft;
    HydObject.Calculate;
    Length:=HydObject.Data.LengthWaterline; 
    B:=HydObject.Data.BeamWaterline; 
    HydObject.Destroy;
   if FFreeship=nil then exit;
   Strings:=TStringList.Create;
   if FFreeship.ProjectSettings.ProjectUnits=fuMetric then Scale:=1.0
                                                      else Scale:=1/Foot;   
//   Strings.Add(FFreeship.ProjectSettings.ProjectName+' L='+FloatToStrF(Scale*FFreeship.ProjectSettings.ProjectLength,ffFixed,7,3)+' B='+FloatToStrF(Scale*FFreeship.ProjectSettings.ProjectBeam,ffFixed,7,3)+' T='+FloatToStrF(Scale*FFreeship.ProjectSettings.ProjectDraft,ffFixed,7,3));
   Strings.Add(FFreeship.ProjectSettings.ProjectName+' L='+FloatToStrF(Scale*Length,ffFixed,7,3)+' B='+FloatToStrF(Scale*B,ffFixed,7,3)+' T='+FloatToStrF(Scale*Draft,ffFixed,7,3));
   if FFreeship.ProjectSettings.ProjectUnits=fuMetric then Strings.Add(FloatToStrF(WaterDensity*1000,ffFixed,7,1)+' '+Truncate(Dat1,3)+' '+Truncate(Dat2,3)+' '+Truncate(Dat3,3)+' '+Truncate(Dat4,3)+' '+Truncate(Scale*WaterDepth,3)+' '+Truncate(AngleBeta,3)+' '+Truncate(Vs,3)+' '+Truncate(Om,3)+' '+Truncate(Ntype+1,0)) // specific gravity of water
                                                      else Strings.Add(FloatToStrF(WaterDensity/WeightConversionFactor*1000,ffFixed,7,1)+' '+Truncate(Dat1,3)+' '+Truncate(Dat2,3)+' '+Truncate(Dat3,3)+' '+Truncate(Dat4,3)+' '+Truncate(Scale*WaterDepth,3)+' '+Truncate(AngleBeta,3)+' '+Truncate(Vs,3)+' '+Truncate(Om,3)+' '+Truncate(Ntype+1,0));
   Strings.Add('0 0 0 0 0 0 0 0 0 0');													  
   Strings.Add(FloatToStrF(Length,ffFixed,7,4)+' '+FloatToStrF(Draft,ffFixed,7,4));
   Strings.Add(IntToStr(NumberOfStations)+' 1. 0.');
   Strings.Add(IntToStr(NumberOfWaterlines));
//   N:=1;
      for I:=1 to NumberOfStations do
      begin
         Str:='';
         for J:=1 to NumberOfWaterlines do
         begin
            Str:=Str+FloatToStrF(Offsets[I-1][J-1],ffFixed,7,3);
            if J<NumberOfWaterlines then Str:=Str+' ';
         end;
         Strings.Add(Str);
      end;
   Strings.Add('/*');   
   Strings.SaveToFile(Filename);
   Strings.Destroy;
end;{TFreeAddMassOutputDialog.SaveToFile}

function TFreeAddMassOutputDialog.FGetMultihull:boolean;
begin
   result:=(Radiobutton2.checked) or (Radiobutton3.checked);
end;{TFreeAddMassOutputDialog.FGetMultihull}

procedure TFreeAddMassOutputDialog.RadioButton1Click(Sender: TObject);
begin
   Distance.Enabled:=Multihull;
   if not Multihull then Distance.Value:=0.0;
   FBuildOffsets;
end;{TFreeAddMassOutputDialog.RadioButton1Click}

procedure TFreeAddMassOutputDialog.DistanceAfterSetValue(Sender: TObject);
begin
   FBuildOffsets;
   viewport.Zoomextents;
end;{TFreeAddMassOutputDialog.DistanceAfterSetValue}

procedure TFreeAddMassOutputDialog.Edit1AfterSetValue(Sender: TObject);
begin
   NumberOfStations:=Edit1.Value;
end;{TFreeAddMassOutputDialog.Edit1AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit2AfterSetValue(Sender: TObject);
begin
   NumberOfWaterLines:=Edit2.Value;
end;{TFreeAddMassOutputDialog.Edit2AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit3AfterSetValue(Sender: TObject);
begin
   Draft:=Edit3.Value;
end;{TFreeAddMassOutputDialog.Edit3AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit7AfterSetValue(Sender: TObject);
begin
   WaterViscosity:=Edit7.value;
end;{TFreeAddMassOutputDialog.Edit7AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit9AfterSetValue(Sender: TObject);
begin
   Angle:=Edit9.value;
end;{TFreeAddMassOutputDialog.Edit9AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit4AfterSetValue(Sender: TObject);
begin
   Length:=Edit4.Value;
end;{TFreeAddMassOutputDialog.Edit4AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit5AfterSetValue(Sender: TObject);
begin
   Volume:=Edit5.Value;
end;{TFreeAddMassOutputDialog.Edit5AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit6AfterSetValue(Sender: TObject);
begin
   WaterDensity:=Edit6.Value;
end;{TFreeAddMassOutputDialog.Edit6AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit8AfterSetValue(Sender: TObject);
begin
   WaterDepth:=Edit8.Value;
end;{TFreeAddMassOutputDialog.Edit8AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit20AfterSetValue(Sender: TObject);
begin
   Dat1:=Edit20.Value;
end;{TFreeAddMassOutputDialog.Edit20AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit21AfterSetValue(Sender: TObject);
begin
   Dat2:=Edit21.Value;
end;{TFreeAddMassOutputDialog.Edit21AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit22AfterSetValue(Sender: TObject);
begin
   Dat3:=Edit22.Value;
end;{TFreeAddMassOutputDialog.Edit22AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit23AfterSetValue(Sender: TObject);
begin
   Dat4:=Edit23.Value;
end;{TFreeAddMassOutputDialog.Edit23AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit24AfterSetValue(Sender: TObject);
begin
   AngleBeta:=Edit24.Value;
end;{TFreeAddMassOutputDialog.Edit24AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit25AfterSetValue(Sender: TObject);
begin
   Vs:=Edit25.Value;
end;{TFreeAddMassOutputDialog.Edit25AfterSetValue}

procedure TFreeAddMassOutputDialog.Edit26AfterSetValue(Sender: TObject);
begin
   Om:=Edit26.Value;
end;{TFreeAddMassOutputDialog.Edit26AfterSetValue}

procedure TFreeAddMassOutputDialog.FormResize(Sender: TObject);
begin
   OKButton.Left:=Panel1.Clientwidth-CancelButton.Width-OkButton.Width-5;
   CancelButton.Left:=Panel1.Clientwidth-CancelButton.Width-5;
end;{TFreeAddMassOutputDialog.FormResize}

procedure TFreeAddMassOutputDialog.ComboBoxClick(Sender: TObject);
begin
   if Combobox.ItemIndex=0 then begin 
                     NType:=0; 
                     Edit24.Enabled:=False;
                     Edit25.Enabled:=False;
                     Edit26.Enabled:=False;                  
                     end;
   if Combobox.ItemIndex=1 then begin 
                     NType:=1;
                     Edit24.Enabled:=False;
                     Edit25.Enabled:=False;
                     Edit26.Enabled:=False;
                     end;
   if Combobox.ItemIndex=2 then begin 
                     NType:=2;
                     Edit24.Enabled:=False;
                     Edit25.Enabled:=False;
                     Edit26.Enabled:=False;
                     end;
   if Combobox.ItemIndex=3 then begin 
                     NType:=3;
                     Edit24.Enabled:=True;
                     Edit25.Enabled:=True;
                     Edit26.Enabled:=True;
                     end;
//      Calculate;
end;

end.
