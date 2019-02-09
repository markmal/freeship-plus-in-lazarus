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
unit FreeMichletOutputDlg;

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
     Spin;

type TFreeMichletOutputDialog  = class(TForm)
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
                                    Panel5: TPanel;
                                    Label21: TLabel;
                                    Label22: TLabel;
                                    Label23: TLabel;
                                    Label24: TLabel;
    _Label25: TLabel;
    _Label26: TLabel;
                                    Label27: TLabel;
                                    Edit12: TFloatSpinEdit;
                                    Edit13: TFloatSpinEdit;
                                    Edit14: TSpinEdit;
                                    Group1: TRadioGroup;
                                    TabSheet4: TTabSheet;
                                    Panel8: TPanel;
                                    GroupBox1: TGroupBox;
    _Label28: TLabel;
    _Label29: TLabel;
    _Label30: TLabel;
    _Label31: TLabel;
    _Label32: TLabel;
                                    Image1: TImage;
    _Label33: TLabel;
    _Label34: TLabel;
    _Label35: TLabel;
                                    Edit15: TFloatSpinEdit;
                                    Edit16: TFloatSpinEdit;
                                    Edit17: TFloatSpinEdit;
                                    Edit18: TSpinEdit;
                                    Edit19: TSpinEdit;
                                    GroupBox2: TGroupBox;
    _Label36: TLabel;
    _Label37: TLabel;
    _Label38: TLabel;
    _Label39: TLabel;
    _Label40: TLabel;
                                    Image2: TImage;
    _Label41: TLabel;
    _Label42: TLabel;
    _Label44: TLabel;
                                    Edit20: TFloatSpinEdit;
                                    Edit21: TFloatSpinEdit;
                                    Edit22: TFloatSpinEdit;
                                    Edit23: TFloatSpinEdit;
                                    Edit24: TSpinEdit;
                                    Edit25: TSpinEdit;
                                    GroupBox3: TGroupBox;
                                    RadioButton1: TRadioButton;
                                    RadioButton2: TRadioButton;
                                    RadioButton3: TRadioButton;
                                    Label14: TLabel;
                                    Distance: TFloatSpinEdit;
    _Label15: TLabel;
    _Label16: TLabel;
    _Label17: TLabel;
    Panel6: TPanel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Viewport: TFreeViewport;
                                    procedure OKbuttonClick(Sender: TObject);
                                    procedure CancelButtonClick(Sender: TObject);
                                    procedure ViewportRequestExtents(Sender: TObject; var Min,Max: T3DCoordinate);
                                    procedure ViewportRedraw(Sender: TObject);
                                    procedure RadioButton1Click(Sender: TObject);
                                    procedure DistanceAfterSetValue(Sender: TObject);
                                    procedure Edit1AfterSetValue(Sender: TObject);
                                    procedure Edit2AfterSetValue(Sender: TObject);
                                    procedure Edit3AfterSetValue(Sender: TObject);
                                    procedure Edit7AfterSetValue(Sender: TObject);
                                    procedure Edit4AfterSetValue(Sender: TObject);
                                    procedure Edit5AfterSetValue(Sender: TObject);
                                    procedure Edit6AfterSetValue(Sender: TObject);
                                    procedure Edit9AfterSetValue(Sender: TObject);
                                    procedure Edit12AfterSetValue(Sender: TObject);
                                    procedure Edit13AfterSetValue(Sender: TObject);
                                    procedure Edit14AfterSetValue(Sender: TObject);
                                    procedure Edit15AfterSetValue(Sender: TObject);
                                    procedure Edit16AfterSetValue(Sender: TObject);
                                    procedure Edit17AfterSetValue(Sender: TObject);
                                    procedure Edit18AfterSetValue(Sender: TObject);
                                    procedure Edit19AfterSetValue(Sender: TObject);
                                    procedure Edit20AfterSetValue(Sender: TObject);
                                    procedure Edit21AfterSetValue(Sender: TObject);
                                    procedure Edit22AfterSetValue(Sender: TObject);
                                    procedure Edit23AfterSetValue(Sender: TObject);
                                    procedure Edit24AfterSetValue(Sender: TObject);
                                    procedure Edit25AfterSetValue(Sender: TObject);
    procedure FormResize(Sender: TObject);
                                 private { Private declarations }
                                    FFreeship     : TFreeShip;
                                    NumberOfHulls : Integer;
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
                                    function FGetG:single;
                                    procedure FSetG(val:single);
                                    function FGetWaterDensity:single;
                                    procedure FSetWaterDensity(val:single);
                                    function FGetWaterViscosity:single;
                                    procedure FSetWaterViscosity(val:single);
                                    function FGetWaterDepth:single;
                                    procedure FSetWaterDepth(val:single);
                                    function FGetStartSpeed:single;
                                    procedure FSetStartSpeed(val:single);
                                    function FGetEndSpeed:single;
                                    procedure FSetEndSpeed(val:single);
                                    function FGetNumberOfSpeeds:Integer;
                                    procedure FSetNumberOfSpeeds(val:Integer);
                                    function FGetR0:single;
                                    procedure FSetR0(val:single);
                                    function FGetR1:single;
                                    procedure FSetR1(val:single);
                                    function FGetBeta:single;
                                    procedure FSetBeta(val:single);
                                    function FGetNr:Integer;
                                    procedure FSetNr(val:Integer);
                                    function FGetNBeta:Integer;
                                    procedure FSetNBeta(val:Integer);
                                    function FGetX0:single;
                                    procedure FSetX0(val:single);
                                    function FGetX1:single;
                                    procedure FSetX1(val:single);
                                    function FGetY0:single;
                                    procedure FSetY0(val:single);
                                    function FGetY1:single;
                                    procedure FSetY1(val:single);
                                    function FGetNx:Integer;
                                    procedure FSetNx(val:Integer);
                                    function FGetNy:Integer;
                                    procedure FSetNy(val:Integer);
                                    function FGetMultihull:boolean;
                                    procedure createViewport();
                                 public { Public declarations }
                                    function Execute(Freeship:TFreeShip):Boolean;
                                    procedure SaveToFile(Filename:string);
                                    property Beta                 : single read FGetBeta write FSetBeta;
                                    property Draft                : single read FGetDraft write FSetDraft;
                                    property G                    : single read FGetG write FSetG;
                                    property Length               : single read FGetLength write FSetLength;
                                    property Multihull            : Boolean read FGetMultihull;
                                    property NBeta                : integer read FGetNBeta write FSetNBeta;
                                    property Nr                   : integer read FGetNr write FSetNr;
                                    property Nx                   : integer read FGetNx write FSetNx;
                                    property Ny                   : integer read FGetNy write FSetNy;
                                    property NumberOfSpeeds       : Integer read FGetNumberOfSpeeds write FSetNumberOfSpeeds;
                                    property NumberOfStations     : Integer read FGetNumberOfStations write FSetNumberOfStations;
                                    property NumberOfWaterlines   : Integer read FGetNumberOfWaterlines write FSetNumberOfWaterlines;
                                    property R0                   : single read FGetR0 write FSetR0;
                                    property R1                   : single read FGetR1 write FSetR1;
                                    property StartSpeed           : single read FGetStartSpeed write FSetStartSpeed;
                                    property EndSpeed             : single read FGetEndSpeed write FSetEndSpeed;
                                    property Volume               : single read FGetVolume write FSetVolume;
                                    property WaterDensity         : single read FGetWaterDensity write FSetWaterDensity;
                                    property WaterDepth           : single read FGetWaterDepth write FSetWaterDepth;
                                    property WaterViscosity       : single read FGetWaterViscosity write FSetWaterViscosity;
                                    property X0                   : single read FGetX0 write FSetX0;
                                    property X1                   : single read FGetX1 write FSetX1;
                                    property Y0                   : single read FGetY0 write FSetY0;
                                    property Y1                   : single read FGetY1 write FSetY1;
                                 end;
var FreeMichletOutputDialog: TFreeMichletOutputDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFreeMichletOutputDialog.FBuildOffsets;
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

end;{TFreeMichletOutputDialog.FBuildOffsets}

function TFreeMichletOutputDialog.FGetDraft:single;
begin
   Result:=Edit3.value;
end;{TFreeMichletOutputDialog.FGetDraft}

procedure TFreeMichletOutputDialog.FSetDraft(val:single);
begin
   if Val<0 then Val:=0;
   if Edit3.Value<>val then Edit3.Value:=val;
   FBuildOffsets;
end;{TFreeMichletOutputDialog.FSetDraft}

function TFreeMichletOutputDialog.FGetG:single;
begin
   Result:=Edit6.Value;
end;{TFreeMichletOutputDialog.FGetG}

procedure TFreeMichletOutputDialog.FSetG(val:single);
begin
   Edit6.Value:=val;
end;{TFreeMichletOutputDialog.FSetG}

function TFreeMichletOutputDialog.FGetR0:single;
begin
   Result:=Edit15.Value;
end;{TFreeMichletOutputDialog.FGetR0}

procedure TFreeMichletOutputDialog.FSetR0(val:single);
begin
   if Val<1.5*Length then Val:=1.5*Length;
   Edit15.Value:=val;
end;{TFreeMichletOutputDialog.FSetR0}

function TFreeMichletOutputDialog.FGetX0:single;
begin
   Result:=Edit20.Value;
end;{TFreeMichletOutputDialog.FGetX0}

procedure TFreeMichletOutputDialog.FSetX0(val:single);
begin
   if Val<1.5*Length then Val:=1.5*Length;
   Edit20.Value:=val;
end;{TFreeMichletOutputDialog.FSetX0}

function TFreeMichletOutputDialog.FGetX1:single;
begin
   Result:=Edit21.value;
end;{TFreeMichletOutputDialog.FGetX1}

procedure TFreeMichletOutputDialog.FSetX1(val:single);
begin
   if Val<2.5*Length then Val:=2.5*Length;
   Edit21.Value:=val;
end;{TFreeMichletOutputDialog.FSetX1}

function TFreeMichletOutputDialog.FGetY0:single;
begin
   Result:=Edit22.Value;
end;{TFreeMichletOutputDialog.FGetY0}

procedure TFreeMichletOutputDialog.FSetY0(val:single);
begin
   Edit22.Value:=Val;
end;{TFreeMichletOutputDialog.FSetY0}

function TFreeMichletOutputDialog.FGetY1:single;
begin
   Result:=Edit23.Value;
end;{TFreeMichletOutputDialog.FGetY1}

procedure TFreeMichletOutputDialog.FSetY1(val:single);
begin
   Edit23.Value:=val;
end;{TFreeMichletOutputDialog.FSetY1}

function TFreeMichletOutputDialog.FGetBeta:single;
begin
   Result:=Edit17.Value;
end;{TFreeMichletOutputDialog.FGetBeta}

procedure TFreeMichletOutputDialog.FSetBeta(val:single);
begin
   if Val<10 then Val:=10;
   if Val>90 then Val:=90;
   Edit17.Value:=Val;
end;{TFreeMichletOutputDialog.FSetBeta}

function TFreeMichletOutputDialog.FGetR1:single;
begin
   Result:=Edit16.Value;
end;{TFreeMichletOutputDialog.FGetR1}

procedure TFreeMichletOutputDialog.FSetR1(val:single);
begin
   if Val<2.5*Length then Val:=2.5*Length;
   Edit16.Value:=Val;
end;{TFreeMichletOutputDialog.FSetR1}

function TFreeMichletOutputDialog.FGetLength:single;
begin
   Result:=edit4.Value;
end;{TFreeMichletOutputDialog.FGetLength}

procedure TFreeMichletOutputDialog.FSetLength(val:single);
begin
   if val<0 then Val:=0;
   Edit4.Value:=val;
end;{TFreeMichletOutputDialog.FSetLength}

function TFreeMichletOutputDialog.FGetWaterViscosity:single;
begin
   Result:=Edit8.Value;
end;{TFreeMichletOutputDialog.FGetWaterViscosity}

procedure TFreeMichletOutputDialog.FSetWaterViscosity(val:single);
begin
   Edit8.Value:=Val;
end;{TFreeMichletOutputDialog.FSetWaterViscosity}

function TFreeMichletOutputDialog.FGetWaterDensity:single;
begin
   Result:=Edit7.Value;
end;{TFreeMichletOutputDialog.FGetWaterDensity}

procedure TFreeMichletOutputDialog.FSetWaterDensity(val:single);
begin
   Edit7.Value:=Val;
   WaterViscosity:=FindWaterViscosity(WaterDensity,fuMetric);
end;{TFreeMichletOutputDialog.FSetWaterDensity}

function TFreeMichletOutputDialog.FGetVolume:single;
begin
   Result:=Edit5.Value;
end;{TFreeMichletOutputDialog.FGetVolume}

procedure TFreeMichletOutputDialog.FSetVolume(val:single);
begin
   Edit5.Value:=val;
end;{TFreeMichletOutputDialog.FSetVolume}

function TFreeMichletOutputDialog.FGetStartSpeed:single;
begin
   Result:=Edit12.Value;
end;{TFreeMichletOutputDialog.FGetStartSpeed}

procedure TFreeMichletOutputDialog.FSetStartSpeed(val:single);
begin
   Edit12.Value:=Val;
   // Skip translation
   if g*length<>0 then _Label25.Caption:='Fn '+FloatToStrF((Val*1852/3600)/Sqrt(g*Length),ffFixed,7,3)
                  else _Label25.Caption:='';
   if StartSpeed>=EndSpeed-0.1 then EndSpeed:=StartSpeed+0.1;
   // End Skip translation
end;{TFreeMichletOutputDialog.FSetStartSpeed}

function TFreeMichletOutputDialog.FGetEndSpeed:single;
begin
   Result:=Edit13.Value;
end;{TFreeMichletOutputDialog.FGetEndSpeed}

procedure TFreeMichletOutputDialog.FSetEndSpeed(val:single);
begin
   if Val<StartSpeed+0.0194 then Val:=StartSpeed+0.0194;
   Edit13.Value:=val;
   // Skip translation
   if g*length<>0 then _Label26.Caption:='Fn '+FloatToStrF((Val*1852/3600)/Sqrt(g*Length),ffFixed,7,3)
                  else _Label26.Caption:='';
   // End Skip translation
end;{TFreeMichletOutputDialog.FSetEndSpeed}

function TFreeMichletOutputDialog.FGetWaterDepth:single;
begin
   Result:=Edit9.Value;
end;{TFreeMichletOutputDialog.FGetWaterDepth}

procedure TFreeMichletOutputDialog.FSetWaterDepth(val:single);
begin
   Edit9.Value:=val;
end;{TFreeMichletOutputDialog.FSetWaterDepth}

function TFreeMichletOutputDialog.FGetNumberOfStations:Integer;
begin
   Result:=Edit1.Value;
end;{TFreeMichletOutputDialog.FGetNumberOfStations}

procedure TFreeMichletOutputDialog.FSetNumberOfStations(val:Integer);
begin
   if not odd(val) then inc(val);
   if Edit1.Value<>Val then Edit1.Value:=Val;
   FBuildOffsets;
end;{TFreeMichletOutputDialog.FSetNumberOfStations}

function TFreeMichletOutputDialog.FGetNumberOfSpeeds:Integer;
begin
   Result:=Edit14.Value;
end;{TFreeMichletOutputDialog.FGetNumberOfSpeeds}

procedure TFreeMichletOutputDialog.FSetNumberOfSpeeds(val:Integer);
begin
   Edit14.Value:=Val;
end;{TFreeMichletOutputDialog.FSetNumberOfSpeeds}

function TFreeMichletOutputDialog.FGetNr:Integer;
begin
   Result:=Edit18.Value;
end;{TFreeMichletOutputDialog.FGetNr}

procedure TFreeMichletOutputDialog.FSetNr(val:Integer);
begin
   Edit18.Value:=Val;
end;{TFreeMichletOutputDialog.FSetNr}

function TFreeMichletOutputDialog.FGetNx:Integer;
begin
   Result:=Edit24.Value;
end;{TFreeMichletOutputDialog.FGetNx}

procedure TFreeMichletOutputDialog.FSetNx(val:Integer);
begin
   Edit24.Value:=val;
end;{TFreeMichletOutputDialog.FSetNx}

function TFreeMichletOutputDialog.FGetNy:Integer;
begin
   Result:=Edit25.Value;
end;{TFreeMichletOutputDialog.FGetNy}

procedure TFreeMichletOutputDialog.FSetNy(val:Integer);
begin
   Edit25.Value:=Val;
end;{TFreeMichletOutputDialog.FSetNy}

function TFreeMichletOutputDialog.FGetNBeta:Integer;
begin
   Result:=Edit19.Value;
end;{TFreeMichletOutputDialog.FGetNBeta}

procedure TFreeMichletOutputDialog.FSetNBeta(val:Integer);
begin
   Edit19.Value:=val;
end;{TFreeMichletOutputDialog.FSetNBeta}

function TFreeMichletOutputDialog.FGetNumberOfWaterlines:Integer;
begin
   Result:=Edit2.Value;
end;{TFreeMichletOutputDialog.FGetNumberOfWaterlines}

procedure TFreeMichletOutputDialog.FSetNumberOfWaterlines(val:Integer);
begin
   if not odd(val) then inc(val);
   if Edit2.Value<>Val then Edit2.Value:=Val;
   FBuildOffsets;
end;{TFreeMichletOutputDialog.FSetNumberOfWaterlines}

procedure TFreeMichletOutputDialog.createViewport();
begin
  if assigned(Viewport) then exit;
  Viewport := TFreeViewport.Create(Self);
  with Viewport do
  begin
    Parent := Panel6;
    Left := 1;
    Height := 316;
    Top := 1;
    Width := 451;
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


function TFreeMichletOutputDialog.Execute(Freeship:TFreeShip):Boolean;
begin
  createViewport();
   ScrollBar1.Position:=Round(Viewport.Angle);
   ScrollBar2.Position:=Round(Viewport.Elevation);
   _Label18.Caption:=LengthStr(Freeship.ProjectSettings.ProjectUnits);
   _Label19.Caption:=LengthStr(Freeship.ProjectSettings.ProjectUnits);
   _Label20.Caption:=VolStr(Freeship.ProjectSettings.ProjectUnits);
   _Label15.Caption:=LengthStr(Freeship.ProjectSettings.ProjectUnits);
   FFreeship:=Freeship;
   Viewport.Color:=FFreeship.Preferences.ViewportColor;
   FBuildOffsets;
   ShowModal;
   Result:=ModalResult=mrOK;
end;{TFreeMichletOutputDialog.Execute}

procedure TFreeMichletOutputDialog.OKbuttonClick(Sender: TObject);
begin
   ModalResult:=mrOK;
end;{TFreeMichletOutputDialog.OKbuttonClick}

procedure TFreeMichletOutputDialog.CancelButtonClick(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeMichletOutputDialog.CancelButtonClick}

procedure TFreeMichletOutputDialog.ViewportRequestExtents(Sender: TObject;var Min, Max: T3DCoordinate);
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
end;{TFreeMichletOutputDialog.ViewportRequestExtents}

procedure TFreeMichletOutputDialog.ViewportRedraw(Sender: TObject);
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
end;{TFreeMichletOutputDialog.ViewportRedraw}

procedure TFreeMichletOutputDialog.SaveToFile(Filename:string);
var HydObject: TFreeHydrostaticCalc;
    Strings : TStringList;
    I,J,N   : Integer;
    Str     : AnsiString;
begin
    HydObject:=TFreeHydrostaticCalc.Create(FFreeship);
    HydObject.Draft:=Draft;
    HydObject.Calculate;
   if FFreeship=nil then exit;
   Strings:=TStringList.Create;
   Strings.Add('# ======================================================================');
   Strings.Add('#');
   Strings.Add('# Project  : '+FFreeship.ProjectSettings.ProjectName);
   Strings.Add('# File     : '+FFreeship.Filename);
   Strings.Add('# Designer : '+FFreeship.ProjectSettings.ProjectDesigner);
   Strings.Add('#');
   Strings.Add('# =================== INPUT FILE TYPE AND SUBTYPE ======================');
   Strings.Add('# Input File Type (0=Standard)');
   Strings.Add('0');
   Strings.Add('# Input File Subtype (0=Standard)');
   Strings.Add('0');
   Strings.Add('# =================== OUTPUT FILE TYPE AND SUBTYPE =====================');
   Strings.Add('# Output File Type (0=Standard)');
   Strings.Add('0');
   Strings.Add('# Output File Subtype (0=Standard)');
   Strings.Add('0');
   Strings.Add('# ====================== COURSE AND VESSEL TYPE ========================');
   Strings.Add('# Course Particulars (0=None)');
   Strings.Add('0');
   Strings.Add('# Number of Hulls (1, 2,..., or 5)');
   if (Radiobutton2.checked) or (Radiobutton3.checked) then NumberOfHulls:=2 else NumberOfHulls:=1;
   Strings.Add(IntToStr(NumberOfHulls));
   Strings.Add('# ======================== PHYSICAL QUANTITIES =========================');
   Strings.Add('# Gravitational Acceleration (m/sec/sec) (min 9.6, max 9.9)');
   Strings.Add(FloatToStrF(g,ffFixed,7,6));
   Strings.Add('# ========================= WATER PROPERTIES ===========================');
   Strings.Add('# Water Density (kg/cubic metre) (min 995.0, max 1030.0)');
   Strings.Add(FloatToStrF(1000*WaterDensity,ffFixed,7,2));
   Strings.Add('# Water Kin. Viscosity (sq. m/sec * 10^-6) (min 0.8, max 1.31)');
   Strings.Add(FloatToStrF(WaterViscosity,ffFixed,7,5));
   Strings.Add('# Base Eddy Kin. Viscosity (non-dimensional, min 1.0)');
   Strings.Add('10.0');
   Strings.Add('# Water Depth (metres) (max=10000.0)');
   Strings.Add(Truncate(WaterDepth,3));
   Strings.Add('# ========================== AIR PROPERTIES ============================');
   Strings.Add('# Air Density (kg/cubic metre) (min 0.9, max 2.0)');
   Strings.Add('1.26');
   Strings.Add('# Air Kin. Viscosity (sq. m/sec * 10^-6) (min 10.0, max 20.0)');
   Strings.Add('14.4');
   Strings.Add('# Wind Speed (m/sec)');
   Strings.Add('0.0');
   Strings.Add('# Wind Direction (degrees)');
   Strings.Add('0.0');
   Strings.Add('# ======================= CALCULATION PARAMETERS =======================');
   Strings.Add('# Minimum Speed (m/sec) (min 0.01, max 39.9)');
   Strings.Add(FloatToStrF(StartSpeed*1852/3600,ffFixed,7,3));
   Strings.Add('# Maximum Speed (m/sec) (max 40.0)');
   Strings.Add(FloatToStrF(EndSpeed*1852/3600,ffFixed,7,3));
   Strings.Add('# Number of Speeds (min 2, max 50)');
   Strings.Add(IntToStr(NumberOfSpeeds));
   Strings.Add('# Leeway Parameters (0=None)');
   Strings.Add('0');
   Strings.Add('# Wave Drag Ntheta');
   Strings.Add('1024');
   Strings.Add('# Skin Friction Method (0=None, 1=ITTC1957, 2=Grigson)');
   Strings.Add(IntToStr(Group1.ItemIndex));
   Strings.Add('# Viscous Drag Form Factor');
   Strings.Add('1.0');
   Strings.Add('# Wave Drag Form Factor');
   Strings.Add('1.0');
   Strings.Add('# Pressure Signature Method (0=None,1=Slender body)');
   Strings.Add('1');
   Strings.Add('# ==================== SHIP CALCULATION PARAMETERS =====================');
   Strings.Add('# Number of Offset Stations (rows) (odd integer: min 5, max 81)');
   Strings.Add(IntToStr(NumberOfStations));
   Strings.Add('# Number of Offset Waterlines (columns) (odd integer: min 5, max 81)');
   Strings.Add(IntToStr(NumberOfWaterlines));
   Strings.Add('# Ship Loading Type');
   Strings.Add('3');
   Strings.Add('# Ship Loading Formula Parameters');
   Strings.Add('1.0,0.0,0.0');
   Strings.Add('# ===================== WAVE ELEVATION PARAMETERS ======================');
   Strings.Add('# Sectorial Cuts and Patches');
   Strings.Add('# R0');
   Strings.Add(FloatToStrF(R0,ffFixed,7,3));
   Strings.Add('# R1');
   Strings.Add(FloatToStrF(R1,ffFixed,7,3));
   Strings.Add('# Beta');
   Strings.Add(FloatToStrF(Beta,ffFixed,7,3));
   Strings.Add('# Nr');
   Strings.Add(IntToStr(Nr));
   Strings.Add('# Nbeta');
   Strings.Add(IntToStr(NBeta));
   Strings.Add('# Rectangular Cuts and Patches');
   Strings.Add('# x0');
   Strings.Add(FloatToStrF(X0,ffFixed,7,3));
   Strings.Add('# x1');
   Strings.Add(FloatToStrF(X1,ffFixed,7,3));
   Strings.Add('# y0');
   Strings.Add(FloatToStrF(Y0,ffFixed,7,3));
   Strings.Add('# y1');
   Strings.Add(FloatToStrF(Y1,ffFixed,7,3));
   Strings.Add('# Nwx');
   Strings.Add(IntToStr(Nx));
   Strings.Add('# Nwy');
   Strings.Add(IntToStr(Ny));
   Strings.Add('# Beaches and Walls');
   Strings.Add('# x0');
   Strings.Add('200.0');
   Strings.Add('# x1');
   Strings.Add('2200.0');
   Strings.Add('# y0');
   Strings.Add('500.0');
   Strings.Add('# z0');
   Strings.Add('-1.0');
   Strings.Add('# z1');
   Strings.Add('0.0');
   Strings.Add('# Slope');
   Strings.Add('90.0');
   Strings.Add('# Nbx');
   Strings.Add('11');
   Strings.Add('# Nbz');
   Strings.Add('11');
   for N:=1 to NumberOfHulls do
   begin
      if N=1 then Strings.Add('# ============================ FIRST HULL ==============================')
             else Strings.Add('# ============================ SECOND HULL =============================');
      Strings.Add('# Offsets');
      for I:=1 to NumberOfStations do
      begin
         Str:='';
         for J:=1 to NumberOfWaterlines do
         begin
            Str:=Str+FloatToStrF(Offsets[I-1][J-1],ffFixed,7,4);
            if J<NumberOfWaterlines then Str:=Str+',';
         end;
         Strings.Add(Str);
      end;
      Strings.Add('# Displacement Volume (cubic metres)');
      if Radiobutton3.Checked then Strings.Add(FloatToStrF(0.5*Volume,ffFixed,7,4))
                              else Strings.Add(FloatToStrF(Volume,ffFixed,7,4));
      Strings.Add('# Length (metres)');
      Length:=HydObject.Data.LengthWaterline; 
      Strings.Add(FloatToStrF(Length,ffFixed,7,4));
      Strings.Add('# Draft (metres)');
      Strings.Add(FloatToStrF(Draft,ffFixed,7,4));
      Strings.Add('# Longitudinal Separation (metres) (0.0 for a monohull)');
      Strings.Add('0.0');
      Strings.Add('# Lateral Separation Distance (metres) (0.0 for a monohull)');
      if N=1 then Strings.Add(FloatToStrF(Distance.Value,ffFixed,7,4));

      Strings.Add('# Loading Type for this hull');
      Strings.Add('3');
      Strings.Add('# Loading Formula Parameters');
      Strings.Add('1.0,0.0,0.0');
      Strings.Add('# Trim Method');
      Strings.Add('0');
      Strings.Add('# Trim: Number of speeds ( >= 2)');
      Strings.Add('2');
      Strings.Add('# Trim: speed, angle');
      Strings.Add('0.0,0.0');
      Strings.Add('10.0,0.0');
      Strings.Add('# Sinkage Method');
      Strings.Add('0');
      Strings.Add('# Sinkage: Number of speeds ( >= 2)');
      Strings.Add('2');
      Strings.Add('# Sinkage: speed, amount');
      Strings.Add('0.0,0.0');
      Strings.Add('10.0,0.0');
      Strings.Add('# Heel Method');
      Strings.Add('0');
      Strings.Add('# Heel: Number of speeds ( >= 2)');
      Strings.Add('2');
      Strings.Add('# Heel: speed, angle');
      Strings.Add('0.0,0.0');
      Strings.Add('10.0,0.0');
      Strings.Add('# Appendages (0=None)');
      Strings.Add('0');
      Strings.Add('# Other Particulars (0=None)');
      Strings.Add('0');
   end;
   Strings.SaveToFile(Filename);
   Strings.Destroy;
end;{TFreeMichletOutputDialog.SaveToFile}

function TFreeMichletOutputDialog.FGetMultihull:boolean;
begin
   result:=(Radiobutton2.checked) or (Radiobutton3.checked);
end;{TFreeMichletOutputDialog.FGetMultihull}

procedure TFreeMichletOutputDialog.RadioButton1Click(Sender: TObject);
begin
   Distance.Enabled:=Multihull;
   if not Multihull then Distance.Value:=0.0;
   FBuildOffsets;
end;{TFreeMichletOutputDialog.RadioButton1Click}

procedure TFreeMichletOutputDialog.DistanceAfterSetValue(Sender: TObject);
begin
   FBuildOffsets;
   viewport.Zoomextents;
end;{TFreeMichletOutputDialog.DistanceAfterSetValue}

procedure TFreeMichletOutputDialog.Edit1AfterSetValue(Sender: TObject);
begin
   NumberOfStations:=Edit1.Value;
end;{TFreeMichletOutputDialog.Edit1AfterSetValue}

procedure TFreeMichletOutputDialog.Edit2AfterSetValue(Sender: TObject);
begin
   NumberOfWaterLines:=Edit2.Value;
end;{TFreeMichletOutputDialog.Edit2AfterSetValue}

procedure TFreeMichletOutputDialog.Edit3AfterSetValue(Sender: TObject);
begin
   Draft:=Edit3.Value;
end;{TFreeMichletOutputDialog.Edit3AfterSetValue}

procedure TFreeMichletOutputDialog.Edit7AfterSetValue(Sender: TObject);
begin
   WaterDensity:=Edit7.value;
end;{TFreeMichletOutputDialog.Edit7AfterSetValue}

procedure TFreeMichletOutputDialog.Edit4AfterSetValue(Sender: TObject);
begin
   Length:=Edit4.Value;
end;{TFreeMichletOutputDialog.Edit4AfterSetValue}

procedure TFreeMichletOutputDialog.Edit5AfterSetValue(Sender: TObject);
begin
   Volume:=Edit5.Value;
end;{TFreeMichletOutputDialog.Edit5AfterSetValue}

procedure TFreeMichletOutputDialog.Edit6AfterSetValue(Sender: TObject);
begin
   G:=Edit6.Value;
end;{TFreeMichletOutputDialog.Edit6AfterSetValue}

procedure TFreeMichletOutputDialog.Edit9AfterSetValue(Sender: TObject);
begin
   WaterDepth:=Edit9.Value;
end;{TFreeMichletOutputDialog.Edit9AfterSetValue}

procedure TFreeMichletOutputDialog.Edit12AfterSetValue(Sender: TObject);
begin
   StartSpeed:=Edit12.Value;
end;{TFreeMichletOutputDialog.Edit12AfterSetValue}

procedure TFreeMichletOutputDialog.Edit13AfterSetValue(Sender: TObject);
begin
   Endspeed:=Edit13.Value;
end;{TFreeMichletOutputDialog.Edit13AfterSetValue}

procedure TFreeMichletOutputDialog.Edit14AfterSetValue(Sender: TObject);
begin
   NumberOfSpeeds:=Edit14.Value;
end;{TFreeMichletOutputDialog.Edit14AfterSetValue}

procedure TFreeMichletOutputDialog.Edit15AfterSetValue(Sender: TObject);
begin
   R0:=Edit15.Value;
end;{TFreeMichletOutputDialog.Edit15AfterSetValue}

procedure TFreeMichletOutputDialog.Edit16AfterSetValue(Sender: TObject);
begin
   R1:=Edit16.Value;
end;{TFreeMichletOutputDialog.Edit16AfterSetValue}

procedure TFreeMichletOutputDialog.Edit17AfterSetValue(Sender: TObject);
begin
   Beta:=Edit17.Value;
end;{TFreeMichletOutputDialog.Edit17AfterSetValue}

procedure TFreeMichletOutputDialog.Edit18AfterSetValue(Sender: TObject);
begin
   Nr:=Edit18.Value;
end;{TFreeMichletOutputDialog.Edit18AfterSetValue}

procedure TFreeMichletOutputDialog.Edit19AfterSetValue(Sender: TObject);
begin
   NBeta:=Edit19.Value;
end;{TFreeMichletOutputDialog.Edit19AfterSetValue}

procedure TFreeMichletOutputDialog.Edit20AfterSetValue(Sender: TObject);
begin
   X0:=Edit20.Value;
end;{TFreeMichletOutputDialog.Edit20AfterSetValue}

procedure TFreeMichletOutputDialog.Edit21AfterSetValue(Sender: TObject);
begin
   X1:=Edit21.Value;
end;{TFreeMichletOutputDialog.Edit21AfterSetValue}

procedure TFreeMichletOutputDialog.Edit22AfterSetValue(Sender: TObject);
begin
   Y0:=Edit22.value;
end;{TFreeMichletOutputDialog.Edit22AfterSetValue}

procedure TFreeMichletOutputDialog.Edit23AfterSetValue(Sender: TObject);
begin
   Y1:=Edit23.Value;
end;{TFreeMichletOutputDialog.Edit23AfterSetValue}

procedure TFreeMichletOutputDialog.Edit24AfterSetValue(Sender: TObject);
begin
   Nx:=Edit24.Value;
end;{TFreeMichletOutputDialog.Edit24AfterSetValue}

procedure TFreeMichletOutputDialog.Edit25AfterSetValue(Sender: TObject);
begin
   Ny:=Edit25.Value;
end;{TFreeMichletOutputDialog.Edit25AfterSetValue}

procedure TFreeMichletOutputDialog.FormResize(Sender: TObject);
begin
   OKButton.Left:=Panel1.Clientwidth-CancelButton.Width-OkButton.Width-5;
   CancelButton.Left:=Panel1.Clientwidth-CancelButton.Width-5;
end;{TFreeMichletOutputDialog.FormResize}

end.
