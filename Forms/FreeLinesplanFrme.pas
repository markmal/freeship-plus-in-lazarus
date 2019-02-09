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

unit FreeLinesplanFrme;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
  PrintersDlgs, Printer4Lazarus, FreePrinter,
{$ENDIF}
  Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     FreeTypes,
     FreeShipUnit,
     FreeGeometry,
     ExtCtrls,
     ComCtrls,
     ToolWin,
     Printers,
     Math,
     ImgList,
     ActnList, StdCtrls, Spin;

const SpacePercentage    = 0.20;
      Textspace          = 0.05;

type TLinesplanView      = (lvProfile,lvAftBody,lvFrontBody,lvPlan);
     TLinesplanViews     = set of TLinesplanView;

     { TFreeLinesplanFrame }

     TFreeLinesplanFrame = class(TFrame)
                                SpinEdit1: TSpinEdit;
                                ToolButton15: TToolButton;
                                 Viewport: TFreeViewport;
                                 ActionList1: TActionList;
                                 ZoomExtents: TAction;
                                 SaveBitmap: TAction;
                                 ZoomIn: TAction;
                                 ZoomOut: TAction;
                                 ExportDXF: TAction;
                                 ShowFillColor: TAction;
                                 MenuImages: TImageList;
                                 ToolBar1: TToolBar;
                                 ToolButton5: TToolButton;
                                 ToolButton8: TToolButton;
                                 ToolButton9: TToolButton;
                                 ToolButton6: TToolButton;
                                 ToolButton19: TToolButton;
                                 ToolButton14: TToolButton;
                                 ToolButton7: TToolButton;
                                 ToolButton13: TToolButton;
                                 Print: TAction;
                                 ToolButton1: TToolButton;
                                 PrintDialog: TPrintDialog;
                                 UseLights: TAction;
                                 ToolButton2: TToolButton;
                                 ShowMonochrome: TAction;
                                 ToolButton3: TToolButton;
                                 MirrorPlanView: TAction;
                                 ToolButton4: TToolButton;
                                 procedure SpinEdit1Change(Sender: TObject);
                                 procedure ViewportRequestExtents(Sender: TObject; var Min,Max: T3DCoordinate);
                                 procedure ViewportRedraw(Sender: TObject);
                                 procedure ZoomExtentsExecute(Sender: TObject);
                                 procedure ZoomInExecute(Sender: TObject);
                                 procedure ZoomOutExecute(Sender: TObject);
                                 procedure ShowFillColorExecute(Sender: TObject);
                                 procedure PrintExecute(Sender: TObject);
                                 procedure ViewportMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
                                 procedure ViewportMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
                                 procedure SaveBitmapExecute(Sender: TObject);
                                 procedure UseLightsExecute(Sender: TObject);
                                 procedure ShowMonochromeExecute(Sender: TObject);
                                 procedure MirrorPlanViewExecute(Sender: TObject);
                                 procedure ExportDXFExecute(Sender: TObject);
                              private   { Private declarations }
                                 FFreeShip         : TFreeShip;
                                 FModelLength      : TFloatType;
                                 FModelHeight      : TFloatType;
                                 FModelBeam        : TFloatType;
                                 FDiagonalWidth    : TFloatType;
                                 FMin3D,FMax3D     : T3DCoordinate;
                                 FProfileOrigin    : T3DCoordinate;
                                 FAftOrigin        : T3DCoordinate;
                                 FFrontOrigin      : T3DCoordinate;
                                 FPlanOrigin       : T3DCoordinate;
                                 FInitialPosition  : TPoint;
                                 procedure FSetFreeShip(Val:TFreeShip);
                                 procedure CreateViewport;
                              private
                              public    { Public declarations }
                                 constructor Create(TheOwner: TComponent); override;
                                 procedure UpdateMenu;
                                 property FreeShip:TFreeShip read FFreeShip write FSetFreeShip;
                           end;

var FreeLinesplanFrame:TFreeLinesplanFrame;

implementation

uses FreeLanguageSupport,
     FasterList;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function CalculateSpace(Percentage,Min,Max:TFloatType):TFloatType;
begin
   Result:=Percentage*(Max-Min);
end;{Space}

{
   object Viewport: TFreeViewport
    Left = 0
    Height = 465
    Top = 29
    Width = 761
    Angle = 90
    Align = alClient
    BackgroundImage.Alpha = 255
    BackgroundImage.Owner = Viewport
    BackgroundImage.Quality = 100
    BackgroundImage.Scale = 1
    BackgroundImage.ShowInView = fvBodyplan
    BackgroundImage.Tolerance = 5
    BackgroundImage.Transparent = False
    BackgroundImage.TransparentColor = clBlack
    BackgroundImage.Visible = True
    BevelOuter = bvLowered
    CameraType = ftStandard
    Color = clWhite
    DoubleBuffer = True
    Elevation = 90
    Margin = 1
    ViewType = fvPlan
    ViewportMode = vmWireFrame
    OnMouseDown = ViewportMouseDown
    OnMouseMove = ViewportMouseMove
    OnRedraw = ViewportRedraw
    OnRequestExtents = ViewportRequestExtents
  end
}

procedure TFreeLinesplanFrame.CreateViewport;
begin
  Viewport := TFreeViewport.Create(Self);
  with Viewport do
    begin
    Parent := Self;
    Left := 0;
    Height := 465;
    Top := 29;
    Width := 761;
    Angle := 90;
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
    BevelOuter := bvLowered;
    CameraType := ftStandard;
    Color := clWhite;
    DoubleBuffer := True;
    Elevation := 90;
    Margin := 1;
    ViewType := fvPlan;
    ViewportMode := vmWireFrame;
    OnMouseDown := ViewportMouseDown;
    OnMouseMove := ViewportMouseMove;
    OnRedraw := ViewportRedraw;
    OnRequestExtents := ViewportRequestExtents;
    end;
end;


constructor TFreeLinesplanFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  CreateViewport;
end;

procedure TFreeLinesplanFrame.UpdateMenu;
begin
   ShowFillcolor.Enabled:=not ShowMonochrome.Checked;
   UseLights.Enabled:=(ShowFillcolor.Checked) and (not ShowMonochrome.Checked);
   MirrorPlanview.Enabled:=Freeship.NumberofDiagonals=0;
end;{TFreeLinesplanFrame.UpdateMenu}

procedure TFreeLinesplanFrame.FSetFreeShip(Val:TFreeShip);
begin
   if FFreeShip<>nil then FFreeShip.LinesplanFrame:=nil;
   FFreeShip:=Val;
   if FFreeShip<>nil then
   begin
      //USE ONCE!
      ///FFreeship.Preferences.dumpIcons(MenuImages,ActionList1);

      Freeship.Preferences.LoadImageListByActions(MenuImages,ActionList1);

      FFreeShip.LinesplanFrame:=self;
      UpdateMenu;
   end;
end;{TFreeLinesplanFrame.FSetFreeShip}

procedure TFreeLinesplanFrame.ViewportRequestExtents(Sender: TObject;var Min, Max: T3DCoordinate);
var Space,Tmp     : TFloatType;
    Min3D,Max3D   : T3DCoordinate;
    P,Diff        : T3DCoordinate;
    I,J,K         : Integer;
    Diagonal      : TFreeIntersection;
    Spline        : TFreeSpline;
    Plane         : T3DPlane;
    First         : Boolean;
    Layer         : TFreeSubdivisionLayer;
begin
   if Freeship<>nil then
   begin
      First:=True;
      for I:=1 to Freeship.NumberOfLayers do
      begin
         Layer:=Freeship.Layer[I-1];
         if Layer.ShowInLinesplan then
         begin
            for J:=1 to Layer.Count do
            begin
               if First then
               begin
                  Min3D:=Layer.Items[J-1].Min;
                  Max3D:=Min3D;
                  First:=False;
               end;
               MinMax(Layer.Items[J-1].Min,Min3D,Max3D);
               MinMax(Layer.Items[J-1].Max,Min3D,Max3D);
            end;
         end;
      end;
      for I:=1 to Freeship.NumberofStations do Freeship.Station[I-1].Extents(Min3D,Max3D);
      for I:=1 to Freeship.NumberofButtocks do Freeship.Buttock[I-1].Extents(Min3D,Max3D);
      for I:=1 to Freeship.NumberofWaterlines do Freeship.Waterline[I-1].Extents(Min3D,Max3D);
      for I:=1 to Freeship.NumberofDiagonals do Freeship.Diagonal[I-1].Extents(Min3D,Max3D);

      FMin3D:=Min3D;
      FMax3D:=Max3D;
      FModelHeight:=Max3D.Z-Min3D.Z;
      FModelBeam:=2*Max3D.Y;
      FModelLength:=Max3D.X-Min3D.X;
      Space:=SpacePercentage*FModelHeight;
      if 2*FModelBeam+Space>FModelLength then FModelLength:=2*FModelBeam+Space;
      FDiagonalWidth:=0;
      for I:=1 to Freeship.NumberofDiagonals do
      begin
         Diagonal:=Freeship.Diagonal[I-1];
         Plane.a:=0.0;
         Plane.b:= 1/Sqrt(2);
         Plane.c:=-1/sqrt(2);
         Plane.d:=-Diagonal.Plane.d;
         if not Diagonal.Build then Diagonal.Rebuild;
         for J:=1 to Diagonal.Count do
         begin
            Spline:=Diagonal.Items[J-1];
            for k:=1 to Spline.NumberOfPoints do
            begin
               P:=Spline.Point[K-1];
               Tmp:=abs(Plane.A*P.x+Plane.B*P.y+Plane.C*P.z+Plane.D);
               if Tmp>FDiagonalWidth then FDiagonalWidth:=Tmp;
            end;
         end;
      end;

      Min.X:=Min3D.X-0.5*space;
      // if no diagonals present in model, the mirror planview
      if Freeship.NumberofDiagonals>0 then Min.Y:=Min3D.Z-2*Space-FModelHeight-0.5*FModelBeam-0.5*space-space-FDiagonalWidth else
         if (Freeship.NumberofDiagonals=0) and (MirrorPlanview.Checked) then Min.Y:=Min3D.Z-2*Space-FModelHeight-FModelBeam-0.5*space else
            Min.Y:=Min3D.Z-2*Space-FModelHeight-0.5*FModelBeam-0.5*space;

      Min.Z:=0.0;
      Max.X:=Min.X+FModelLength+0.5*space;
      Max.Y:=-Min3D.Z+FModelHeight+0.5*space;
      Max.Z:=0.0;
      Tmp:=(FModelLength-2*FModelbeam-3*space)/3;

      // Attachpoint for profileview
      FProfileOrigin.X:=0.0;
      FProfileOrigin.Y:=-Min3D.Z;
      FProfileOrigin.Z:=0.0;
      // Attachpoint for aft view of bodyplan
      FAftOrigin.X:=Min.X+Tmp+0.5*FModelbeam;
      FAftOrigin.Y:=FProfileOrigin.Y-Space-FModelHeight;
      FAftOrigin.Z:=0.0;
      // Attachpoint for front view of bodyplan
      FFrontOrigin:=FAftOrigin;
      FFrontOrigin.X:=Max.X-Tmp-0.5*FModelbeam;
      // Attachpoint for plan view
      FPlanOrigin.X:=FProfileOrigin.X;
      FPlanOrigin.Y:=FAftOrigin.Y-space-0.5*FModelBeam+Min3D.Z;
      FPlanOrigin.Z:=0.0;

       // Blowup the boundary box by 3%
       Diff:=ScalePoint(0.03,Subtract(Max3D,Min3D));
       Min:=Subtract(Min,Diff);
       Diff:=ScalePoint(-1.0,Diff);
       Max:=Subtract(Max,diff);

   end else
   begin
      Min.X:=-1;
      Min.Y:=Min.X;
      Min.Z:=Min.X;
      Max.X:=-Min.X;
      Max.Y:=Max.X;
      Max.Z:=Max.X;
   end;
end;{TFreeLinesplanFrame.ViewportRequestExtents}

procedure TFreeLinesplanFrame.SpinEdit1Change(Sender: TObject);
begin
   Font.Size := SpinEdit1.Value;
   Viewport.invalidate;
end;

procedure TFreeLinesplanFrame.ViewportRedraw(Sender: TObject);
type TriangleData = record
                       P1,P2,P3  : T3DCoordinate;
                       Center    : T3DCoordinate;
                       Color     : TColor;
                       Symmetric : boolean;
                    end;
     TriangleArray= record
                       Capacity  : integer;
                       Count     : integer;
                       Triangles : array of TriangleData;
                    end;
var I,J,N,K       : Integer;
    Edge          : TFreeSubdivisionEdge;
    Layer         : TFreeSubdivisionLayer;
    Face          : TFreeSubdivisionControlface;
    Child         : TFreeSubdivisionFace;
    Mainframe     : TFloatType;
    Tmp,Space     : TFloatType;
    Min,Max       : TFloatType;
    ResFactor     : TFloatType;
    ScaleFactor   : TFloatType;
    WlPlane       : T3DPlane;
    MainPlane     : T3DPlane;
    Plane         : T3DPlane;
    Below         : TriangleArray;
    Above         : TriangleArray;
    Done          : Boolean;
    Diagonal      : TFreeIntersection;
    Spline        : TFreeSpline;
    P,P1,P2       : T3DCoordinate;
    Pts           : array of TPoint;
    Pt            : TPoint;
    Steps         : integer;
    PenwidthFactor: integer;
    Str           : string;
    Prevcursor    : TCursor;
    SubmColor     : TColor;

   procedure DrawLineAtt(Attachpoint,P1,P2:T3DCoordinate;Text:string;CenterText:Boolean);
   var Proj1: T3DCoordinate;
       Proj2: T3DCoordinate;
       Pts  : array[0..1] of TPoint;
       W    : Integer;
   begin
      Proj1.X:=Attachpoint.X+P1.X;
      Proj1.Y:=Attachpoint.Y+P1.Y;
      Proj1.Z:=0.0;
      Pts[0]:=Viewport.Project(Proj1);
      Proj2.X:=Attachpoint.X+P2.X;
      Proj2.Y:=Attachpoint.Y+P2.Y;
      Proj2.Z:=0.0;
      Pts[1]:=Viewport.Project(Proj2);
      Viewport.Polyline(Pts);
      if Text<>'' then
      begin
         if (CenterText) and (Pts[0].X=Pts[1].X) then
         begin
            W:=Viewport.TextWidth(Text) div 2;
            Viewport.TextOut(Pts[0].X-W,Pts[0].Y,Text);
            Viewport.TextOut(Pts[1].X-W,Pts[1].Y,Text);
         end else
         begin
            W:=0;
            Viewport.TextOut(Pts[1].X-W,Pts[0].Y,Text);
            if (Pts[0].Y=Pts[1].Y) and (Pts[1].X<>Pts[0].X) then W:=Viewport.TextWidth(Text)
                                                            else W:=0;
            Viewport.TextOut(Pts[0].X-W,Pts[1].Y,Text);
         end;
      end;
   end;{DrawLineAtt}

   procedure DrawDiagonalLine(Origin:T3DCoordinate;P1,P2:T3DCoordinate);
   var Pt: TPoint;
       P : T3DCoordinate;
   begin
      P.X:=Origin.X+P1.Y;
      P.Y:=Origin.Y+P1.Z;
      P.Z:=0.0;
      Pt:=Viewport.Project(P);
      Viewport.MoveTo(Pt.X,Pt.Y);
      P.X:=Origin.X+P2.Y;
      P.Y:=Origin.Y+P2.Z;
      Pt:=Viewport.Project(P);
      Viewport.LineTo(Pt.X,Pt.Y);
      P.X:=Origin.X-P1.Y;
      P.Y:=Origin.Y+P1.Z;
      Pt:=Viewport.Project(P);
      Viewport.MoveTo(Pt.X,Pt.Y);
      P.X:=Origin.X-P2.Y;
      P.Y:=Origin.Y+P2.Z;
      Pt:=Viewport.Project(P);
      Viewport.LineTo(Pt.X,Pt.Y);
   end;{DrawDiagonalLine}

   procedure DrawLine(P1,P2:T3DCoordinate);
   var Proj1:T3DCoordinate;
       Proj2:T3DCoordinate;
       Pts:array[0..1] of TPoint;
   begin
      // Draw in profileview
      Proj1.X:=FProfileOrigin.X+P1.X;
      Proj1.Y:=FProfileOrigin.Y+P1.Z;
      Proj1.Z:=0.0;
      Pts[0]:=Viewport.Project(Proj1);
      Proj2.X:=FProfileOrigin.X+P2.X;
      Proj2.Y:=FProfileOrigin.Y+P2.Z;
      Proj2.Z:=0.0;
      Pts[1]:=Viewport.Project(Proj2);
      Viewport.Polyline(Pts);

      // Draw in aft view of bodyplan
      if (P1.X<=Mainframe) and (P2.X<Mainframe) then
      begin
         Proj1.X:=FAftOrigin.X-P1.Y;
         Proj1.Y:=FAftOrigin.Y+P1.Z;
         Pts[0]:=Viewport.Project(Proj1);
         Proj2.X:=FAftOrigin.X-P2.Y;
         Proj2.Y:=FAftOrigin.Y+P2.Z;
         Pts[1]:=Viewport.Project(Proj2);
         Viewport.PolyLine(Pts);
         Proj1.X:=FAftOrigin.X+P1.Y;
         Proj1.Y:=FAftOrigin.Y+P1.Z;
         Pts[0]:=Viewport.Project(Proj1);
         Proj2.X:=FAftOrigin.X+P2.Y;
         Proj2.Y:=FAftOrigin.Y+P2.Z;
         Pts[1]:=Viewport.Project(Proj2);
         Viewport.Polyline(Pts);
      end;

      // Draw in front view of bodyplan
      if (P1.X>=Mainframe) and (P2.X>Mainframe) then
      begin
         Proj1.X:=FFrontOrigin.X-P1.Y;
         Proj1.Y:=FFrontOrigin.Y+P1.Z;
         Pts[0]:=Viewport.Project(Proj1);
         Proj2.X:=FFrontOrigin.X-P2.Y;
         Proj2.Y:=FFrontOrigin.Y+P2.Z;
         Pts[1]:=Viewport.Project(Proj2);
         Viewport.Polyline(Pts);
         Proj1.X:=FFrontOrigin.X+P1.Y;
         Proj1.Y:=FFrontOrigin.Y+P1.Z;
         Pts[0]:=Viewport.Project(Proj1);
         Proj2.X:=FFrontOrigin.X+P2.Y;
         Proj2.Y:=FFrontOrigin.Y+P2.Z;
         Pts[1]:=Viewport.Project(Proj2);
         Viewport.Polyline(Pts);
      end;
      // Draw in plan view
      Proj1.X:=FPlanOrigin.X+P1.X;
      Proj1.Y:=FPlanOrigin.Y+P1.Y;
      Pts[0]:=Viewport.Project(Proj1);
      Proj2.X:=FPlanOrigin.X+P2.X;
      Proj2.Y:=FPlanOrigin.Y+P2.Y;
      Pts[1]:=Viewport.Project(Proj2);
      Viewport.Polyline(Pts);
      if (Freeship.NumberofDiagonals=0) and (MirrorPlanview.Checked) then
      begin
         Proj1.X:=FPlanOrigin.X+P1.X;
         Proj1.Y:=FPlanOrigin.Y-P1.Y;
         Pts[0]:=Viewport.Project(Proj1);
         Proj2.X:=FPlanOrigin.X+P2.X;
         Proj2.Y:=FPlanOrigin.Y-P2.Y;
         Pts[1]:=Viewport.Project(Proj2);
         Viewport.Polyline(Pts);
      end;
   end;{DrawLine}

   procedure DrawSpline(Spline:TFreeSpline;Views:TLinesplanViews;Style:TPenStyle);
   var I    : Integer;
       P,Pr : T3DCoordinate;
       Pts  : array of TPoint;
   begin
      Setlength(Pts,Steps+1);
      Viewport.PenStyle:=Style;
      if lvProfile in views then
      begin
         for I:=0 to steps do
         begin
            P:=Spline.Value(I/steps);
            Pr.X:=FProfileOrigin.X+P.X;
            Pr.Y:=FProfileOrigin.Y+P.Z;
            Pr.Z:=0.0;
            Pts[I]:=Viewport.Project(Pr);
         end;
         Viewport.Polyline(Pts);
      end;
      if (lvAftBody in views) and (Spline.Max.X<=MainFrame) then
      begin
         for I:=0 to steps do
         begin
            P:=Spline.Value(I/steps);
            Pr.X:=FAftOrigin.X-P.Y;
            Pr.Y:=FAftOrigin.Y+P.Z;
            Pr.Z:=0.0;
            Pts[I]:=Viewport.Project(Pr);
         end;
         Viewport.Polyline(Pts);
         for I:=0 to steps do
         begin
            P:=Spline.Value(I/steps);
            Pr.X:=FAftOrigin.X+P.Y;
            Pr.Y:=FAftOrigin.Y+P.Z;
            Pr.Z:=0.0;
            Pts[I]:=Viewport.Project(Pr);
         end;
         Viewport.Polyline(Pts);
      end;
      if (lvFrontBody in views) and (Spline.Min.X>=MainFrame) then
      begin
         for I:=0 to steps do
         begin
            P:=Spline.Value(I/steps);
            Pr.X:=FFrontOrigin.X-P.Y;
            Pr.Y:=FFrontOrigin.Y+P.Z;
            Pr.Z:=0.0;
            Pts[I]:=Viewport.Project(Pr);
         end;
         Viewport.Polyline(Pts);
         for I:=0 to steps do
         begin
            P:=Spline.Value(I/steps);
            Pr.X:=FFrontOrigin.X+P.Y;
            Pr.Y:=FFrontOrigin.Y+P.Z;
            Pr.Z:=0.0;
            Pts[I]:=Viewport.Project(Pr);
         end;
         Viewport.Polyline(Pts);
      end;
      if lvPLan in views then
      begin
         for I:=0 to steps do
         begin
            P:=Spline.Value(I/steps);
            Pr.X:=FPlanOrigin.X+P.X;
            Pr.Y:=FPlanOrigin.Y+P.Y;
            Pr.Z:=0.0;
            Pts[I]:=Viewport.Project(Pr);
         end;
         Viewport.Polyline(Pts);
         if (Freeship.NumberofDiagonals=0) and (MirrorPlanview.Checked) then
         begin
            for I:=0 to steps do
            begin
               P:=Spline.Value(I/steps);
               Pr.X:=FPlanOrigin.X+P.X;
               Pr.Y:=FPlanOrigin.Y-P.Y;
               Pr.Z:=0.0;
               Pts[I]:=Viewport.Project(Pr);
            end;
            Viewport.Polyline(Pts);
         end;
      end;
   end;{DrawSpline}

   procedure DrawIntersection(Intersection:TFreeIntersection;Views:TLinesplanViews;Style:TPenStyle);
   var I:Integer;
   begin
      if not Intersection.Build then Intersection.Rebuild;
      for I:=1 to Intersection.Count do DrawSpline(Intersection.Items[I-1],Views,Style);
   end;{DrawIntersection}

   procedure AddTriangle(P1,P2,P3:T3DCoordinate;Color:TColor;var Destination:TriangleArray;Symmetric:boolean);
   var C:T3DCoordinate;
   begin
      if Destination.Count=Destination.Capacity then
      begin
         Destination.Capacity:=Destination.Capacity+50;
         Setlength(Destination.Triangles,Destination.Capacity);
      end;
      C.X:=(P1.X+P2.X+P3.X)/3;
      C.Y:=(P1.Y+P2.Y+P3.Y)/3;
      C.Z:=(P1.Z+P2.Z+P3.Z)/3;
      Destination.Triangles[Destination.Count].P1:=P1;
      Destination.Triangles[Destination.Count].P2:=P2;
      Destination.Triangles[Destination.Count].P3:=P3;
      Destination.Triangles[Destination.Count].Center:=C;
      Destination.Triangles[Destination.Count].Color:=Color;
      Destination.Triangles[Destination.Count].Symmetric:=Symmetric;
      inc(Destination.Count);
   end;{AddTriangle}

   procedure ProcessFace(Face:TFreeSubdivisionFace;Color:TColor;Symmetric:boolean);
   var I,J           : integer;
       Nabove        : integer;
       Nbelow        : Integer;
       AbovePoints   : TFreeCoordinateArray;
       BelowPoints   : TFreeCoordinateArray;
   begin
      for I:=3 to Face.NumberOfpoints do
      begin
         ClipTriangle(Face.Point[0].Coordinate,
                      Face.Point[I-2].Coordinate,
                      Face.Point[I-1].Coordinate,
                      WlPlane,Nabove,Nbelow,AbovePoints,BelowPoints);
         // proces submerged area
         for J:=3 to NBelow do AddTriangle(BelowPoints[0],BelowPoints[J-2],BelowPoints[J-1],SubmColor,Below,Symmetric);
         for J:=3 to NAbove do AddTriangle(AbovePoints[0],AbovePoints[J-2],AbovePoints[J-1],Color,Above,Symmetric);
      end;
   end;{Processface}

   procedure SortTriangles(var Triangles:TriangleArray;SortType:byte);// Sorttype 1=X, 2=Y, 3=Z

      procedure QuickSort(L,R:Integer);
      var I, J : Integer;
          T1   : TriangleData;

       Procedure Swap(I,J:Integer);
       var Tmp : TriangleData;
       begin
          Tmp:=Triangles.Triangles[I];
          Triangles.Triangles[I]:=Triangles.Triangles[J];
          Triangles.Triangles[J]:=Tmp;
       end;{swap two triangles}

      begin
         I:=L;
         J:=R;
         T1:=Triangles.Triangles[(L+R) div 2];
         repeat
            if SortType=1 then
            begin
   	         While Triangles.Triangles[I].Center.X<T1.Center.X do Inc(I);
      	      while T1.Center.X<Triangles.Triangles[J].Center.X do Dec(J);
            end else if SortType=2 then
            begin
               While Triangles.Triangles[I].Center.Y<T1.Center.Y do Inc(I);
      	      while T1.Center.Y<Triangles.Triangles[J].Center.Y do Dec(J);
            end else
            begin
               While Triangles.Triangles[I].Center.Z<T1.Center.Z do Inc(I);
      	      while T1.Center.Z<Triangles.Triangles[J].Center.Z do Dec(J);
            end;
            if I<=J then
            begin
               Swap(I,J);
               Inc(I);
               Dec(J);
            end;
         Until I>J;
         if L<J then QuickSort(L,J);
         if I<R then QuickSort(I,R);
      end;{QuickSort}

   begin
      if Triangles.Count>1 then QuickSort(0,Triangles.Count-1);
   end;{SortTriangles}

   procedure DrawTriangles(Triangles:TriangleArray;Views:TLinesplanViews);
   var I       : Integer;
       Triangle: TriangleData;
       Pts     : array[0..2] of TPoint;
       P       : T3DCoordinate;
       L       : T3DCoordinate;
       Col     : TColor;

       function GetColor(Lightvector:T3DCoordinate;DpScale,Contrast:TFloatType;Triangle:TriangleData):TColor;
       var N      : T3DCoordinate;
           Dp     : TFloatType;
           R,G,B  : byte;
       begin
          N:=UnifiedNormal(Triangle.P1,Triangle.P2,Triangle.P3);
          Dp:=DotProduct(N,L);
          if Dp<0.0 then Dp:=-Dp;

          Dp:=Power(Dp,Contrast);
          Dp:=(DpScale-1.0)+DpScale*Dp;
          if Dp>1.0 then Dp:=1.0;
          R:=Round(Dp*GetRValue(Triangle.Color));
          G:=Round(Dp*GetGValue(Triangle.Color));
          B:=Round(Dp*GetBValue(Triangle.Color));
          Result:=RGB(R,G,B);
          {
          Viewport.ShadedColor(Dp,GetRValue(Triangle.Color),GetGValue(Triangle.Color),GetBValue(Triangle.Color),R,G,B);
          Result:=RGB(R,G,B);
          }
       end;{GetColor}

   begin
      if lvAftBody in views then
      begin
         P.Z:=0.0;
         L:=SetPoint(-1.0,0.0,0.0);
         for I:=Triangles.Count downto 1 do
         begin
            Triangle:=Triangles.Triangles[I-1];
            if not UseLights.Checked then
            begin
               Viewport.BrushColor:=Triangle.Color;
               Viewport.PenColor:=Triangle.Color;
            end else
            begin
               Col:=GetColor(L,1.4,1.0,Triangle);
               Viewport.BrushColor:=Col;
               Viewport.PenColor:=Col;
            end;
            // Draw in aft view
            P.X:=FAftOrigin.X-Triangle.P1.Y;
            P.Y:=FAftOrigin.Y+Triangle.P1.Z;
            Pts[0]:=Viewport.Project(P);
            P.X:=FAftOrigin.X-Triangle.P2.Y;
            P.Y:=FAftOrigin.Y+Triangle.P2.Z;
            Pts[1]:=Viewport.Project(P);
            P.X:=FAftOrigin.X-Triangle.P3.Y;
            P.Y:=FAftOrigin.Y+Triangle.P3.Z;
            Pts[2]:=Viewport.Project(P);
            Viewport.Polygon(Pts);
            if Triangle.Symmetric then
            begin
               P.X:=FAftOrigin.X+Triangle.P1.Y;
               P.Y:=FAftOrigin.Y+Triangle.P1.Z;
               Pts[0]:=Viewport.Project(P);
               P.X:=FAftOrigin.X+Triangle.P2.Y;
               P.Y:=FAftOrigin.Y+Triangle.P2.Z;
               Pts[1]:=Viewport.Project(P);
               P.X:=FAftOrigin.X+Triangle.P3.Y;
               P.Y:=FAftOrigin.Y+Triangle.P3.Z;
               Pts[2]:=Viewport.Project(P);
               Viewport.Polygon(Pts);
            end;
         end;
      end else
      begin
         L:=SetPoint(-1.0,0.0,0.0);
         for I:=1 to Triangles.Count do
         begin
            Triangle:=Triangles.Triangles[I-1];
            P.Z:=0.0;
            if lvProfile in Views then
            begin
               L:=SetPoint(0.0,-1.0,0.0);
               if not UseLights.Checked then
               begin
                  Viewport.BrushColor:=Triangle.Color;
                  Viewport.PenColor:=Triangle.Color;
               end else
               begin
                  Col:=GetColor(L,1.1,1.5,Triangle);
                  Viewport.BrushColor:=Col;
                  Viewport.PenColor:=Col;
               end;
               // Draw in profile view
               P.X:=FProfileOrigin.X+Triangle.P1.X;
               P.Y:=FProfileOrigin.Y+Triangle.P1.Z;
               Pts[0]:=Viewport.Project(P);
               P.X:=FProfileOrigin.X+Triangle.P2.X;
               P.Y:=FProfileOrigin.Y+Triangle.P2.Z;
               Pts[1]:=Viewport.Project(P);
               P.X:=FProfileOrigin.X+Triangle.P3.X;
               P.Y:=FProfileOrigin.Y+Triangle.P3.Z;
               Pts[2]:=Viewport.Project(P);
               Viewport.Polygon(Pts);
            end;

            if lvFrontBody in views then
            begin
               L:=SetPoint(1.0,0.0,0.0);
               if not UseLights.Checked then
               begin
                  Viewport.BrushColor:=Triangle.Color;
                  Viewport.PenColor:=Triangle.Color;
               end else
               begin
                  Col:=GetColor(L,1.4,1.0,Triangle);
                  Viewport.BrushColor:=Col;
                  Viewport.PenColor:=Col;
               end;
               // Draw in Front view
               P.X:=FFrontOrigin.X-Triangle.P1.Y;
               P.Y:=FFrontOrigin.Y+Triangle.P1.Z;
               Pts[0]:=Viewport.Project(P);
               P.X:=FFrontOrigin.X-Triangle.P2.Y;
               P.Y:=FFrontOrigin.Y+Triangle.P2.Z;
               Pts[1]:=Viewport.Project(P);
               P.X:=FFrontOrigin.X-Triangle.P3.Y;
               P.Y:=FFrontOrigin.Y+Triangle.P3.Z;
               Pts[2]:=Viewport.Project(P);
               Viewport.Polygon(Pts);
               if Triangle.Symmetric then
               begin
                  P.X:=FFrontOrigin.X+Triangle.P1.Y;
                  P.Y:=FFrontOrigin.Y+Triangle.P1.Z;
                  Pts[0]:=Viewport.Project(P);
                  P.X:=FFrontOrigin.X+Triangle.P2.Y;
                  P.Y:=FFrontOrigin.Y+Triangle.P2.Z;
                  Pts[1]:=Viewport.Project(P);
                  P.X:=FFrontOrigin.X+Triangle.P3.Y;
                  P.Y:=FFrontOrigin.Y+Triangle.P3.Z;
                  Pts[2]:=Viewport.Project(P);
                  Viewport.Polygon(Pts);
               end;
            end;
            if lvPlan in views then
            begin
               L:=SetPoint(0.0,0.0,-1.0);
               if not UseLights.Checked then
               begin
                  Viewport.BrushColor:=Triangle.Color;
                  Viewport.PenColor:=Triangle.Color;
               end else
               begin
                  Col:=GetColor(L,1.1,1.5,Triangle);
                  Viewport.BrushColor:=Col;
                  Viewport.PenColor:=Col;
               end;
               // Draw portside in plan view
               P.X:=FPlanOrigin.X+Triangle.P1.X;
               P.Y:=FPlanOrigin.Y+Triangle.P1.Y;
               Pts[0]:=Viewport.Project(P);
               P.X:=FPlanOrigin.X+Triangle.P2.X;
               P.Y:=FPlanOrigin.Y+Triangle.P2.Y;
               Pts[1]:=Viewport.Project(P);
               P.X:=FPlanOrigin.X+Triangle.P3.X;
               P.Y:=FPlanOrigin.Y+Triangle.P3.Y;
               Pts[2]:=Viewport.Project(P);
               Viewport.Polygon(Pts);
               if (Freeship.NumberofDiagonals=0) and (MirrorPlanview.Checked) and (Triangle.Symmetric) then
               begin
                  P.X:=FPlanOrigin.X+Triangle.P1.X;
                  P.Y:=FPlanOrigin.Y-Triangle.P1.Y;
                  Pts[0]:=Viewport.Project(P);
                  P.X:=FPlanOrigin.X+Triangle.P2.X;
                  P.Y:=FPlanOrigin.Y-Triangle.P2.Y;
                  Pts[1]:=Viewport.Project(P);
                  P.X:=FPlanOrigin.X+Triangle.P3.X;
                  P.Y:=FPlanOrigin.Y-Triangle.P3.Y;
                  Pts[2]:=Viewport.Project(P);
                  Viewport.Polygon(Pts);
               end;
            end;
         end;
      end;
   end;{DrawTriangles}

   procedure SetFontHeight(DesiredHeight:TFloatType);
   var Height         : integer;
       CurrentHeight  : integer;
   begin
      // Sets the fontheight to a height in modelspace
      Height:=round(DesiredHeight*Viewport.Scale*Viewport.Zoom);
      CurrentHeight := Viewport.FontHeight;
      if CurrentHeight <> Height
        then Viewport.FontHeight := Height;

      // below code causes loop redraw and 100% CPU.
      // Changed to above code with direct set of required Font.Height
      {Height:=DesiredHeight*Viewport.Scale*Viewport.Zoom;
      Viewport.Font.Size:=8;
      CurrentHeight:=Viewport.TextHeight('X');
      while CurrentHeight>Height do
      begin
         Viewport.Font.Size:=Viewport.Font.Size-1;
         CurrentHeight:=Viewport.TextHeight('X');
         if Viewport.Font.Size<6 then break;
      end;}
   end;{SetFontHeight}

begin
   if Freeship=nil then exit;
   if Viewport.Printing then PenWidthfactor:=Round(Viewport.PrintScaleFactor)
                        else PenwidthFactor:=1;
   if Viewport.Printing then Steps:=1250
                        else Steps:=200;
   WlPlane.a:=0.0;
   WlPlane.b:=0.0;
   WlPlane.c:=1.0;
   WlPlane.d:=-(Freeship.Surface.Min.Z+Freeship.ProjectSettings.ProjectDraft);
   Mainframe:=Freeship.ProjectSettings.ProjectMainframeLocation;
   Mainplane.a:=1.0;
   Mainplane.b:=0.0;
   Mainplane.c:=0.0;
   Mainplane.d:=-Mainframe;

   Viewport.BrushStyle:=bsSolid;
   Below.Count:=0;
   Below.Capacity:=0;
   Above.Count:=0;
   Above.Capacity:=0;
   Prevcursor:=Screen.Cursor;
   Screen.Cursor:=crHourglass;
   Viewport.PenColor:=clSilver;
   Viewport.FontName:='Arial';
   Viewport.FontColor:=clBlack;
   // calculate and set fontheight
   //SetFontHeight(DistPP3D(FMin3D,FMax3D)/FontheightFactor * FFontheightScale);

   //just set font from the frame self
   Viewport.FontName:=Font.Name;
   Viewport.FontColor:=Font.Color;
   //Viewport.FontHeight:=Font.Height;
   Viewport.FontSize:=Font.Size;

   try
      if (not ShowMonochrome.Checked) and (ShowFillcolor.Checked) then
      begin
         for I:=1 to Freeship.Surface.NumberOfLayers do if Freeship.Layer[I-1].ShowInLinesplan then
         begin
            Layer:=Freeship.Surface.Layer[I-1];
            if Freeship.ProjectSettings.ProjectShadeUnderwaterShip then SubmColor:=Freeship.ProjectSettings.ProjectUnderWaterColor
                                                                   else SubmColor:=Layer.Color;
            for j:=1 to Layer.Count do
            begin
               Face:=Layer.Items[J-1];
               Done:=False;
               if Face.Max.Z<Freeship.Surface.Min.Z+Freeship.ProjectSettings.ProjectDraft then
               begin
                  for N:=1 to Face.ChildCount do
                  begin
                     Child:=Face.Child[N-1];
                     for K:=3 to Child.NumberOfpoints do AddTriangle(Child.Point[0].Coordinate,Child.Point[K-2].Coordinate,Child.Point[K-1].Coordinate,SubmColor,Below,Layer.Symmetric);
                  end;
                  Done:=True;
               end else if Face.Min.Z>Freeship.Surface.Min.Z+Freeship.ProjectSettings.ProjectDraft then
               begin
                  for N:=1 to Face.ChildCount do
                  begin
                     Child:=Face.Child[N-1];
                     for K:=3 to Child.NumberOfpoints do AddTriangle(Child.Point[0].Coordinate,Child.Point[K-2].Coordinate,Child.Point[K-1].Coordinate,Layer.Color,Above,Layer.Symmetric);
                  end;
                  Done:=True;
               end;
               if not done then for N:=1 to Face.ChildCount do Processface(Face.Child[N-1],Layer.Color,Layer.Symmetric);
            end;
         end;

         SortTriangles(Below,2);
         SortTriangles(Above,2);
         DrawTriangles(Below,[lvProfile]);
         DrawTriangles(Above,[lvProfile]);
         SortTriangles(Below,1);
         SortTriangles(Above,1);
         // Aft bodyplan
         DrawTriangles(Below,[lvAftBody]);
         DrawTriangles(Above,[lvAftBody]);
         // frontview on bodyplan
         DrawTriangles(Below,[lvFrontBody]);
         DrawTriangles(Above,[lvFrontBody]);
         // plan view
         SortTriangles(Below,3);
         SortTriangles(Above,3);
         DrawTriangles(Below,[lvPlan]);
         DrawTriangles(Above,[lvPlan]);

         // Draw dwl as a white band in profile and bodyplan views
         Viewport.PenColor:=Viewport.Color;
         Viewport.SetPenWidth(5*PenwidthFactor);
         Space:=CalculateSpace(0.5*textspace,FMin3D.X,FMax3D.X);
         Viewport.BrushStyle:=bsClear;
         // draw dwl in profile
         DrawLineAtt(FProfileOrigin,SetPoint(FMin3D.X-Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),SetPoint(FMax3D.X+Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),'',False);
         // draw dwl in aft bodyplan
         Space:=CalculateSpace(textspace,FMin3D.Y,FMax3D.Y);
         DrawLineAtt(FAftOrigin,SetPoint(-FMax3D.Y-Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),SetPoint(FMax3D.Y+Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),'',False);
         // draw dwl in front bodyplan
         DrawLineAtt(FFrontOrigin,SetPoint(-FMax3D.Y-Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),SetPoint(FMax3D.Y+Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),'',False);
      end;

      // Draw grid
      if ShowMonochrome.Checked then Viewport.PenColor:=clSilver
                                else Viewport.PenColor:=clSilver;
      Viewport.BrushStyle:=bsClear;
      // first draw grid in profile view
      Viewport.SetPenWidth(2*PenwidthFactor);
      Space:=CalculateSpace(0.2*textspace,FMin3D.X,FMax3D.X);
      if ShowMonochrome.Checked then Viewport.FontColor:=clBlack
                                else Viewport.FontColor:=clRed;
      // draw baseline
      DrawLineAtt(FProfileOrigin,SetPoint(FMin3D.X-Space,FMin3D.Z,0),SetPoint(FMax3D.X+Space,FMin3D.Z,0),Userstring(184)+#32+
                  ConvertDimension(FMin3D.Z,Freeship.ProjectSettings.ProjectUnits),False);
      // draw dwl
      DrawLineAtt(FProfileOrigin,SetPoint(FMin3D.X-Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),SetPoint(FMax3D.X+Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),Userstring(185)+#32+
                  ConvertDimension(FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,Freeship.ProjectSettings.ProjectUnits),False);
      Viewport.SetPenWidth(PenwidthFactor);
      Viewport.FontColor:=clBlack;
      for I:=1 to FFreeship.NumberofWaterlines do
      begin
         Tmp:=-FFreeship.Waterline[I-1].Plane.D;
         Str:=ConvertDimension(Tmp,Freeship.ProjectSettings.ProjectUnits);
         DrawLineAtt(FProfileOrigin,SetPoint(FMin3D.X-Space,Tmp,0),SetPoint(FMax3D.X+Space,Tmp,0),Str,False);
      end;
      Space:=CalculateSpace(textspace,FMin3D.Z,FMax3D.Z);
      for I:=1 to FFreeship.NumberofStations do
      begin
         Tmp:=-FFreeship.Station[I-1].Plane.D;
         Str:=ConvertDimension(Tmp,Freeship.ProjectSettings.ProjectUnits);
         DrawLineAtt(FProfileOrigin,SetPoint(Tmp,FMin3D.Z-space,0),SetPoint(Tmp,FMax3D.Z+space,0),Str,True);
      end;
      // draw grid in aft body view
      Viewport.SetPenWidth(2*PenwidthFactor);
      Space:=CalculateSpace(textspace,FMin3D.Y,FMax3D.Y);
      if ShowMonochrome.Checked then Viewport.FontColor:=clBlack
                                else Viewport.FontColor:=clRed;
      // draw base
      DrawLineAtt(FAftOrigin,SetPoint(-FMax3D.Y-Space,FMin3D.Z,0),SetPoint(FMax3D.Y+Space,FMin3D.Z,0),Userstring(184)+#32+ConvertDimension(FMin3D.Z,Freeship.ProjectSettings.ProjectUnits),False);
      // draw dwl
      DrawLineAtt(FAftOrigin,SetPoint(-FMax3D.Y-Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),SetPoint(FMax3D.Y+Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),Userstring(185)+#32+
                  ConvertDimension(FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,Freeship.ProjectSettings.ProjectUnits),False);
      Space:=CalculateSpace(textspace,FMin3D.Z,FMax3D.Z);
      DrawLineAtt(FAftOrigin,SetPoint(0.0,FMin3D.Z-space,0),SetPoint(0.0,FMax3D.Z+space,0),Userstring(245),True);
      Space:=CalculateSpace(textspace,FMin3D.Y,FMax3D.Y);
      Viewport.FontColor:=clBlack;
      Viewport.SetPenWidth(PenwidthFactor);
      for I:=1 to FFreeship.NumberofWaterlines do
      begin
         Tmp:=-FFreeship.Waterline[I-1].Plane.D;
         Str:=ConvertDimension(Tmp,Freeship.ProjectSettings.ProjectUnits);
         DrawLineAtt(FAftOrigin,SetPoint(-FMax3D.Y-Space,Tmp,0),SetPoint(FMax3D.Y+Space,Tmp,0),Str,False);
      end;
      Space:=CalculateSpace(textspace,FMin3D.Z,FMax3D.Z);
      for I:=1 to FFreeship.NumberofButtocks do
      begin
         Tmp:=-FFreeship.Buttock[I-1].Plane.D;
         Str:=ConvertDimension(Tmp,Freeship.ProjectSettings.ProjectUnits);
         DrawLineAtt(FAftOrigin,SetPoint(Tmp,FMin3D.Z-space,0),SetPoint(Tmp,FMax3D.Z+space,0),Str,True);
         DrawLineAtt(FAftOrigin,SetPoint(-Tmp,FMin3D.Z-space,0),SetPoint(-Tmp,FMax3D.Z+space,0),Str,True);
      end;
      // draw grid in front body view
      Viewport.SetPenWidth(2*PenwidthFactor);
      Space:=CalculateSpace(textspace,FMin3D.Y,FMax3D.Y);
      if ShowMonochrome.Checked then Viewport.FontColor:=clBlack
                                else Viewport.FontColor:=clRed;
      // draw baseline
      DrawLineAtt(FFrontOrigin,SetPoint(-FMax3D.Y-Space,FMin3D.Z,0),SetPoint(FMax3D.Y+Space,FMin3D.Z,0),Userstring(184)+#32+
                  ConvertDimension(FMin3D.Z,Freeship.ProjectSettings.ProjectUnits),False);
      // draw dwl
      DrawLineAtt(FFrontOrigin,SetPoint(-FMax3D.Y-Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),SetPoint(FMax3D.Y+Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,0),Userstring(185)+#32+
                  ConvertDimension(FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft,Freeship.ProjectSettings.ProjectUnits),False);
      Space:=CalculateSpace(textspace,FMin3D.Z,FMax3D.Z);
      DrawLineAtt(FFrontOrigin,SetPoint(0.0,FMin3D.Z-space,0),SetPoint(0.0,FMax3D.Z+space,0),Userstring(245),True);
      Space:=CalculateSpace(textspace,FMin3D.Y,FMax3D.Y);
      Viewport.FontColor:=clBlack;
      Viewport.SetPenWidth(PenwidthFactor);
      for I:=1 to FFreeship.NumberofWaterlines do
      begin
         Tmp:=-FFreeship.Waterline[I-1].Plane.D;
         Str:=ConvertDimension(Tmp,Freeship.ProjectSettings.ProjectUnits);
         DrawLineAtt(FFrontOrigin,SetPoint(-FMax3D.Y-Space,Tmp,0),SetPoint(FMax3D.Y+Space,Tmp,0),Str,False);
      end;
      Space:=CalculateSpace(textspace,FMin3D.Z,FMax3D.Z);
      for I:=1 to FFreeship.NumberofButtocks do
      begin
         Tmp:=-FFreeship.Buttock[I-1].Plane.D;
         Str:=ConvertDimension(Tmp,Freeship.ProjectSettings.ProjectUnits);
         DrawLineAtt(FFrontOrigin,SetPoint(Tmp,FMin3D.Z-space,0),SetPoint(Tmp,FMax3D.Z+space,0),Str,True);
         DrawLineAtt(FFrontOrigin,SetPoint(-Tmp,FMin3D.Z-space,0),SetPoint(-Tmp,FMax3D.Z+space,0),Str,True);
      end;
      // draw grid in plan view
      Viewport.SetPenWidth(2*PenwidthFactor);
      Space:=CalculateSpace(0.2*textspace,FMin3D.X,FMax3D.X);
      if ShowMonochrome.Checked then Viewport.FontColor:=clBlack
                                else Viewport.FontColor:=clRed;
      DrawLineAtt(FPlanOrigin,SetPoint(FMin3D.X-Space,0,0),SetPoint(FMax3D.X+Space,0,0),Userstring(245),False);
      Viewport.FontColor:=clBlack;
      Viewport.SetPenWidth(PenwidthFactor);
      for I:=1 to FFreeship.NumberofButtocks do
      begin
         Tmp:=-FFreeship.Buttock[I-1].Plane.D;
         Str:=ConvertDimension(Tmp,Freeship.ProjectSettings.ProjectUnits);
         DrawLineAtt(FPlanOrigin,SetPoint(FMin3D.X-Space,Tmp,0),SetPoint(FMax3D.X+Space,Tmp,0),Str,False);
         if (Freeship.NumberofDiagonals=0) and (MirrorPlanview.Checked) then DrawLineAtt(FPlanOrigin,SetPoint(FMin3D.X-Space,-Tmp,0),SetPoint(FMax3D.X+Space,-Tmp,0),Str,False);
      end;
      Space:=CalculateSpace(textspace,FMin3D.Y,FMax3D.Y);
      for I:=1 to FFreeship.NumberofStations do
      begin
         Tmp:=-FFreeship.Station[I-1].Plane.D;
         Str:=ConvertDimension(Tmp,Freeship.ProjectSettings.ProjectUnits);
         if (Freeship.NumberofDiagonals=0) and (MirrorPlanview.Checked) then DrawLineAtt(FPlanOrigin,SetPoint(Tmp,-FMax3D.Y-space,0),SetPoint(Tmp,FMax3D.Y+space,0),Str,True)
                                           else DrawLineAtt(FPlanOrigin,SetPoint(Tmp,-FDiagonalWidth-space,0),SetPoint(Tmp,FMax3D.Y+space,0),Str,True);
      end;

      // draw knuckle-lines
      //if not ShowFillColor.checked then
      begin
         if ShowFillColor.checked then Viewport.SetPenWidth(1*PenwidthFactor)
                                  else Viewport.SetPenWidth(2*PenwidthFactor);
         Viewport.PenStyle:=psSolid;
         if ShowMonochrome.Checked then Viewport.PenColor:=clBlack
                                   else Viewport.PenColor:=Freeship.Preferences.CreaseColor;
         for I:=1 to FreeShip.NumberOfLayers do
         begin
            Layer:=FreeShip.Layer[I-1];
            if Layer.ShowInLinesplan then for J:=1 to Layer.Count do
            begin
               Face:=layer.Items[J-1];
               for K:=1 to Face.ControlEdgeCount do
               begin
                  Edge:=Face.ControlEdge[K-1];
                  if Edge.Crease then DrawLine(Edge.StartPoint.Coordinate,Edge.EndPoint.Coordinate);
               end;
            end;
         end;
      end;

      Viewport.SetPenWidth(PenwidthFactor);
      Mainframe:=Freeship.ProjectSettings.ProjectMainframeLocation;
      // Draw stations
      if ShowMonochrome.Checked then Viewport.PenColor:=clBlack
                                else Viewport.PenColor:=Freeship.Preferences.StationColor;
      for i:=1 to Freeship.NumberofStations do DrawIntersection(Freeship.Station[I-1],[lvAftBody,lvFrontBody],psSolid);
      // Draw Buttocks
      if ShowMonochrome.Checked then Viewport.PenColor:=clBlack
                                else Viewport.PenColor:=Freeship.Preferences.ButtockColor;
      for i:=1 to Freeship.NumberofButtocks do DrawIntersection(Freeship.Buttock[I-1],[lvProfile],psSolid);
      // Draw Waterlines
      if ShowMonochrome.Checked then Viewport.PenColor:=clBlack
                                else Viewport.PenColor:=Freeship.Preferences.WaterlineColor;
      for i:=1 to Freeship.NumberofWaterlines do DrawIntersection(Freeship.Waterline[I-1],[lvPlan],psSolid);

      // Draw diagonals
      Viewport.PenWidth:=PenwidthFactor;
      for I:=1 to Freeship.NumberofDiagonals do
      begin
         Diagonal:=Freeship.Diagonal[I-1];
         Plane.a:=0.0;
         Plane.b:= 1/Sqrt(2);
         Plane.c:=-1/sqrt(2);
         Plane.d:=-Diagonal.Plane.d;
         if not Diagonal.Build then Diagonal.Rebuild;
         for J:=1 to Diagonal.Count do
         begin
            Viewport.SetPenWidth(1);
            if ShowMonochrome.Checked then Viewport.PenColor:=clBlack
                                      else Viewport.PenColor:=Freeship.Preferences.DiagonalColor;
            Spline:=Diagonal.Items[J-1];
            Setlength(Pts,Steps+1);
            Min:=0;
            Max:=0;
            for k:=0 to steps do
            begin
               P:=Spline.Value(K/steps);
               Tmp:=abs(Plane.A*P.x+Plane.B*P.y+Plane.C*P.z+Plane.D);
               if K=0 then
               begin
                  Min:=Tmp;
                  Max:=Tmp;
               end else
               begin
                  if Tmp<min then Min:=Tmp;
                  if Tmp>max then Max:=Tmp;
               end;
               P.X:=FPlanOrigin.X+P.X;
               P.Y:=FPlanOrigin.Y-abs(Tmp);
               P.Z:=0.0;
               Pts[K]:=Viewport.Project(P);
            end;
            Viewport.Polyline(Pts);
            // draw as grid in bodyplan views
            Viewport.SetPenWidth(PenwidthFactor);
            if ShowMonochrome.Checked then Viewport.PenColor:=clBlack
                                      else Viewport.PenColor:=clSilver;
            // calculate height of intersection of diagonal with centerplane
            Tmp:=-Diagonal.PLane.d/Diagonal.Plane.c;
            P1:=SetPoint(0.0,Min*Sin(DegToRad(45)),Tmp-Min*Sin(DegToRad(45)));
            P2:=SetPoint(0.0,Max*Sin(DegToRad(45)),Tmp-Max*Sin(DegToRad(45)));
            DrawDiagonalLine(FAftOrigin,P1,P2);
            DrawDiagonalLine(FFrontOrigin,P1,P2);
         end;
      end;
   finally
      Screen.Cursor:=Prevcursor;
   end;
end;{TFreeLinesplanFrame.ViewportRedraw}

procedure TFreeLinesplanFrame.ZoomExtentsExecute(Sender: TObject);
begin
   Viewport.ZoomExtents;
end;{TFreeLinesplanFrame.ZoomExtentsExecute}

procedure TFreeLinesplanFrame.ZoomInExecute(Sender: TObject);
begin
   Viewport.ZoomIn;
end;{TFreeLinesplanFrame.ZoomInExecute}

procedure TFreeLinesplanFrame.ZoomOutExecute(Sender: TObject);
begin
   Viewport.ZoomOut;
end;{TFreeLinesplanFrame.ZoomOutExecute}

procedure TFreeLinesplanFrame.ShowFillColorExecute(Sender: TObject);
begin
   ShowFillcolor.Checked:=not ShowFillcolor.Checked;
   UpdateMenu;
   Viewport.Refresh;
end;{TFreeLinesplanFrame.ShowFillColorExecute}

procedure TFreeLinesplanFrame.PrintExecute(Sender: TObject);
begin
   if Viewport.Width>Viewport.Height then Printer.Orientation:=poLandscape
                                     else Printer.Orientation:=poPortrait;
   if PrintDialog.Execute then Viewport.Print(FFreeship.ProjectSettings.ProjectUnits,True,Userstring(246));
end;{TFreeLinesplanFrame.PrintExecute}

procedure TFreeLinesplanFrame.ViewportMouseMove(Sender: TObject;Shift: TShiftState; X, Y: Integer);
var P    : TPoint;
begin
   if ssLeft in Shift then
   begin
      // Zoom in or zoom out
      if abs(FInitialPosition.Y-Y)>4 then
      begin
         if Y<FInitialPosition.Y then Viewport.ZoomIn else
            if Y>FInitialPosition.Y then Viewport.ZoomOut;
         FInitialPosition.X:=X;
         FInitialPosition.Y:=Y;
      end;
   end else if ssRight in Shift then
   begin
      // Pan the window left, right, top or bottom
      if (abs(FInitialPosition.X-X)>4) or (abs(FInitialPosition.Y-Y)>4)  then
      begin
         P.X:=Viewport.Pan.X+X-FInitialPosition.X;
         P.Y:=Viewport.Pan.Y+Y-FInitialPosition.Y;
         Viewport.Pan:=P;
         FInitialPosition.X:=X;
         FInitialPosition.Y:=Y;
      end;
   end;
end;{TFreeLinesplanFrame.ViewportMouseMove}

procedure TFreeLinesplanFrame.ViewportMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   FInitialPosition.X:=X;
   FInitialPosition.Y:=Y;
end;{TFreeLinesplanFrame.ViewportMouseDown}

procedure TFreeLinesplanFrame.SaveBitmapExecute(Sender: TObject);
var Str        : string;
    
begin
   Str:=FFreeShip.Preferences.ExportDirectory;
   if Str[Length(Str)]<>'\' then Str:=Str+'\';
   Str:=Str+ChangeFileExt(ExtractFilename(FFreeship.FileName),'')+'_Linesplan.bmp';
   Viewport.SaveAsBitmap(Str);
end;{TFreeLinesplanFrame.SaveBitmapExecute}

procedure TFreeLinesplanFrame.UseLightsExecute(Sender: TObject);
begin
   UseLights.Checked:=not UseLights.Checked;
   UpdateMenu;
   Viewport.Refresh;
end;{TFreeLinesplanFrame.UseLightsExecute}

procedure TFreeLinesplanFrame.ShowMonochromeExecute(Sender: TObject);
begin
   ShowMonochrome.Checked:=not ShowMonochrome.Checked;
   UpdateMenu;
   Viewport.Refresh;
end;{TFreeLinesplanFrame.ShowMonochromeExecute}

procedure TFreeLinesplanFrame.MirrorPlanViewExecute(Sender: TObject);
begin
   MirrorPlanview.Checked:=not MirrorPlanview.Checked;
   UpdateMenu;
   Viewport.ZoomExtents;
end;{TFreeLinesplanFrame.MirrorPlanViewExecute}

procedure TFreeLinesplanFrame.ExportDXFExecute(Sender: TObject);
var SaveDialog : TSaveDialog;
    Strings    : TStringList;
    Str        : string;
    Space,Tmp  : TFloatType;
    Mainframe  : TFloatType;
    Min,Max    : TFloatType;
    I,J,K      : Integer;
    Edge       : TFreeSubdivisionEdge;
    P1,P2      : T3DCoordinate;
    Diagonal   : TFreeIntersection;
    Plane      : T3DPlane;
    Spline     : TFreeSpline;
    Edges      : TFasterList;
    Points     : TFasterList;

   procedure WriteDXFPoint(N:integer;P:T3DCoordinate);
   begin
      Strings.Add(IntToStr(N)+EOL+Truncate(P.X,6));
      Strings.Add(IntToStr(N+10)+EOL+Truncate(P.Y,6));
   end;{WriteDXFPoint}

    procedure AddLine(P1,P2:T3DCoordinate;Views:TLinesplanViews;Layername:string;Color:TColor);
    var Startp,Endp:T3DCoordinate;
    begin
      if lvProfile in views then
      begin
         StartP.X:=FProfileOrigin.X+P1.X;
         StartP.Y:=FProfileOrigin.Y+P1.Z;
         StartP.Z:=0.0;
         EndP.X:=FProfileOrigin.X+P2.X;
         EndP.Y:=FProfileOrigin.Y+P2.Z;
         EndP.Z:=0.0;
      end;
      if lvAftBody in views then
      begin
         StartP.X:=FAftOrigin.X-P1.Y;
         StartP.Y:=FAftOrigin.Y+P1.Z;
         StartP.Z:=0.0;
         EndP.X:=FAftOrigin.X-P2.Y;
         EndP.Y:=FAftOrigin.Y+P2.Z;
         EndP.Z:=0.0;
      end;
      if lvFrontBody in views then
      begin
         StartP.X:=FFrontOrigin.X-P1.Y;
         StartP.Y:=FFrontOrigin.Y+P1.Z;
         StartP.Z:=0.0;
         EndP.X:=FFrontOrigin.X-P2.Y;
         EndP.Y:=FFrontOrigin.Y+P2.Z;
         EndP.Z:=0.0;
      end;
      if lvPlan in views then
      begin
         StartP.X:=FPlanOrigin.X+P1.X;
         StartP.Y:=FPlanOrigin.Y+P1.Y;
         StartP.Z:=0.0;
         EndP.X:=FPlanOrigin.X+P2.X;
         EndP.Y:=FPlanOrigin.Y+P2.Y;
         EndP.Z:=0.0;
      end;
      Strings.Add('0'+EOL+'LINE');
      Strings.Add('8'+EOL+LayerName);
      Strings.Add('62'+EOL+IntToStr(FindDXFColorIndex(Color)));
      WriteDXFPoint(10,StartP);
      WriteDXFPoint(11,EndP);
    end;{AddLine}

    procedure AddSpline(Spline:TFreeSpline;Views:TLinesplanViews;Layername:string;Color:TColor);
    var Points : array of T3DCoordinate;
        P      : T3DCoordinate;
        I      : Integer;
        Params : TFloatArray;
        NParams: Integer;
    begin
      NParams:=0;
      Setlength(Params,Spline.NumberOfPoints);
      // count number of knucklepoints
      if not Spline.Build then Spline.Rebuild;
      for I:=2 to Spline.NumberOfPoints-1 do
      begin
         if Spline.Knuckle[I-1] then
         begin
            Params[NParams]:=Spline.Parameter[I-1];
            inc(NParams);
         end;
      end;
      Spline.Fragments:=100;
      Setlength(Params,NParams+Spline.Fragments);
      for I:=1 to Spline.Fragments do
      begin
         Params[NParams]:=(I-1)/(Spline.Fragments-1);
         inc(NParams);
      end;
      SortFloatArray(Params,NParams);

      Setlength(Points,NParams);
      for I:=0 to NParams-1 do Points[I]:=Spline.Value(Params[I]);
      Strings.Add('0'+EOL+'POLYLINE');
      Strings.Add('8'+EOL+LayerName);   // layername
      Strings.Add('62'+EOL+IntToStr(FindDXFColorIndex(Color)));
      Strings.Add('66'+EOL+'1');    // vertices follow
      for I:=0 to NParams-1 do
      begin
         P.Z:=0.0;
         if lvProfile in views then
         begin
            P.X:=FProfileOrigin.X+Points[I].X;
            P.Y:=FProfileOrigin.Y+Points[I].Z;
         end;
         if lvAftBody in views then
         begin
            P.X:=FAftOrigin.X-Points[I].Y;
            P.Y:=FAftOrigin.Y+Points[I].Z;
         end;
         if lvFrontBody in views then
         begin
            P.X:=FFrontOrigin.X-Points[I].Y;
            P.Y:=FFrontOrigin.Y+Points[I].Z;
         end;
         if lvPlan in views then
         begin
            P.X:=FPlanOrigin.X+Points[I].X;
            P.Y:=FPlanOrigin.Y+Points[I].Y;
         end;
         Strings.Add('0'+EOL+'VERTEX');
         Strings.Add('8'+EOL+LayerName);
         Strings.Add('10'+EOL+Truncate(P.X,4));
         Strings.Add('20'+EOL+Truncate(P.Y,4));
      end;
      Strings.Add('0'+EOL+'SEQEND');

      if (lvAftBody in views) or (lvFrontBody in views) or ((lvPlan in views) and (Freeship.NumberofDiagonals=0) and (MirrorPlanView.Checked))then
      begin
         // mirror line
         Strings.Add('0'+EOL+'POLYLINE');
         Strings.Add('8'+EOL+LayerName);   // layername
         Strings.Add('62'+EOL+IntToStr(FindDXFColorIndex(Color)));
         Strings.Add('66'+EOL+'1');    // vertices follow
         for I:=0 to NParams-1 do
         begin
            P.Z:=0.0;
            if lvAftBody in views then
            begin
               P.X:=FAftOrigin.X+Points[I].Y;
               P.Y:=FAftOrigin.Y+Points[I].Z;
            end;
            if lvFrontBody in views then
            begin
               P.X:=FFrontOrigin.X+Points[I].Y;
               P.Y:=FFrontOrigin.Y+Points[I].Z;
            end;
            if lvPlan in views then
            begin
               P.X:=FPlanOrigin.X+Points[I].X;
               P.Y:=FPlanOrigin.Y-Points[I].Y;
            end;

            Strings.Add('0'+EOL+'VERTEX');
            Strings.Add('8'+EOL+LayerName);
            Strings.Add('10'+EOL+Truncate(P.X,4));
            Strings.Add('20'+EOL+Truncate(P.Y,4));
         end;
         Strings.Add('0'+EOL+'SEQEND');
      end;
    end;{AddSpline}

    procedure AddEdgeLoop(Points:TFasterList;Views:TLinesplanViews;Layername:string;Color:TColor);
    var Point  : TFreeSubdivisionPoint;
        P      : T3DCoordinate;
        I      : Integer;
    begin
      Strings.Add('0'+EOL+'POLYLINE');
      Strings.Add('8'+EOL+LayerName);   // layername
      Strings.Add('62'+EOL+IntToStr(FindDXFColorIndex(Color)));
      Strings.Add('66'+EOL+'1');    // vertices follow
      for I:=1 to Points.Count do
      begin
         Point:=Points[I-1];
         P.Z:=0.0;
         if lvProfile in views then
         begin
            P.X:=FProfileOrigin.X+Point.Coordinate.X;
            P.Y:=FProfileOrigin.Y+Point.Coordinate.Z;
         end;
         if lvAftBody in views then
         begin
            P.X:=FAftOrigin.X-Point.Coordinate.Y;
            P.Y:=FAftOrigin.Y+Point.Coordinate.Z;
         end;
         if lvFrontBody in views then
         begin
            P.X:=FFrontOrigin.X-Point.Coordinate.Y;
            P.Y:=FFrontOrigin.Y+Point.Coordinate.Z;
         end;
         if lvPlan in views then
         begin
            P.X:=FPlanOrigin.X+Point.Coordinate.X;
            P.Y:=FPlanOrigin.Y+Point.Coordinate.Y;
         end;
         Strings.Add('0'+EOL+'VERTEX');
         Strings.Add('8'+EOL+LayerName);
         Strings.Add('10'+EOL+Truncate(P.X,4));
         Strings.Add('20'+EOL+Truncate(P.Y,4));
      end;
      Strings.Add('0'+EOL+'SEQEND');

      if (lvAftBody in views) or (lvFrontBody in views) or ((lvPlan in views) and (Freeship.NumberofDiagonals=0) and (MirrorPlanView.Checked))then
      begin
         // mirror line
         Strings.Add('0'+EOL+'POLYLINE');
         Strings.Add('8'+EOL+LayerName);   // layername
         Strings.Add('62'+EOL+IntToStr(FindDXFColorIndex(Color)));
         Strings.Add('66'+EOL+'1');    // vertices follow
         for I:=1 to Points.Count do
         begin
            Point:=Points[I-1];
            P.Z:=0.0;
            if lvAftBody in views then
            begin
               P.X:=FAftOrigin.X+Point.Coordinate.Y;
               P.Y:=FAftOrigin.Y+Point.Coordinate.Z;
            end;
            if lvFrontBody in views then
            begin
               P.X:=FFrontOrigin.X+Point.Coordinate.Y;
               P.Y:=FFrontOrigin.Y+Point.Coordinate.Z;
            end;
            if lvPlan in views then
            begin
               P.X:=FPlanOrigin.X+Point.Coordinate.X;
               P.Y:=FPlanOrigin.Y-Point.Coordinate.Y;
            end;

            Strings.Add('0'+EOL+'VERTEX');
            Strings.Add('8'+EOL+LayerName);
            Strings.Add('10'+EOL+Truncate(P.X,4));
            Strings.Add('20'+EOL+Truncate(P.Y,4));
         end;
         Strings.Add('0'+EOL+'SEQEND');
      end;
    end;{AddSpline}

    procedure AddIntersection(Intersection:TFreeIntersection;Views:TLinesplanViews;Layername:string;Color:TColor);
    var I:Integer;
    begin
       if not Intersection.Build then Intersection.Rebuild;
       for I:=1 to Intersection.Count do AddSpline(Intersection.Items[I-1],Views,Layername,Color);
    end;{AddIntersection}
begin
   Str:=FFreeShip.Preferences.ExportDirectory;
   if Str[Length(Str)]<>'\' then Str:=Str+'\';
   Str:=Str+ChangeFileExt(ExtractFilename(FFreeship.FileName),'')+'_Linesplan';
   SaveDialog:=TSaveDialog.Create(Owner);
   SaveDialog.InitialDir:=Freeship.Preferences.ExportDirectory;
   SaveDialog.FileName:=ChangeFileExt(Str,'');
   SaveDialog.Filter:='Autocad dxf file (*.dxf)|*.dxf';
   Savedialog.Options:=[ofOverwritePrompt,ofHideReadOnly];
   if SaveDialog.Execute then
   begin
      Freeship.Preferences.ExportDirectory:=ExtractFilePath(SaveDialog.FileName);
      Mainframe:=Freeship.ProjectSettings.ProjectMainframeLocation;

      Edges:=TFasterList.Create;
      Freeship.Surface.ExtractAllEdgeLoops(Edges);
      Strings:=TStringlist.Create;
      Strings.Add('0'+EOL+'SECTION');
      Strings.Add('2'+EOL+'ENTITIES');
      // PROFILE VIEW
      Space:=CalculateSpace(0.5*textspace,FMin3D.X,FMax3D.X);
      // draw baseline
      AddLine(SetPoint(FMin3D.X-Space,FMin3D.Z,0),SetPoint(FMax3D.X+Space,FMin3D.Z,0),[lvProfile],Userstring(184),Freeship.Preferences.GridColor);
      // draw dwl
      AddLine(SetPoint(FMin3D.X-Space,0,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft),SetPoint(FMax3D.X+Space,0,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft),[lvProfile],Userstring(185),clRed);
      for I:=1 to FFreeship.NumberofWaterlines do
      begin
         Tmp:=-FFreeship.Waterline[I-1].Plane.D;
         AddLine(SetPoint(FMin3D.X-Space,0,Tmp),SetPoint(FMax3D.X+Space,0,Tmp),[lvProfile],'wlgrid',Freeship.Preferences.GridColor);
      end;
      Space:=CalculateSpace(textspace,FMin3D.Z,FMax3D.Z);
      for I:=1 to FFreeship.NumberofStations do
      begin
         Tmp:=-FFreeship.Station[I-1].Plane.D;
         AddLine(SetPoint(Tmp,0,FMin3D.Z-space),SetPoint(Tmp,0,FMax3D.Z+space),[lvProfile],'stationgrid',Freeship.Preferences.GridColor);
      end;
      // draw buttocks
      for I:=1 to Freeship.NumberofButtocks do AddIntersection(Freeship.Buttock[I-1],[lvProfile],'buttocks',Freeship.Preferences.ButtockColor);
      // Add knuckle lines
      for I:=1 to Edges.Count do
      begin
         Points:=Edges[I-1];
         AddEdgeLoop(Points,[lvProfile],'Knuckle_lines',Freeship.Preferences.CreaseColor);
      end;

      // AFT VIEW OF BODYPLAN
      Space:=CalculateSpace(0.5*textspace,-FMax3D.Y,FMax3D.Y);
      // draw baseline
      AddLine(SetPoint(0.0,-FMax3D.Y-Space,0.0),SetPoint(0.0,FMax3D.Y+Space,0.0),[lvAftBody],Userstring(184),Freeship.Preferences.GridColor);
      // draw dwl
      AddLine(SetPoint(0.0,-FMax3D.Y-Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft),SetPoint(0.0,FMax3D.Y+Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft),[lvAftBody],Userstring(185),clRed);
      for I:=1 to FFreeship.NumberofWaterlines do
      begin
         Tmp:=-FFreeship.Waterline[I-1].Plane.D;
         AddLine(SetPoint(0.0,-FMax3D.Y-Space,Tmp),SetPoint(0.0,FMax3D.Y+Space,Tmp),[lvAftBody],'wlgrid',Freeship.Preferences.GridColor);
      end;
      Space:=CalculateSpace(textspace,FMin3D.Z,FMax3D.Z);
      // draw centerline
      AddLine(SetPoint(0.0,0.0,FMin3D.Z-space),SetPoint(0.0,0.0,FMax3D.Z+space),[lvAftBody],Userstring(245),clRed);
      for I:=1 to FFreeship.NumberofButtocks do
      begin
         Tmp:=-FFreeship.Buttock[I-1].Plane.D;
         AddLine(SetPoint(0.0,Tmp,FMin3D.Z-space),SetPoint(0.0,Tmp,FMax3D.Z+space),[lvAftBody],'buttockgrid',Freeship.Preferences.GridColor);
         AddLine(SetPoint(0.0,-Tmp,FMin3D.Z-space),SetPoint(0.0,-Tmp,FMax3D.Z+space),[lvAftBody],'buttockgrid',Freeship.Preferences.GridColor);
      end;
      // draw stations
      for I:=1 to Freeship.NumberofStations do if -Freeship.Station[I-1].Plane.d<=Mainframe then AddIntersection(Freeship.Station[I-1],[lvAftBody],'stations',Freeship.Preferences.StationColor);
      // Add knuckle lines
      for I:=1 to FreeShip.Surface.NumberOfEdges do
      begin
         Edge:=FreeShip.Surface.Edge[I-1];
         if (Edge.Crease) and (Edge.StartPoint.Coordinate.X<=Mainframe) and (Edge.EndPoint.Coordinate.X<=Mainframe) then
         begin
            P1:=Edge.StartPoint.Coordinate;
            P2:=Edge.EndPoint.Coordinate;
            AddLine(P1,P2,[lvAftBody],'Knuckle_lines',Freeship.Preferences.CreaseColor);
            P1.Y:=-P1.Y;
            P2.Y:=-P2.Y;
            AddLine(P1,P2,[lvAftBody],'Knuckle_lines',Freeship.Preferences.CreaseColor);
         end;
      end;

      // FRONT VIEW OF BODYPLAN
      Space:=CalculateSpace(0.5*textspace,-FMax3D.Y,FMax3D.Y);
      // draw baseline
      AddLine(SetPoint(0.0,-FMax3D.Y-Space,0.0),SetPoint(0.0,FMax3D.Y+Space,0.0),[lvFrontBody],Userstring(184),Freeship.Preferences.GridColor);
      // draw dwl
      AddLine(SetPoint(0.0,-FMax3D.Y-Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft),SetPoint(0.0,FMax3D.Y+Space,FMin3D.Z+FFreeship.ProjectSettings.ProjectDraft),[lvFrontBody],Userstring(185),clRed);
      for I:=1 to FFreeship.NumberofWaterlines do
      begin
         Tmp:=-FFreeship.Waterline[I-1].Plane.D;
         AddLine(SetPoint(0.0,-FMax3D.Y-Space,Tmp),SetPoint(0.0,FMax3D.Y+Space,Tmp),[lvFrontBody],'wlgrid',Freeship.Preferences.GridColor);
      end;
      Space:=CalculateSpace(textspace,FMin3D.Z,FMax3D.Z);
      // draw centerline
      AddLine(SetPoint(0.0,0.0,FMin3D.Z-space),SetPoint(0.0,0.0,FMax3D.Z+space),[lvFrontBody],Userstring(245),clRed);
      for I:=1 to FFreeship.NumberofButtocks do
      begin
         Tmp:=-FFreeship.Buttock[I-1].Plane.D;
         AddLine(SetPoint(0.0,Tmp,FMin3D.Z-space),SetPoint(0.0,Tmp,FMax3D.Z+space),[lvFrontBody],'buttockgrid',Freeship.Preferences.GridColor);
         AddLine(SetPoint(0.0,-Tmp,FMin3D.Z-space),SetPoint(0.0,-Tmp,FMax3D.Z+space),[lvFrontBody],'buttockgrid',Freeship.Preferences.GridColor);
      end;
      // draw stations
      for I:=1 to Freeship.NumberofStations do if -Freeship.Station[I-1].Plane.d>=Mainframe then AddIntersection(Freeship.Station[I-1],[lvFrontBody],'stations',Freeship.Preferences.StationColor);
      // Add knuckle lines
      for I:=1 to FreeShip.Surface.NumberOfEdges do
      begin
         Edge:=FreeShip.Surface.Edge[I-1];
         if (Edge.Crease) and (Edge.StartPoint.Coordinate.X>=Mainframe) and (Edge.EndPoint.Coordinate.X>=Mainframe) then
         begin
            P1:=Edge.StartPoint.Coordinate;
            P2:=Edge.EndPoint.Coordinate;
            AddLine(P1,P2,[lvFrontBody],'Knuckle_lines',Freeship.Preferences.CreaseColor);
            P1.Y:=-P1.Y;
            P2.Y:=-P2.Y;
            AddLine(P1,P2,[lvFrontBody],'Knuckle_lines',Freeship.Preferences.CreaseColor);
         end;
      end;
      // PLAN VIEW
      Space:=CalculateSpace(0.5*textspace,FMin3D.X,FMax3D.X);
      // draw centerline
      AddLine(SetPoint(FMin3D.X-Space,0.0,0.0),SetPoint(FMax3D.X+Space,0.0,0.0),[lvPlan],Userstring(245),clRed);
      for I:=1 to FFreeship.NumberofButtocks do
      begin
         Tmp:=-FFreeship.Buttock[I-1].Plane.D;
         AddLine(SetPoint(FMin3D.X-Space,Tmp,0.0),SetPoint(FMax3D.X+space,Tmp,0.0),[lvPlan],'buttockgrid',Freeship.Preferences.GridColor);
         if (Freeship.NumberofDiagonals=0) and (MirrorPlanView.Checked) then AddLine(SetPoint(FMin3D.X-space,-Tmp,0.0),SetPoint(FMax3D.X+space,-Tmp,0.0),[lvPlan],'buttockgrid',Freeship.Preferences.GridColor);
      end;
      // stations
      Space:=CalculateSpace(textspace,FMin3D.Y,FMax3D.Y);
      for I:=1 to FFreeship.NumberofStations do
      begin
         Tmp:=-FFreeship.Station[I-1].Plane.D;
         if (Freeship.NumberofDiagonals=0) and (MirrorPlanview.Checked) then AddLine(SetPoint(Tmp,-FMax3D.Y-space,0),SetPoint(Tmp,FMax3D.Y+space,0),[lvPlan],'stationgrid',Freeship.Preferences.GridColor)
                                                                        else AddLine(SetPoint(Tmp,-FDiagonalWidth-space,0),SetPoint(Tmp,FMax3D.Y+space,0),[lvPlan],'stationgrid',Freeship.Preferences.GridColor);
      end;
      // draw waterlines
      for I:=1 to Freeship.NumberofWaterlines do AddIntersection(Freeship.Waterline[I-1],[lvPLan],'waterlines',Freeship.Preferences.WaterlineColor);

      // draw diagonals
      for I:=1 to Freeship.NumberofDiagonals do
      begin
         Diagonal:=Freeship.Diagonal[I-1];
         Plane.a:=0.0;
         Plane.b:= 1/Sqrt(2);
         Plane.c:=-1/sqrt(2);
         Plane.d:=-Diagonal.Plane.d;
         if not Diagonal.Build then Diagonal.Rebuild;
         for J:=1 to Diagonal.Count do
         begin
            Spline:=Diagonal.Items[J-1];
            Min:=0;
            Max:=0;
            Strings.Add('0'+EOL+'POLYLINE');
            Strings.Add('8'+EOL+'Diagonals');   // layername
            Strings.Add('62'+EOL+IntToStr(FindDXFColorIndex(Freeship.Preferences.DiagonalColor)));
            Strings.Add('66'+EOL+'1');    // vertices follow

            for k:=0 to 100 do
            begin
               P1:=Spline.Value(K/100);
               Tmp:=abs(Plane.A*P1.x+Plane.B*P1.y+Plane.C*P1.z+Plane.D);
               if K=0 then
               begin
                  Min:=Tmp;
                  Max:=Tmp;
               end else
               begin
                  if Tmp<min then Min:=Tmp;
                  if Tmp>max then Max:=Tmp;
               end;
               P2.X:=FPlanOrigin.X+P1.X;
               P2.Y:=FPlanOrigin.Y-abs(Tmp);
               P2.Z:=0.0;
               Strings.Add('0'+EOL+'VERTEX');
               Strings.Add('8'+EOL+'Diagonals');
               Strings.Add('10'+EOL+Truncate(P2.X,4));
               Strings.Add('20'+EOL+Truncate(P2.Y,4));
            end;
            Strings.Add('0'+EOL+'SEQEND');
            // draw as grid in bodyplan views
            // calculate height of intersection of diagonal with centerplane
            Tmp:=-Diagonal.Plane.d/Diagonal.Plane.c;
            P1:=SetPoint(0.0,Min*Sin(DegToRad(45)),Tmp-Min*Sin(DegToRad(45)));
            P2:=SetPoint(0.0,Max*Sin(DegToRad(45)),Tmp-Max*Sin(DegToRad(45)));
            AddLine(P1,P2,[lvAftBody],'diagonalgrid',freeship.Preferences.GridColor);
            AddLine(P1,P2,[lvFrontBody],'diagonalgrid',freeship.Preferences.GridColor);
            P1.Y:=-P1.Y;
            P2.Y:=-P2.Y;
            AddLine(P1,P2,[lvAftBody],'diagonalgrid',freeship.Preferences.GridColor);
            AddLine(P1,P2,[lvFrontBody],'diagonalgrid',freeship.Preferences.GridColor);
         end;
      end;
      // Add knuckle lines
      for I:=1 to Edges.Count do
      begin
         Points:=Edges[I-1];
         AddEdgeLoop(Points,[lvPlan],'Knuckle_lines',Freeship.Preferences.CreaseColor);
      end;

      // Destroy extracted edgeloops
      for I:=1 to Edges.Count do
      begin
         Points:=Edges[I-1];
         Points.Destroy;
      end;
      Edges.Destroy;

      Strings.Add('0'+EOL+'ENDSEC');
      Strings.Add('0'+EOL+'EOF');
      Strings.SaveToFile(ChangeFileExt(SaveDialog.FileName,'.dxf'));
      Strings.Destroy;
   end;
   SaveDialog.Destroy;
end;

end.
