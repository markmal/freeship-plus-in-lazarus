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

unit freehullformwindow_form;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$DEFINE NOTUSEOPENGL}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
  PrintersDlgs,
{$ENDIF}
     SysUtils,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     FreeTypes,
     FreeGeometry,
     FreeShipUnit,
     StdCtrls,
     Menus,
     ActnList,
     Printers,
     LightDialog
{$IFDEF USEOPENGL}
    ,FreeViewPortOpenGL
{$ENDIF}
;

type

{ TFreeHullForm }

 TFreeHullForm   = class(TForm)
                          SetLight: TAction;
                          Light: TMenuItem;
                          olerance1: TMenuItem;
                           ScrollBar1: TScrollBar;
                           ScrollBar2: TScrollBar;
                           Viewport  : TFreeViewport;
                           PopupMenuHull: TPopupMenu;
                           ActionListHull: TActionList;
                           StandardLens: TAction;
                           WideLens: TAction;
                           Camera1: TMenuItem;
                           Widelens28mm1: TMenuItem;
                           Standard50mm1: TMenuItem;
                           ShortTeleLens: TAction;
                           Shorttelelens90mm1: TMenuItem;
                           MediumTeleLens: TAction;
                           Mediumtelelens130mm1: TMenuItem;
                           LongTeleLens: TAction;
                           Longtelelens200mm1: TMenuItem;
                           View1: TMenuItem;
                           Bodyplan1: TMenuItem;
                           Profile1: TMenuItem;
                           Planview1: TMenuItem;
                           Perspective1: TMenuItem;
                           ViewBodyPlan: TAction;
                           ViewProfile: TAction;
                           ViewPlan: TAction;
                           ViewPerspective: TAction;
                           ZoomIn: TAction;
                           Zoom1: TMenuItem;
                           ZoomIn1: TMenuItem;
                           ZoomExtents: TAction;
                           ZoomOut: TAction;
                           Zoomout1: TMenuItem;
                           All1: TMenuItem;
                           DeselectAll: TAction;
                           Deselectall1: TMenuItem;
                           ImagesHull: TImageList;
                           Print: TAction;
                           ShowWireFrame: TAction;
                           Mode1: TMenuItem;
                           Wireframe1: TMenuItem;
                           ShowFlatShade: TAction;
                           Shade1: TMenuItem;
                           ShowGaussCurvature: TAction;
                           Gausscurvature1: TMenuItem;
                           ShowDevelopablity: TAction;
                           Developablitycheck1: TMenuItem;
                           Print1: TMenuItem;
                           PrintDialogHull: TPrintDialog;
                           SaveAsBitmap: TAction;
                           Saveimage1: TMenuItem;
                           ShadeZebra: TAction;
                           Zebrashading1: TMenuItem;
                           ImportBackGround: TAction;
                           Backgroundimage1: TMenuItem;
                           BackgroundOrigin: TAction;
                           Backgroundimage2: TMenuItem;
                           Origin1: TMenuItem;
                           BackgroundScale: TAction;
                           Setscale1: TMenuItem;
                           BackgroundTransparentColor: TAction;
                           ransparentcolor1: TMenuItem;
                           Backgroundclear: TAction;
                           Clear1: TMenuItem;
                           BackgroundBlending: TAction;
                           Blending1: TMenuItem;
                           BackgroundExport: TAction;
                           Export1: TMenuItem;
                           BackgroundTolerance: TAction;
                           Tolerance1: TMenuItem;
                           BackgroundVisible: TAction;
                           Visible1: TMenuItem;
                           procedure FormDestroy(Sender: TObject);
                           procedure FormKeyPress(Sender: TObject; var Key: char
                             );
                           procedure FormKeyUp(Sender: TObject; var Key: Word;
                             Shift: TShiftState);
                           procedure SetLightExecute(Sender: TObject);
                           procedure ViewportRequestExtents(Sender: TObject; var Min,Max: T3DCoordinate);
                           procedure ViewportRedraw(Sender: TObject);
                           procedure FormCreate(Sender: TObject);
                           procedure FormClose(Sender: TObject; var Action: TCloseAction);
                           procedure PopupMenuHullPopup(Sender: TObject);
                           procedure StandardLensExecute(Sender: TObject);
                           procedure WideLensExecute(Sender: TObject);
                           procedure ShortTeleLensExecute(Sender: TObject);
                           procedure MediumTeleLensExecute(Sender: TObject);
                           procedure LongTeleLensExecute(Sender: TObject);
                           procedure ViewBodyPlanExecute(Sender: TObject);
                           procedure ViewProfileExecute(Sender: TObject);
                           procedure ViewPlanExecute(Sender: TObject);
                           procedure ViewPerspectiveExecute(Sender: TObject);
                           procedure FormShow(Sender: TObject);
                           procedure ViewportChangeViewType(Sender: TObject);
                           procedure ZoomInExecute(Sender: TObject);
                           procedure ZoomExtentsExecute(Sender: TObject);
                           procedure ZoomOutExecute(Sender: TObject);
                           procedure ViewportMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
                           procedure ViewportMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
                           procedure ViewportMouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
                           procedure DeselectAllExecute(Sender: TObject);
                           procedure ViewportMouseLeave(Sender: TObject);
                           procedure PrintExecute(Sender: TObject);
                           procedure ShowWireFrameExecute(Sender: TObject);
                           procedure ShowFlatShadeExecute(Sender: TObject);
                           procedure ShowGaussCurvatureExecute(Sender: TObject);
                           procedure ShowDevelopablityExecute(Sender: TObject);
                           procedure SaveAsBitmapExecute(Sender: TObject);
                           procedure ViewportKeyUp(Sender: TObject; var Key: Word;Shift: TShiftState);
                           procedure ViewportKeyPress(Sender: TObject; var Key: Char);
                           procedure ShadeZebraExecute(Sender: TObject);
                           procedure ImportBackGroundExecute(Sender: TObject);
                           procedure BackgroundOriginExecute(Sender: TObject);
                           procedure BackgroundScaleExecute(Sender: TObject);
                           procedure BackgroundTransparentColorExecute(Sender: TObject);
                           procedure BackgroundclearExecute(Sender: TObject);
                           procedure BackgroundBlendingExecute(Sender: TObject);
                           procedure ViewportRequestBackgroundImage(Sender: TObject);
                           procedure ViewportChangeBackground(Sender: TObject);
                           procedure BackgroundExportExecute(Sender: TObject);
                           procedure BackgroundToleranceExecute(Sender: TObject);
                           procedure BackgroundVisibleExecute(Sender: TObject);
                        private    { Private declarations }
                           FFreeShip         : TFreeShip;
                           FPanned           : Boolean;  // Private variable from which can be seen if the popup menu has to be shown or not
                           FInitialPosition  : TPoint;   // Initial position of the mouse cursor when the left or right button was pressed
                           FAllowPanOrZoom   : Boolean;  // Flag to check whether panning or zooming is allowed or not (not when an item has just been selected)
                           procedure FSetFreeShip(Val:TFreeShip);
                           function FCaptionText:string;
                           procedure createFreeViewport();
                        public     { Public declarations }
                           LightDialog:TLightDialog;
                           procedure SetCaption;
                           procedure UpdateMenu;
                           property FreeShip:TFreeShip read FFreeShip write FSetFreeShip;
  end;

//var FreeHullWindow: TFreeHullForm;

implementation

uses FreeLanguageSupport,
     Main;

{$IFnDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeHullForm.FCaptionText:string;
begin
   Case Viewport.ViewType of
      fvBodyplan     : Result:=Userstring(215)+'.';
      fvProfile      : Result:=Userstring(216)+'.';
      fvPlan         : Result:=Userstring(217)+'.';
      fvPerspective  : Result:=Userstring(218)+'.';
      else Result:='';
   end;
end;{TFreeHullForm.FCaptionText}

procedure TFreeHullForm.SetCaption;
begin
   Caption:=FCaptionText;
end;{TFreeHullForm.SetCaption}

procedure TFreeHullForm.FSetFreeShip(Val:TFreeShip);
begin
   if Val<>FFreeShip then
   begin
      if FFreeShip<>nil then
      begin
         // Disconnect from Freeship component
         FFreeShip.DeleteViewport(Viewport);
      end;
      FFreeShip:=Val;
      if FFreeShip<>nil then
      begin
         // Connect to Freeship component
         FFreeShip.AddViewport(Viewport);
      end;
   end;
end;{TFreeHullForm.FSetFreeShip}

procedure TFreeHullForm.UpdateMenu;
begin
   // Update all menuitems and action
   Print.Enabled:=(Viewport.ViewportMode=vmWireframe) and (Printer<>nil);
   WideLens.Checked:=Viewport.CameraType=ftWide;
   Camera1.Enabled:=Viewport.Viewtype=fvPerspective;
   StandardLens.Checked:=Viewport.CameraType=ftStandard;
   ShortTeleLens.Checked:=Viewport.CameraType=ftShortTele;
   MediumTeleLens.Checked:=Viewport.CameraType=ftMediumTele;
   LongTeleLens.Checked:=Viewport.CameraType=ftFarTele;
   // viewport view
   ViewBodyplan.Checked:=Viewport.ViewType=fvBodyplan;
   ViewProfile.Checked:=Viewport.ViewType=fvProfile;
   ViewPlan.Checked:=Viewport.ViewType=fvPlan;
   ViewPerspective.Checked:=Viewport.ViewType=fvPerspective;
   // Drawingmode
   ShowWireframe.Checked:=Viewport.ViewportMode=vmWireframe;
   ShowFlatShade.Checked:=Viewport.ViewportMode=vmShade;
   ShowGaussCurvature.Checked:=Viewport.ViewportMode=vmShadeGauss;
   ShowDevelopablity.Checked:=Viewport.ViewportMode=vmShadeDevelopable;
   ShadeZebra.Checked:=Viewport.ViewportMode=vmShadeZebra;
   // background image properties
   ImportBackGround.Enabled:=Viewport.ViewType<>fvPerspective;
   Backgroundclear.Enabled:=Viewport.BackgroundImage.Bitmap<>nil;
   BackgroundOrigin.Enabled:=(Viewport.ViewType<>fvPerspective) and (Viewport.BackgroundImage.Bitmap<>nil) and (Viewport.BackgroundImage.Visible) and (Viewport.BackgroundImage.ShowInView=Viewport.ViewType);
   BackgroundScale.Enabled:=(Viewport.ViewType<>fvPerspective) and (Viewport.BackgroundImage.Bitmap<>nil) and (Viewport.BackgroundImage.Visible) and (Viewport.BackgroundImage.ShowInView=Viewport.ViewType);
   BackgroundTransparentColor.Enabled:=(Viewport.ViewType<>fvPerspective) and (Viewport.BackgroundImage.Bitmap<>nil) and (Viewport.BackgroundImage.Visible) and (Viewport.BackgroundImage.ShowInView=Viewport.ViewType);
   BackgroundBlending.Enabled:=(Viewport.ViewType<>fvPerspective) and (Viewport.BackgroundImage.Bitmap<>nil) and (Viewport.BackgroundImage.Visible) and (Viewport.BackgroundImage.ShowInView=Viewport.ViewType);
   BackgroundExport.Enabled:=(Viewport.ViewType<>fvPerspective) and (Viewport.BackgroundImage.Bitmap<>nil) and (Viewport.BackgroundImage.Visible) and (Viewport.BackgroundImage.ShowInView=Viewport.ViewType);
   BackgroundTolerance.Enabled:=(Viewport.ViewType<>fvPerspective) and
                                (Viewport.BackgroundImage.Bitmap<>nil) and
                                (Viewport.BackgroundImage.Visible) and
                                (Viewport.BackgroundImage.ShowInView=Viewport.ViewType) and
                                (Viewport.BackgroundImage.Transparent);
   BackgroundVisible.Checked:=Viewport.BackgroundImage.Visible;
   BackgroundVisible.Enabled:=(Viewport.ViewType<>fvPerspective) and (Viewport.BackgroundImage.Bitmap<>nil) and (Viewport.BackgroundImage.ShowInView=Viewport.ViewType);
end;{TFreeHullForm.UpdateMenu}

procedure TFreeHullForm.ViewportRequestExtents(Sender: TObject; var Min,Max: T3DCoordinate);
begin
   if FreeShip<>nil then
   begin
      Freeship.Extents(Min,Max);
      if Viewport.ViewType=fvBodyPlan then Min.Y:=-Max.Y;
   end;
end;{TFreeHullForm.ViewportRequestExtents}

procedure TFreeHullForm.FormDestroy(Sender: TObject);
begin
end;

procedure TFreeHullForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  ViewportKeyPress(Sender, Key);
end;

procedure TFreeHullForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ViewportKeyUp(Sender, Key, Shift);
end;

procedure TFreeHullForm.SetLightExecute(Sender: TObject);
begin
  if not assigned(LightDialog) then
     begin
     LightDialog:=TLightDialog.Create(self);
     LightDialog.ViewPort := self.Viewport;
     LightDialog.Show;
     end
  else
      begin
      if not LightDialog.IsVisible then
        LightDialog.Show;
      LightDialog.BringToFront;
      LightDialog.SetFocus;
      end;
end;

procedure TFreeHullForm.ViewportRedraw(Sender: TObject);
begin
   if FreeShip<>nil then FreeShip.DrawToViewport(Viewport);
end;{TFreeHullForm.ViewportRedraw}

procedure TFreeHullForm.createFreeViewport();
  begin
    Viewport := TFreeViewport.Create(Self);
    with Viewport do
    begin
      Parent := Self;
      Cursor := crCross;
      Left := 0;
      Height := 270;
      Top := 0;
      Width := 425;
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
      BevelInner := bvNone;
      BevelOuter := bvNone;
      BorderStyle := bsSingle;
      BorderWidth := 0;
      CameraType := ftStandard;
      Color := 10461087;
      DoubleBuffer := True;
      Elevation := 20;
      HorScrollbar := ScrollBar1;
      Margin := 1;
      PopupMenuHull := PopupMenuHull;
      VertScrollbar := ScrollBar2;
      ViewType := fvPerspective;
      ViewportMode := vmWireFrame;
      OnChangeBackground := ViewportChangeBackground;
      OnChangeViewType := ViewportChangeViewType;
      OnKeyPress := ViewportKeyPress;
      OnKeyUp := ViewportKeyUp;
      OnMouseDown := ViewportMouseDown;
      OnMouseUp := ViewportMouseUp;
      OnMouseMove := ViewportMouseMove;
      OnMouseLeave := ViewportMouseLeave;
      OnRedraw := ViewportRedraw;
      OnRequestBackgroundImage := ViewportRequestBackgroundImage;
      OnRequestExtents := ViewportRequestExtents;
    end;
  end;


procedure TFreeHullForm.FormCreate(Sender: TObject);
{$IFDEF USEOPENGL}
var VP: TFreeViewportOpenGL;
{$ENDIF}
begin
{   createFreeViewport();
   ScrollBar1.Position:=Round(Viewport.Angle);
   ScrollBar2.Position:=Round(Viewport.Elevation);
   FAllowPanOrZoom:=False;
   FreeShip:=GlobalFreeShip;
}
   {$IFDEF USEOPENGL}
   VP := TFreeViewportOpenGL.Create(Self);
   VP.Parent := ViewPort.Parent;
   VP.Align  := ViewPort.Align;
   VP.BevelInner:= ViewPort.BevelInner;
   VP.BevelOuter:= ViewPort.BevelOuter;
   VP.Color := ViewPort.Color;
   VP.Cursor := ViewPort.Cursor;
   VP.HorScrollbar := ViewPort.HorScrollbar;
   VP.PopupMenu := ViewPort.PopupMenu;
   VP.VertScrollbar := ViewPort.VertScrollbar;

   ViewPort.Destroy;
   ViewPort:=VP;
   {$ENDIF}
end;{TFreeHullForm.FormCreate}

procedure TFreeHullForm.FormClose(Sender: TObject; var Action: TCloseAction);
var I:integer; C:TComponent;
begin
   // Disconnect from FreeShip component;
   {$IFDEF FPC}
   if Assigned(Owner) then
     for I:=0 to TMainForm(Owner).MDIChildCount-1 do
       begin
         C:= TMainForm(Owner).GetMDIChildren(I);
         if (C is TFreeHullForm) and (TFreeHullForm(C)=Self) then
             TMainForm(Owner).AbandonMDIChildren(I);

       end;
   {$ENDIF}
   Freeship:=nil;
   Action:=caFree;
end;{TFreeHullForm.FormClose}

procedure TFreeHullForm.PopupMenuHullPopup(Sender: TObject);
begin
   UpdateMenu;
end;{TFreeHullForm.PopupMenu1Popup}

procedure TFreeHullForm.StandardLensExecute(Sender: TObject);
begin
   Viewport.CameraType:=ftStandard;
end;{TFreeHullForm.StandardLensExecute}

procedure TFreeHullForm.WideLensExecute(Sender: TObject);
begin
   Viewport.CameraType:=ftWide;
end;{TFreeHullForm.WideLensExecute}

procedure TFreeHullForm.ShortTeleLensExecute(Sender: TObject);
begin
   Viewport.CameraType:=ftShortTele;
end;{TFreeHullForm.ShortTeleLensExecute}

procedure TFreeHullForm.MediumTeleLensExecute(Sender: TObject);
begin
   Viewport.CameraType:=ftMediumTele;
end;{TFreeHullForm.MediumTeleLensExecute}

procedure TFreeHullForm.LongTeleLensExecute(Sender: TObject);
begin
   Viewport.CameraType:=ftFarTele;
end;{TFreeHullForm.LongTeleLensExecute}

procedure TFreeHullForm.ViewBodyPlanExecute(Sender: TObject);
begin
   Viewport.ViewType:=fvBodyplan;
end;{TFreeHullForm.ViewBodyPlanExecute}

procedure TFreeHullForm.ViewProfileExecute(Sender: TObject);
begin
   Viewport.ViewType:=fvProfile;
end;{TFreeHullForm.ViewProfileExecute}

procedure TFreeHullForm.ViewPlanExecute(Sender: TObject);
begin
   Viewport.ViewType:=fvPlan;
end;{TFreeHullForm.ViewPlanExecute}

procedure TFreeHullForm.ViewPerspectiveExecute(Sender: TObject);
begin
   Viewport.ViewType:=fvPerspective;
end;{TFreeHullForm.ViewPerspectiveExecute}

procedure TFreeHullForm.FormShow(Sender: TObject);
begin
   SetCaption;
end;{TFreeHullForm.FormShow}

procedure TFreeHullForm.ViewportChangeViewType(Sender: TObject);
begin
   SetCaption;
end;{TFreeHullForm.ViewportChangeViewType}

procedure TFreeHullForm.ZoomInExecute(Sender: TObject);
begin
   Viewport.ZoomIn;
end;{TFreeHullForm.ZoomInExecute}

procedure TFreeHullForm.ZoomExtentsExecute(Sender: TObject);
begin
   Viewport.ZoomExtents;
end;{TFreeHullForm.ZoomExtentsExecute}

procedure TFreeHullForm.ZoomOutExecute(Sender: TObject);
begin
   Viewport.ZoomOut;
end;{TFreeHullForm.ZoomOutExecute}

procedure TFreeHullForm.ViewportMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Select  : Boolean;
    P:T2DCoordinate;
begin
   FInitialPosition.X:=X;
   FInitialPosition.Y:=Y;
   if (ssAlt in Shift) and (Viewport.Viewtype<>fvPerspective) then
   begin
      P:=Viewport.ProjectBackTo2D(FInitialPosition);
      if (Freeship.Visibility.ModelView=mvBoth) and (Viewport.ViewType=fvBodyplan) then P.X:=abs(P.X);
      Freeship.Edit.Flowline_Add(P,Viewport.Viewtype);
      TMainform(Application.MainForm).UpdateMenu;
      exit;
   end;
   FPanned:=False;
   if Viewport.ViewportMode=vmWireframe then FreeShip.MouseDown(Viewport,Button,Shift,X,Y,Select);
   FAllowPanOrZoom:=not select; // An item has just been selected or deselect, so do NOT pan or zoom the viewport when the user (accidentally) moves the mouse
end;{TFreeHullForm.ViewportMouseDown}

procedure TFreeHullForm.ViewportMouseMove(Sender: TObject;Shift: TShiftState; X, Y: Integer);
var P    : TPoint;
    P2D  : T2DCoordinate;
    Str  : string;
begin
   if Viewport.ViewType<>fvPerspective then
   begin
      P.X:=X;
      P.Y:=Y;
      P2D:=Viewport.ProjectBackTo2D(P);
      if abs(P2D.X)<1e-6 then P2D.X := 0;
      if abs(P2D.X)>1e+6 then P2D.X := 1e+6;
      if abs(P2D.Y)<1e-6 then P2D.Y := 0;
      if abs(P2D.Y)>1e+6 then P2D.Y := 1e+6;

      Case Viewport.ViewType of
         fvBodyplan     : Str:=Userstring(215)+'.';
         fvProfile      : Str:=Userstring(216)+'.';
         fvPlan         : Str:=Userstring(217)+'.';
         else Str:='';
      end;
      Case Viewport.ViewType of
         fvBodyplan     : Str:=Str+'  Y='+FloatToStrF(P2D.X,ffFixed,7,3)+',   Z='+FloatToStrF(P2D.Y,ffFixed,7,3);
         fvProfile      : Str:=Str+'  X='+FloatToStrF(P2D.X,ffFixed,7,3)+',   Z='+FloatToStrF(P2D.Y,ffFixed,7,3);
         fvPlan         : Str:=Str+'  X='+FloatToStrF(P2D.X,ffFixed,7,3)+',   Y='+FloatToStrF(P2D.Y,ffFixed,7,3);
      end;
      Caption:=Str;
   end;
                         
   if (ssLeft in Shift) and (FAllowPanOrZoom) then
   begin
      // Zoom in or zoom out
      if abs(FInitialPosition.Y-Y)>4 then
      begin
         if Y<FInitialPosition.Y then
         begin
            Viewport.ZoomIn;
         end else if Y>FInitialPosition.Y then
         begin
           Viewport.ZoomOut;
         end;
         FInitialPosition.X:=X;
         FInitialPosition.Y:=Y;
      end;
   end else if (ssRight in Shift) and (FAllowPanOrZoom) then
   begin
      // Pan the window left, right, top or bottom
      if (abs(FInitialPosition.X-X)>4) or (abs(FInitialPosition.Y-Y)>4)  then
      begin
         P.X:=Viewport.Pan.X+X-FInitialPosition.X;
         P.Y:=Viewport.Pan.Y+Y-FInitialPosition.Y;
         Viewport.Pan:=P;
         FPanned:=True;
         FInitialPosition.X:=X;
         FInitialPosition.Y:=Y;
      end;
   end
   else
     if Assigned(FFreeShip) then
       FFreeShip.MouseMove(Viewport,Shift,X,Y);
end;{TFreeHullForm.ViewportMouseMove}

procedure TFreeHullForm.ViewportMouseUp(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P:TPoint;
begin
   if Button=mbRight then
   begin
      //Tracing:=false;
      // Only show pop-up menu if user has not panned the viewport (with right mouse-button)
      if FPanned then
      begin
         FPanned:=False;
      end else
      begin
         P.X:=X;
         P.Y:=Y;
         P:=Viewport.ClientToScreen(P);
         PopupMenuHull.Popup(P.X,P.Y);
      end;
   end;
   // Reset the pan/zoom flag
   FAllowPanOrZoom:=True;
   Freeship.MouseUp(Viewport,Shift,X,Y);
end;{TFreeHullForm.ViewportMouseUp}

procedure TFreeHullForm.DeselectAllExecute(Sender: TObject);
begin
   FreeShip.Edit.Selection_Clear;
end;{TFreeHullForm.DeselectAllExecute}

procedure TFreeHullForm.ViewportMouseLeave(Sender: TObject);
begin
   // stop panning or zooming when the cursor leaves the viewport
   FAllowPanOrZoom:=False;
   // And remove the cursor location from the caption
   SetCaption;
   DeActivate;
end;{TFreeHullForm.ViewportMouseLeave}

procedure TFreeHullForm.PrintExecute(Sender: TObject);
begin
   if Viewport.Width>Viewport.Height then Printer.Orientation:=poLandscape
                                     else Printer.Orientation:=poPortrait;
   if PrintDialogHull.Execute then Viewport.Print(FreeShip.ProjectSettings.ProjectUnits,Viewport.ViewType<>fvPerspective,'FREE!ship '+FCaptiontext);
end;{TFreeHullForm.PrintExecute}

procedure TFreeHullForm.ShowWireFrameExecute(Sender: TObject);
begin
   Viewport.ViewportMode:=vmWireframe;
end;{TFreeHullForm.ShowWireFrameExecute}

procedure TFreeHullForm.ShowFlatShadeExecute(Sender: TObject);
begin
   Viewport.ViewportMode:=vmShade;
end;{TFreeHullForm.ShowFlatShadeExecute}

procedure TFreeHullForm.ShowGaussCurvatureExecute(Sender: TObject);
begin
   Viewport.ViewportMode:=vmShadeGauss;
end;{TFreeHullForm.ShowGaussCurvatureExecute}

procedure TFreeHullForm.ShowDevelopablityExecute(Sender: TObject);
begin
   Viewport.ViewportMode:=vmShadeDevelopable;
end;{TFreeHullForm.ShowDevelopablityExecute}

procedure TFreeHullForm.SaveAsBitmapExecute(Sender: TObject);
var Str, vpt : string;
begin
  Case Viewport.ViewType of
     fvBodyplan     : vpt:='_Bodyplan';
     fvProfile      : vpt:='_Profile';
     fvPlan         : vpt:='_Plan';
     fvPerspective  : vpt:='_Perspective';
     else vpt:='';
  end;
   // Skip translation
  Str := Freeship.Preferences.ExportDirectory + DirectorySeparator
      + ChangeFileExt(ExtractFilename(Freeship.Filename)+vpt,'.png');
  Viewport.SaveAsBitmap(Str, true);
   // End Skip translation
end;{TFreeHullForm.SaveAsBitmapExecute}

procedure TFreeHullForm.ViewportKeyUp(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
   Freeship.KeyUp(Viewport,Key,Shift);
end;{TFreeHullForm.ViewportKeyUp}

procedure TFreeHullForm.ViewportKeyPress(Sender: TObject; var Key: Char);
begin
   if key=#27 then DeselectAllExecute(self);
end;{TFreeHullForm.ViewportKeyPress}

procedure TFreeHullForm.ShadeZebraExecute(Sender: TObject);
begin
   Viewport.ViewportMode:=vmShadeZebra;
   UpdateMenu;
end;{TFreeHullForm.ShadeZebraExecute}

procedure TFreeHullForm.ImportBackGroundExecute(Sender: TObject);
begin
   if Freeship<>nil then Freeship.Edit.BackgroundImage_Open(Viewport);
end;{TFreeHullForm.ImportBackGroundExecute}

procedure TFreeHullForm.BackgroundOriginExecute(Sender: TObject);
begin
   Viewport.BackgroundMode:=emSetOrigin;
end;{TFreeHullForm.BackgroundOriginExecute}

procedure TFreeHullForm.BackgroundScaleExecute(Sender: TObject);
begin
   Viewport.BackgroundMode:=emSetScale;
end;{TFreeHullForm.BackgroundScaleExecute}

procedure TFreeHullForm.BackgroundTransparentColorExecute(Sender: TObject);
begin
   Viewport.BackgroundMode:=emSetTransparentColor;
end;{TFreeHullForm.BackgroundTransparentColorExecute}

procedure TFreeHullForm.BackgroundClearExecute(Sender: TObject);
begin
   if (Viewport.BackgroundImage.Bitmap<>nil) and (Viewport.BackgroundImage.Visible) then
   begin
      if Freeship<>nil then Freeship.Edit.BackgroundImage_Delete(Viewport);
   end;
end;{TFreeHullForm.BackgroundclearExecute}

procedure TFreeHullForm.BackgroundBlendingExecute(Sender: TObject);
begin
   Viewport.BackgroundImage.SetBlendingValue;
end;

procedure TFreeHullForm.ViewportRequestBackgroundImage(Sender: TObject);
var I    : Integer;
    Data : TFreeBackgroundImageData;
    Pt:TPoint;
begin
   if Freeship<>nil then
   begin
      Data:=nil;
      for I:=1 to Freeship.NumberofBackgroundImages do if Freeship.BackgroundImage[I-1].AssignedView=Viewport.ViewType then
         Data:=Freeship.BackgroundImage[I-1];
      if Data<>nil then Viewport.BackgroundImage.AssignData(Data.Image,Data.AssignedView,Data.Origin,Data.Scale,Data.Transparent,Data.TransparentColor,Data.BlendingValue,Data.Quality,Data.Tolerance,True)
                   else if Viewport.BackgroundImage.Bitmap<>nil then
      begin
         Pt.X:=0;
         Pt.Y:=0;
         Viewport.BackgroundImage.AssignData(nil,fvPerspective,Pt,1.0,False,clBlack,255,100,3,True);
      end;
   end;
end;

procedure TFreeHullForm.ViewportChangeBackground(Sender: TObject);
var I:Integer;
begin
   for I:=1 to Freeship.NumberofBackgroundImages do if Freeship.BackgroundImage[I-1].AssignedView=Viewport.ViewType then
   begin
      Freeship.Edit.CreateUndoObject(Userstring(219),true);
      Freeship.BackgroundImage[I-1].UpdateData(Viewport);
      Break;
   end;
end;{TFreeHullForm.ViewportChangeBackground}

procedure TFreeHullForm.BackgroundExportExecute(Sender: TObject);
begin
   Viewport.BackgroundImage.Save;
end;{TFreeHullForm.BackgroundExportExecute}

procedure TFreeHullForm.BackgroundToleranceExecute(Sender: TObject);
var Str:Ansistring;
    Value,I:Integer;
begin
  Viewport.BackgroundImage.SetToleranceValue;

{  Str:=IntToStr(Viewport.BackgroundImage.Tolerance);
   if InputQuery(Userstring(220),Userstring(221)+' (0-255)',Str) then
   begin
      val(Str,Value,I);
      if I=0 then
      begin
         if Value<0 then value:=0 else
            if Value>255 then value:=255;
         Viewport.BackgroundImage.Tolerance:=Value;
      end else MessageDlg(Userstring(222)+'!',mtError,[mbok],0)
   end;}
end;{TFreeHullForm.BackgroundToleranceExecute}

procedure TFreeHullForm.BackgroundVisibleExecute(Sender: TObject);
begin
   Viewport.BackgroundImage.Visible:=not Viewport.BackgroundImage.Visible;
end;{TFreeHullForm.BackgroundVisibleExecute}

end.
