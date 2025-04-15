unit FreeTextureDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ExtDlgs, ActnList, ComCtrls, StdCtrls,
  Math,
  FreeTypes, FreeGeometry,
  FreeShipUnit,
  LCLTranslator, Spin;

type


  TControlModeEnum = (cmNone, cmPatchMove, cmPatchRotate, cmPatchScale, cmBGimageMove, cmBGimageScale, cmViewportPan, cmViewportZoom);

  { TFreeTextureForm }

  TFreeTextureForm = class(TForm)
    ActionDelete: TAction;
    ColorButton1: TColorButton;
    LabelBmOrigin: TLabel;
    LabelBmScale: TLabel;
    Ok: TAction;
    Button1: TButton;
    FloatSpinEditRotate: TFloatSpinEdit;
    FloatSpinEditShiftX: TFloatSpinEdit;
    FloatSpinEditShiftY: TFloatSpinEdit;
    FloatSpinEditScale: TFloatSpinEdit;
    LabelAnchor1: TLabel;
    LabelAnchor2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelTarget1: TLabel;
    LabelTarget2: TLabel;
    CloseDialog: TAction;
    LoadFile: TAction;
    ActionList1: TActionList;
    ComboBoxSelectTexture: TComboBox;
    ImageList1: TImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    PanelRight: TPanel;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButtonDelete: TToolButton;
    ToolButtonCancel: TToolButton;
    ToolButtonOk: TToolButton;
    Viewport: TFreeViewport;
    procedure DeleteTextureExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CloseDialogExecute(Sender: TObject);
    procedure AdjustBitmapOriginAndScale();
    procedure ComboBoxSelectTextureSelect(Sender: TObject);
    procedure CreateUnrolledPatches;
    procedure FitBitmap;
    procedure FloatSpinEditShiftXChange(Sender: TObject);
    procedure FloatSpinEditShiftYChange(Sender: TObject);
    procedure FloatSpinEditScaleChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LoadFileExecute(Sender: TObject);
    procedure FloatSpinEditRotateChange(Sender: TObject);
    procedure OkExecute(Sender: TObject);
    procedure setControlsAndLabels();
    procedure ViewportRequestExtents(Sender: TObject; var Min, Max: T3DCoordinate);
    procedure ViewportRedraw(Sender: TObject);
    procedure ViewportMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure ViewportMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ViewportMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SetBitmapTargetPoints;
  private
    FTextures: TFasterListTFreeTexture;
    FActiveTexture: TFreeTexture;
    FFreeShip: TFreeShip;
    FLayer: TFreeSubdivisionLayer;
    FFontSize: integer;
    FControlMode: TControlModeEnum;
    FInitialPosition: TPoint;
    FInitialPatchPosition: TPoint;
    FInitialBGimagePosition: TPoint;
    FFoundPoint: TFreeSubdivisionPoint;
    FFoundUnrolledPoint: T2DCoordinate;
    FFoundEdge: TFreeSubdivisionEdge;
    FFoundBGimage: TFreeBackgroundImage;
    procedure InitViewPort;
  public
    function Execute(FreeShip: TFreeShip; Layer: TFreeSubdivisionLayer): boolean;
  end;

var
  FreeTextureForm: TFreeTextureForm;

implementation

{$R *.lfm}

{ TFreeTextureForm }
procedure TFreeTextureForm.InitViewPort;
begin
  ViewPort := TFreeViewPort.Create(Self);
  with Viewport do
  begin
    Parent := Self;
    Left := 0;
    Height := 557;
    Top := 26;
    Width := 636;
    Angle := 0;
    Align := alClient;
    BackgroundImage.Bitmap := nil;
    BackgroundImage.Alpha := 255;
    BackgroundImage.Owner := Viewport;
    BackgroundImage.Quality := 100;
    BackgroundImage.Scale := 1;
    BackgroundImage.ShowInView := fvBodyplan;
    BackgroundImage.Tolerance := 5;
    BackgroundImage.Transparent := True;
    BackgroundImage.TransparentColor := clBlack;
    BackgroundImage.Visible := False;
    CameraType := ftStandard;
    Color := clWhite;
    DoubleBuffer := False;
    Elevation := 90;
    ParentFont:=false;
    Margin := 4;
    ViewType := fvPlan;
    ViewportMode := vmWireFrame;
    OnMouseDown := @ViewportMouseDown;
    OnMouseUp := @ViewportMouseUp;
    OnMouseMove := @ViewportMouseMove;
    OnRedraw := @ViewportRedraw;
    OnRequestExtents := @ViewportRequestExtents;
  end;
end;

procedure TFreeTextureForm.CreateUnrolledPatches;
var i: integer;
  DPL: TFasterListTFreeDevelopedPatch;
  Patch: TFreeDevelopedPatch;
  Texture: TFreeTexture;
begin
  DPL:= TFasterListTFreeDevelopedPatch.Create;
  if not FLayer.Surface.Built then FLayer.Surface.Rebuild;
  if not FLayer.Surface.CheckIntegrity then
    exit;
  FLayer.Unroll(DPL);
  FTextures.Clear;
  for i:=0 to DPL.Count-1 do
  begin
    Patch := DPL[i];
    if (not FLayer.Symmetric) or (FLayer.Symmetric and (Patch.Side<>fsStarboard)) then
    begin
      Texture := TFreeTexture.Create(FLayer, DPL[i]);
      FTextures.Add(Texture);
      ComboBoxSelectTexture.AddItem(Texture.DevelopedPatchName, Texture);
    end;
  end;
end;

function TFreeTextureForm.Execute(FreeShip: TFreeShip;
  Layer: TFreeSubdivisionLayer): boolean;
var
  I: integer;
  Texture: TFreeTexture;
  Patch: TFreeDevelopedPatch;
  Min, Max: T3DCoordinate;
  MinT, MaxT: T3DCoordinate;
  P2D: T2DCoordinate;
  Clearance: TFloatType;
  Tmp: TFloatType;
  Pt, Pt0:TPoint;
begin

  InitViewPort;
  FFreeship := FreeShip;
  FLayer := Layer;
  FTextures := TFasterListTFreeTexture.Create;
  FActiveTexture := nil;

  for i:=0 to Layer.Textures.Count-1 do
  begin
    FActiveTexture := Layer.Textures[i];
    FTextures.Add(FActiveTexture);
    ComboBoxSelectTexture.AddItem(FActiveTexture.DevelopedPatchName, FActiveTexture);
    //Viewport.ZoomExtents;
  end;

  if FTextures.Count = 0 then
    CreateUnrolledPatches;

  if FTextures.Count > 0 then
  begin
    ComboBoxSelectTexture.ItemIndex := 0;
    FActiveTexture := FTextures[0];
    ComboBoxSelectTextureSelect(Self);
  end;

  //USE ONCE!
  ////Freeship.Preferences.dumpIcons(MenuImages,ActionList1);

  ImageList1.Height := Freeship.Preferences.ToolIconSize;
  ImageList1.Width := Freeship.Preferences.ToolIconSize;
  ToolBar1.ButtonHeight:= ImageList1.Height + 4;
  ToolBar1.ButtonWidth := ImageList1.Width  + 4;
  Freeship.Preferences.LoadImageListByActions(ImageList1, ActionList1, 'Action');

  //FUpdateListBox;

  FFontSize := 8;
  FControlMode := cmNone;

  OpenPictureDialog1.InitialDir := FFreeShip.Preferences.ImportDirectory;

  //ShowTranslatedValues(Self);
  ShowModal;
  Result := ModalResult = mrOk;

  if Result then
  begin
    FLayer.Textures.Clear;
    FLayer.Textures.AddList(FTextures);
    FLayer.ShowTexture:=true;
  end;

end;{TFreeTextureForm.Execute}

procedure TFreeTextureForm.ViewportMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y : integer);
var
  S,D0,D1,Z: TFloatType;
  P, bP, Po, Pt : TPoint;
  P1, P2, P3, uP, T: T2DCoordinate;
  R: TRect;
  p3d:T3DCoordinate;
  Diff: T2DCoordinate;
  Patch: TFreeDevelopedPatch;
  MP: TFreeSubdivisionPoint;
  Edge: TFreeSubdivisionEdge;
  UP1,UP2: T2DCoordinate;
begin
  if not Assigned(FActiveTexture) then exit;

  if FControlMode <> cmNone then
  if FControlMode = cmViewportZoom then
  begin
    // Zoom in or zoom out
    if abs(FInitialPosition.Y - Y) > 4 then
    begin
      if Y < FInitialPosition.Y then
        Viewport.ZoomIn
      else
      if Y > FInitialPosition.Y then
        Viewport.ZoomOut;
      FInitialPosition.X := X;
      FInitialPosition.Y := Y;
      //SetBitmapTargetPoints;
      Viewport.Invalidate;
    end;
  end
  else if FControlMode = cmViewportPan then
    if (abs(FInitialPosition.X - X) > 4) or (abs(FInitialPosition.Y - Y) > 4) then
    begin
      P.X := Viewport.Pan.X + X - FInitialPosition.X;
      P.Y := Viewport.Pan.Y + Y - FInitialPosition.Y;
      Viewport.Pan := P;
      FInitialPosition.X := X;
      FInitialPosition.Y := Y;
      //SetBitmapTargetPoints;
      Viewport.Invalidate;
    end
  else
  else if (FControlMode = cmPatchMove)
     and ((abs(FInitialPatchPosition.X - X) > 0) or (abs(FInitialPatchPosition.Y - Y) > 0)) then
    begin
      T := FActiveTexture.Translation;
      bP := Viewport.Project(Point3D(T.X, T.Y, 0));
      P.X := bP.X + X - FInitialPatchPosition.X;
      P.Y := bP.Y + Y - FInitialPatchPosition.Y;
      T := Viewport.ProjectBackTo2D(P);

      FloatSpinEditShiftX.OnChange := nil;
      FloatSpinEditShiftY.OnChange := nil;
      FActiveTexture.Translation  := T;
      FloatSpinEditShiftX.Value := T.X;
      FloatSpinEditShiftY.Value := T.Y;
      FloatSpinEditShiftX.OnChange := @FloatSpinEditShiftXChange;
      FloatSpinEditShiftY.OnChange := @FloatSpinEditShiftYChange;

      FInitialPatchPosition.X := X;
      FInitialPatchPosition.Y := Y;
      FActiveTexture.IsManuallyAdjusted := true;
      SetBitmapTargetPoints;
      Viewport.Invalidate;
    end
  else if FControlMode = cmPatchScale then
    begin
      S := FActiveTexture.Scale;
      P1 := FActiveTexture.GetMidPoint;
      D0 := Distance2D(P1, FFoundUnrolledPoint);
      P2 := Viewport.ProjectBackTo2D(ToPoint(X,Y));
      D1 := Distance2D(P1, P2);
      S := (D1/D0);

      FloatSpinEditScale.OnChange := nil;
      FActiveTexture.Scale  := S;
      FloatSpinEditScale.Value := S;
      FloatSpinEditScale.OnChange := @FloatSpinEditScaleChange;

      FActiveTexture.IsManuallyAdjusted := true;
      SetBitmapTargetPoints;
      Viewport.Invalidate;
    end
  else if FControlMode = cmPatchRotate then
    begin
      S := FActiveTexture.Rotation;
      P1 := FActiveTexture.GetMidPoint;
      P2 := Viewport.ProjectBackTo2D(ToPoint(X,Y));
      S := Angle(P1, FFoundUnrolledPoint, P1, P2);

      FloatSpinEditRotate.OnChange := nil;
      FActiveTexture.Rotation  := S;
      FloatSpinEditRotate.Value := S;
      FloatSpinEditRotate.OnChange := @FloatSpinEditRotateChange;

      FActiveTexture.IsManuallyAdjusted := true;
      SetBitmapTargetPoints;
      Viewport.Invalidate;
    end
  else if (FControlMode = cmBgImageMove)
    and ((abs(FInitialBGimagePosition.X - X) > 0) or (abs(FInitialBGimagePosition.Y - Y) > 0)) then
    begin
      Po := FActiveTexture.BitmapOrigin;
      Pt := Viewport.Project(ZERO);
      Z := (Viewport.Scale * Viewport.Zoom * Viewport.BackgroundImage.Scale);
      P.X := round(Po.X + ((FInitialBGimagePosition.X - X))/Z);
      P.Y := round(Po.Y + ((FInitialBGimagePosition.Y - Y))/Z);

      FActiveTexture.BitmapOrigin := P;
      Viewport.BackgroundImage.Origin := P;

      FInitialBGimagePosition.X := X;
      FInitialBGimagePosition.Y := Y;
      FActiveTexture.IsManuallyAdjusted := true;
      SetBitmapTargetPoints;
      Viewport.Invalidate;
    end
  else if FControlMode = cmBgImageScale then
    begin
    end;

  P.X := X;
  P.Y := Y;
  P2 := Viewport.ProjectBackTo2D(P);

  p3d.x:=P2.X; p3d.y:=P2.y; p3d.z:=0;
  P:=Viewport.Project(p3d);

  P3 := FActiveTexture.ProjectViewportTo2D(P2);

  //MP := FActiveTexture.FindSubdivionPoint(P3);

  StatusBar1.Panels[0].Text := String.Format('Scr %0:d:%1:d',[X,Y]);

  Viewport.Cursor := crDefault;

  FFoundBGimage := nil;
  if Assigned(Viewport.BackgroundImage.Bitmap)
      and (Viewport.BackgroundImage.Bitmap.Width > 0)
      and (Viewport.BackgroundImage.Bitmap.Height > 0) then
  begin
    bP := Viewport.BackgroundImage.ImageCoordinate(X,Y);
    if (bP.X>=0) and (bP.X<=Viewport.BackgroundImage.Bitmap.Width)
      and (bP.Y>=0) and (bP.Y<=Viewport.BackgroundImage.Bitmap.Height)
    then
    begin
      FFoundBGimage := Viewport.BackgroundImage;
      Viewport.Cursor := crSizeAll;
    end;
    StatusBar1.Panels[1].Text := String.Format('Bmp %0:d:%1:d',[bP.X,bP.Y]);
  end;

  StatusBar1.Panels[2].Text := String.Format('Viw %0:.3f:%1:.3f',[P3.X, P3.Y]);

  FFoundEdge := nil;
  FFoundPoint := nil;
  MP:=nil;
  MP := FActiveTexture.FindSubdivionPointByScreen(X,Y,Viewport);
  if Assigned(MP) then
  begin
    FFoundPoint := MP;
    Viewport.Cursor := crRotate2d;
    uP := FActiveTexture.FindUnrolledPointForSubdivionPoint(MP);
    if not IsNAN(uP.X) then
    begin
      FFoundUnrolledPoint := uP;
      StatusBar1.Panels[3].Text := String.Format(
        'Plt %0:8.3f:%1:8.3f  Mdl %2:8.3f:%3:8.3f:%4:8.3f',
        [uP.X, uP.Y,
         MP.Coordinate.X, MP.Coordinate.Y, MP.Coordinate.Z]);

      bP := FActiveTexture.ProjectOnBitmap(uP);
      StatusBar1.Panels[0].Text := String.Format('Txt %0:d:%1:d',[bP.X,bP.Y]);
    end;
  end;

  if not Assigned(MP) then
  begin
    Edge := FActiveTexture.FindSubdivionEdgeByScreen(X,Y,Viewport,UP1,UP2);
    if Assigned(Edge) then
    begin
      FFoundEdge := Edge;
      Viewport.Cursor := crSizeAll;
    end;
  end;

end;{TFreeTextureForm.ViewportMouseMove}

procedure TFreeTextureForm.ViewportMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  //Active: TFreeDevelopedPatch;
  I, Dist: integer;
begin
  FInitialPosition.X := X;
  FInitialPosition.Y := Y;
  FInitialPatchPosition.X := X;
  FInitialPatchPosition.Y := Y;
  FInitialBGimagePosition.X := X;
  FInitialBGimagePosition.Y := Y;
  //FAllowPanOrZoom := True;
  FControlMode := cmNone;
  if Assigned(FFoundEdge) and (ssLeft in Shift) then FControlMode := cmPatchMove
  else if Assigned(FFoundPoint) and (ssLeft in Shift) then FControlMode := cmPatchRotate
  else if Assigned(FFoundPoint) and (ssRight in Shift) then FControlMode := cmPatchScale
  else if Assigned(FFoundBGimage) and (ssLeft in Shift) then FControlMode := cmBGimageMove
  else if Assigned(FFoundBGimage) and (ssRight in Shift) then FControlMode := cmBGimageScale
  else if (ssLeft in Shift) then FControlMode := cmViewportPan
  else if (ssRight in Shift) then FControlMode := cmViewportZoom;

  //SetBitmapTargetPoints;
end;{TFreeTextureForm.ViewportMouseDown}

procedure TFreeTextureForm.ViewportMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FControlMode := cmNone;
  FFoundPoint := nil;
  FFoundEdge := nil;
  FFoundBGimage := nil;
end;{TFreeTextureForm.ViewportMouseUp}


procedure TFreeTextureForm.SetBitmapTargetPoints;
var p1,p2: T3DCoordinate;
  tp1, tp2: TPoint;
  z: TFloatType;
begin
  if (Assigned(ViewPort.BackgroundImage.Bitmap)
     and (ViewPort.BackgroundImage.Bitmap.Width > 0)
     and (ViewPort.BackgroundImage.Bitmap.Height > 0))
  then
  begin
    p1 := FActiveTexture.Project2DtoViewport(FActiveTexture.DevelopedPatchAnchorPoint1);
    tp1 := ViewPort.Project(p1);
    FActiveTexture.BitmapTargetPoint1 := ViewPort.BackgroundImage.ImageCoordinate(tp1.X,tp1.Y);

    p2 := FActiveTexture.Project2DtoViewport(FActiveTexture.DevelopedPatchAnchorPoint2);
    tp2 := ViewPort.Project(p2);
    FActiveTexture.BitmapTargetPoint2 := ViewPort.BackgroundImage.ImageCoordinate(tp2.X,tp2.Y);

    FActiveTexture.IsCorelated := true;

    setControlsAndLabels();
  end;
end;

procedure TFreeTextureForm.ViewportRequestExtents(Sender: TObject;
  var Min, Max: T3DCoordinate);
begin
  if not Assigned(FActiveTexture.Bitmap) then exit;
  Min.X:=0; Min.Y:=0; Min.Z:=0;
  Max.X:=1; Max.Y:=1; Max.Z:=0;
  if FActiveTexture <> nil then
  begin
    Min.X:=Single.MaxValue; Min.Y:=Single.MaxValue; Min.Z:=0;
    Max.X:=Single.MinValue; Max.Y:=Single.MinValue; Max.Z:=0;
    FActiveTexture.Extents(Min,Max);
  end;
end;{TFreeTextureForm.ViewportRequestExtents}

procedure TFreeTextureForm.FitBitmap;
var Pt0, Pt, Pt1, Pt2: TPoint; W,H: integer;
  PatchW, PatchH: TFloatType;
  Bm1,Bm2,P2D: T2DCoordinate; Bm3: T3DCoordinate;
begin
  Viewport.BackgroundImage.Visible := false;
  if not Assigned(FActiveTexture.Bitmap) then exit;
  Viewport.ZoomExtents;
  W := FActiveTexture.Bitmap.Width;
  H := FActiveTexture.Bitmap.Height;
  if (W=0) or (H=0) then exit;
  FActiveTexture.BitmapScale := (Viewport.ClientWidth - 40) / W / Viewport.Scale;
  Bm1 := Viewport.ProjectBackTo2D(ToPoint(0+20, 0+20));
  Bm2 := Viewport.ProjectBackTo2D(ToPoint(Viewport.ClientWidth-20, Viewport.ClientHeight-20));
  Pt.X := round( -Bm1.X / FActiveTexture.BitmapScale);
  Pt.Y := round(  Bm1.Y / FActiveTexture.BitmapScale);
  FActiveTexture.BitmapOrigin := Pt;

  Viewport.BackgroundImage.Origin := FActiveTexture.BitmapOrigin;
  Viewport.BackgroundImage.Scale := FActiveTexture.BitmapScale;
  Viewport.BackgroundImage.Visible := true;
  SetBitmapTargetPoints;
  setControlsAndLabels();
end;

procedure TFreeTextureForm.AdjustBitmapOriginAndScale();
var Pt0, vzP, Pt1, Pt2, bmP1,bmP2, vbP0,vbP1,vbP2, vmP1,vmP2: TPoint;
  mP1, mP2 : T2DCoordinate;
  vP1,vP2 : T3DCoordinate;
  bmR: TRect; Z, A, L: TFloatType;
  vX, vY: longint;
begin
  bmP1 := FActiveTexture.BitmapTargetPoint1;
  bmP2 := FActiveTexture.BitmapTargetPoint2;
  mP1 := FActiveTexture.DevelopedPatchAnchorPoint1;
  mP2 := FActiveTexture.DevelopedPatchAnchorPoint2;

  FActiveTexture.BitmapScale := abs(
   sqrt(sqr(mP2.X - mP1.X) + sqr(mP2.Y - mP1.Y)) /
   sqrt(sqr(bmP2.X - bmP1.X) + sqr(bmP2.Y - bmP1.Y)) );
  Viewport.BackgroundImage.Scale := FActiveTexture.BitmapScale;

  //Viewport.Zoom := 1;

  Viewport.BackgroundImage.Origin := ToPoint(0,0);
  vbP1 := Viewport.BackgroundImage.TargetCoordinate(bmP1.X,bmP1.Y);
  vbP2 := Viewport.BackgroundImage.TargetCoordinate(bmP2.X,bmP2.Y);
  vmP1 := Viewport.Project(Point3D(mP1.X,mP1.Y,0));
  vmP2 := Viewport.Project(Point3D(mP2.X,mP2.Y,0));

  A := Angle( Point2D(vbP1.X,vbP1.Y), Point2D(vbP2.X,vbP2.Y),
              Point2D(vmP1.X,vmP1.Y), Point2D(vmP2.X,vmP2.Y));
  FActiveTexture.Rotation := -A;

  vP1 := FActiveTexture.Project2DtoViewport(mP1);
  vP2 := FActiveTexture.Project2DtoViewport(mP2);
  vmP1 := Viewport.Project(vP1);
  vmP2 := Viewport.Project(vP2);

  vzP := Viewport.Project(ZERO);
  bmR := Viewport.BackgroundImage.TargetRect;

  Pt1 := Viewport.BackgroundImage.ImageCoordinate(bmR.Left + (vmP1.X - vbP1.X), bmR.Top + (vmP1.Y - vbP1.Y));

  FActiveTexture.BitmapOrigin := ToPoint(-Pt1.X, -Pt1.Y);
  Viewport.BackgroundImage.Origin := FActiveTexture.BitmapOrigin;

  //Viewport.Zoom := 1;


    //Result.Left := Pt.X - round(Owner.Scale * Owner.Zoom * FScale * FOrigin.X);
    //(Pt.X - Result.Left) := Owner.Scale * Owner.Zoom * FScale * FOrigin.X;
    //(Pt.X - Result.Left) / (Owner.Scale * Owner.Zoom * FScale) := FOrigin.X;
    //FOrigin.X := (Pt.X - Result.Left) / (Owner.Scale * Owner.Zoom * FScale);

  //Z := (Viewport.Scale * Viewport.Zoom * Viewport.BackgroundImage.Scale);
  //Pt2.X := round((vzP.X - vbP0.X) / Z);
  //Pt2.Y := round((vzP.Y - vbP0.Y) / Z);
  //L := vzP.X - Z*Pt2.X; // should give vbP1.X
  //FActiveTexture.BitmapOrigin := Pt2;
  //Viewport.BackgroundImage.Origin := FActiveTexture.BitmapOrigin;

end;

procedure TFreeTextureForm.ComboBoxSelectTextureSelect(Sender: TObject);
var Pt0, Pt, Pt1, Pt2, bmP1,bmP2, vpP1,vpP2: TPoint; W,H: integer;
  PatchW, PatchH: TFloatType;
  Bm2, mP1, mP2: T2DCoordinate; Bm3: T3DCoordinate;
  bmR: TRect; Z: TFloatType;
begin
  FActiveTexture := TFreeTexture(ComboBoxSelectTexture.Items.Objects[ComboBoxSelectTexture.ItemIndex]);
  if not Assigned(FActiveTexture) then Exit;
  //if not FActiveTexture.IsManuallyAdjusted then
  begin
    if FActiveTexture.HasBitmap then
    begin
      Viewport.BackgroundImage.Bitmap := FActiveTexture.Bitmap;
      Viewport.BackgroundImage.ShowInView := Viewport.ViewType;
      Viewport.BackgroundImage.Visible := true;
      Viewport.BackgroundMode:=emNormal;
      if (FActiveTexture.IsCorelated) then
      begin
        Viewport.ZoomExtents;
        AdjustBitmapOriginAndScale();
      end
      else
      begin
        FActiveTexture.FindOptimalRotation;
        Viewport.ZoomExtents;
        FitBitmap;
        FActiveTexture.AutoSetDevelopedPatchAnchorPoints;
        SetBitmapTargetPoints;
      end;
    end
    else
    begin
      Viewport.ZoomExtents;
      Viewport.BackgroundImage.Visible := false;
    end;
  end;

  setControlsAndLabels();

  Viewport.Invalidate;
end;

procedure TFreeTextureForm.setControlsAndLabels();
begin
  FloatSpinEditRotate.OnChange := nil;
  FloatSpinEditShiftX.OnChange := nil;
  FloatSpinEditShiftY.OnChange := nil;
  FloatSpinEditScale.OnChange := nil;

  FloatSpinEditRotate.Value := FActiveTexture.Rotation;
  FloatSpinEditShiftX.Value := FActiveTexture.Translation.X;
  FloatSpinEditShiftY.Value := FActiveTexture.Translation.Y;
  FloatSpinEditScale.Value := FActiveTexture.Scale;

  FloatSpinEditRotate.OnChange := @FloatSpinEditRotateChange;
  FloatSpinEditShiftX.OnChange := @FloatSpinEditShiftXChange;
  FloatSpinEditShiftY.OnChange := @FloatSpinEditShiftYChange;
  FloatSpinEditScale.OnChange := @FloatSpinEditScaleChange;

  FActiveTexture.Color := clYellow;

  LabelAnchor1.Caption:= String.Format('%0:.3f : %1:.3f',
    [FActiveTexture.DevelopedPatchAnchorPoint1.X, FActiveTexture.DevelopedPatchAnchorPoint1.Y]);

  LabelAnchor2.Caption:= String.Format('%0:.3f : %1:.3f',
    [FActiveTexture.DevelopedPatchAnchorPoint2.X, FActiveTexture.DevelopedPatchAnchorPoint2.Y]);

  LabelTarget1.Caption:= String.Format('%0:d : %1:d',
    [FActiveTexture.BitmapTargetPoint1.X, FActiveTexture.BitmapTargetPoint1.Y]);

  LabelTarget2.Caption:= String.Format('%0:d : %1:d',
    [FActiveTexture.BitmapTargetPoint2.X, FActiveTexture.BitmapTargetPoint2.Y]);

  LabelBmOrigin.Caption:= String.Format('BO:%0:d:%1:d',
    [FActiveTexture.BitmapOrigin.X, FActiveTexture.BitmapOrigin.Y]);

  LabelBmScale.Caption:= String.Format('BS:%0:.3f',
    [FActiveTexture.BitmapScale]);

end;

procedure TFreeTextureForm.FloatSpinEditShiftXChange(Sender: TObject);
var t:T2DCoordinate;
begin
  t.X := FloatSpinEditShiftX.Value;
  t.Y := FloatSpinEditShiftY.Value;
  FActiveTexture.Translation := t;
  FActiveTexture.IsManuallyAdjusted := true;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;

procedure TFreeTextureForm.FloatSpinEditShiftYChange(Sender: TObject);
var t:T2DCoordinate;
begin
  t.X := FloatSpinEditShiftX.Value;
  t.Y := FloatSpinEditShiftY.Value;
  FActiveTexture.Translation := t;
  FActiveTexture.IsManuallyAdjusted := true;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;

procedure TFreeTextureForm.FloatSpinEditScaleChange(Sender: TObject);
begin
  FActiveTexture.Scale := FloatSpinEditScale.Value;
  FActiveTexture.IsManuallyAdjusted := true;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;

procedure TFreeTextureForm.FloatSpinEditRotateChange(Sender: TObject);
begin
  FActiveTexture.Rotation := FloatSpinEditRotate.Value;
  FActiveTexture.IsManuallyAdjusted := true;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;

procedure TFreeTextureForm.OkExecute(Sender: TObject);
begin
    ModalResult := mrOk;
end;

procedure TFreeTextureForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Viewport.BackgroundImage.Visible:=false;
  Viewport.BackgroundImage.Bitmap:=nil;
end;

procedure TFreeTextureForm.FormCreate(Sender: TObject);
begin

end;

procedure TFreeTextureForm.LoadFileExecute(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    FActiveTexture.LoadIntfImageFromFile(OpenPictureDialog1.FileName);
    FActiveTexture.IsCorelated := false;
    if FActiveTexture.HasBitmap then
    begin
      Viewport.BackgroundImage.Bitmap := FActiveTexture.Bitmap;
      Viewport.BackgroundImage.Origin := FActiveTexture.BitmapOrigin;
      Viewport.BackgroundImage.Scale := FActiveTexture.BitmapScale;
      Viewport.BackgroundImage.ShowInView := Viewport.ViewType;
      Viewport.BackgroundImage.Visible := true;
      Viewport.BackgroundMode:=emNormal;
      if (not FActiveTexture.IsCorelated) then
      begin
        FitBitmap;
        FActiveTexture.AutoSetDevelopedPatchAnchorPoints;
        SetBitmapTargetPoints;
      end;
    end
    else
      Viewport.BackgroundImage.Visible := false;
  end;
end;

procedure TFreeTextureForm.CloseDialogExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFreeTextureForm.Button1Click(Sender: TObject);
begin
  FActiveTexture.IsManuallyAdjusted := false;
  FActiveTexture.FindOptimalRotation;
  FloatSpinEditRotate.Value := FActiveTexture.Rotation;
  FActiveTexture.AutoSetDevelopedPatchAnchorPoints;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;

procedure TFreeTextureForm.DeleteTextureExecute(Sender: TObject);
begin
  Viewport.BackgroundImage.Visible:=false;
  Viewport.BackgroundImage.Bitmap := nil;
  FActiveTexture.ClearBitmap;
end;


procedure TFreeTextureForm.ViewportRedraw(Sender: TObject);
var
  Pt0, Pt: TPoint;
begin
  if not Assigned(FActiveTexture) then exit;
  FActiveTexture.Color := ColorButton1.ButtonColor;
  FActiveTexture.Draw(Viewport);
end;{TFreeTextureForm.ViewportRedraw}

end.

