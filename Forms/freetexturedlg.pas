unit FreeTextureDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ExtDlgs, ActnList, ComCtrls, StdCtrls,
  Math,
  FreeTypes, FreeGeometry,
  FreeShipUnit,
  LCLTranslator, Spin, Types;

type

  TWorkModeEnum = (wmViewport, wmAnchors, wmPatch, wmBGimage);
  TWorkModeSet = set of TWorkModeEnum;
  TControlModeEnum = (cmNone, cmAnchorMove, cmPatchMove, cmPatchRotate, cmPatchScale, cmBGimageMove, cmBGimageScale, cmViewportPan, cmViewportZoom);

  { TFreeTextureForm }

  TFreeTextureForm = class(TForm)
    ActionDelete: TAction;
    CheckBoxSymmetric: TCheckBox;
    ColorButton1: TColorButton;
    ComboBoxWrapMode: TComboBox;
    FloatSpinEditTextureScale: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
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
    SpinEditTextureShiftX: TSpinEdit;
    SpinEditTextureShiftY: TSpinEdit;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButtonLayer: TToolButton;
    ToolButtonTexture: TToolButton;
    ToolButtonAnchors: TToolButton;
    ToolButtonDelete: TToolButton;
    ToolButtonCancel: TToolButton;
    ToolButtonOk: TToolButton;
    Viewport: TFreeViewport;
    procedure CheckBoxSymmetricChange(Sender: TObject);
    procedure ComboBoxWrapModeSelect(Sender: TObject);
    procedure DeleteTextureExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CloseDialogExecute(Sender: TObject);
    procedure AdjustBitmapOriginAndScale();
    procedure AdjustPatchScaleTranslationAndRotation(origBTP1,origBTP2:TPoint);
    {procedure CalculateTransformation(
      const OldAnchor1VP, OldAnchor2VP: T3DCoordinate;
      const NewAnchor1VP, NewAnchor2VP: T3DCoordinate;
      const NewTxAnchor1VP, NewTxAnchor2VP: T2DCoordinate;
      const OldScreen1, OldScreen2: TPoint;
      const NewScreen1, NewScreen2: TPoint;
      const OldScale: Single;
      const OldRotationDeg: Single;
      const OldTX, OldTY: Single;
      var NewScale: Single;
      var NewRotationDeg: Single;
      var NewTX, NewTY: Single
    );}
    procedure CalculateNewPatchTransform(const OldScreen1, OldScreen2: TPoint; const NewScreen1, NewScreen2: TPoint);
    procedure ComboBoxSelectTextureSelect(Sender: TObject);
    procedure CreateUnrolledPatches;
    procedure FitBitmap;
    procedure FloatSpinEditShiftXChange(Sender: TObject);
    procedure FloatSpinEditShiftYChange(Sender: TObject);
    procedure FloatSpinEditScaleChange(Sender: TObject);
    procedure FloatSpinEditTextureScaleChange(Sender: TObject);
    procedure SpinEditTextureShiftXChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SpinEditTextureShiftYChange(Sender: TObject);
    procedure ViewportMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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
    FWorkModes: TWorkModeSet;
    FControlMode: TControlModeEnum;
    FInitialPosition: TPoint;
    FInitialAnchorPosition: TPoint;
    FInitialPatchPosition: TPoint;
    FInitialBGimagePosition: TPoint;
    FInitialFoundAnchorNo: integer;
    FInitialAnchor1VP: T3DCoordinate;
    FInitialAnchor2VP: T3DCoordinate;
    FInitialAnchor1screen: TPoint;
    FInitialAnchor2screen: TPoint;
    FInitialScale: TFloatType;
    FInitialRotation: TFloatType;
    FInitialTranslation: T2DCoordinate;

    FBitmapTargetPoint1old: TPoint;
    FBitmapTargetPoint2old: TPoint;
    FFoundAnchor2d: T2DCoordinate;
    FFoundAnchorNo: integer;
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
    OnMouseWheel:= @ViewportMouseWheel;
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

  if Result and ((FActiveTexture<>nil) and FActiveTexture.IsManuallyAdjusted) then
  begin
    FLayer.Textures.Clear;
    FLayer.Textures.AddList(FTextures);
    FLayer.ShowTexture:=true;
    Self.FFreeShip.FileChanged:=true;
  end;
  if Result and (FActiveTexture=nil) then
  begin
    FLayer.Textures.Clear;
    FLayer.ShowTexture:=false;
    Self.FFreeShip.FileChanged:=true;
  end;

end;{TFreeTextureForm.Execute}

procedure TFreeTextureForm.ViewportMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y : integer);
var
  S,D0,D1,Z: TFloatType;
  P, bP, Po, Pt, origBTP1, origBTP2 : TPoint;
  P1, P2, P3, uP, T: T2DCoordinate;
  R: TRect;
  p3d:T3DCoordinate;
  Diff: T2DCoordinate;
  Patch: TFreeDevelopedPatch;
  MP: TFreeSubdivisionPoint;
  Edge: TFreeSubdivisionEdge;
  UP1,UP2: T2DCoordinate;
  Anchor1screen, Anchor2screen: TPoint;
begin
  if not Assigned(FActiveTexture) then exit;

  Viewport.Cursor := crDefault;

  FWorkModes := [wmViewport];
  if self.ToolButtonAnchors.Down then Include(FWorkModes, wmAnchors);
  if self.ToolButtonLayer.Down then Include(FWorkModes, wmPatch);
  if self.ToolButtonTexture.Down then Include(FWorkModes, wmBGimage);

  StatusBar1.Panels[0].Text := String.Format('Scr %0:d:%1:d',[X,Y]);
  P3 := Viewport.ProjectBackTo2D(Point(X,Y));
  StatusBar1.Panels[1].Text := String.Format('VP %0:.3f:%1:.3f',[P3.X, P3.Y]);

  FFoundBGimage := nil;
  Viewport.BackgroundMode := emUnsetFrame;
  if Assigned(Viewport.BackgroundImage.Bitmap)
      and (Viewport.BackgroundImage.Bitmap.Width > 0)
      and (Viewport.BackgroundImage.Bitmap.Height > 0)
  then
  begin
    bP := Viewport.BackgroundImage.ImageCoordinate(X,Y);
    StatusBar1.Panels[2].Text := String.Format('Img %0:d:%1:d',[bP.X,bP.Y]);
    if (bP.X>=0) and (bP.X<=Viewport.BackgroundImage.Bitmap.Width)
      and (bP.Y>=0) and (bP.Y<=Viewport.BackgroundImage.Bitmap.Height)
      and (wmBGimage in FWorkModes)
    then
    begin
      FFoundBGimage := Viewport.BackgroundImage;
      Viewport.Cursor := crSizeAll;
    end;
  end;


  FFoundAnchorNo := 0;
  FFoundAnchor2d := ZERO2D;
  FFoundEdge := nil;
  FFoundPoint := nil;
  MP:=nil;
  MP := FActiveTexture.FindSubdivionPointByScreen(X,Y,Viewport,5);
  if Assigned(MP) then
  begin
    FFoundPoint := MP;
    uP := FActiveTexture.FindUnrolledPointForSubdivionPoint(MP);
    if not IsNAN(uP.X) then
    begin
      FFoundUnrolledPoint := uP;

      if (Distance2D(uP, FActiveTexture.DevelopedPatchAnchorPoint1) < 0.001)
        then
        begin
          FFoundAnchor2d := FActiveTexture.DevelopedPatchAnchorPoint1;
          FFoundAnchorNo := 1;
        end;
      if (Distance2D(uP, FActiveTexture.DevelopedPatchAnchorPoint2) < 0.001)
        then
        begin
          FFoundAnchor2d := FActiveTexture.DevelopedPatchAnchorPoint2;
          FFoundAnchorNo := 2;
        end;

      if (wmPatch in FWorkModes) and (FFoundAnchorNo > 0)
      then Viewport.Cursor := crRotate2d;

      StatusBar1.Panels[3].Text := String.Format(
        'Plt %0:8.3f:%1:8.3f  Mdl %2:8.3f:%3:8.3f:%4:8.3f  Anchor:%5:d CM:%6:d',
        [uP.X, uP.Y,
         MP.Coordinate.X, MP.Coordinate.Y, MP.Coordinate.Z, FFoundAnchorNo,FControlMode]);
    end;
  end;

  if (wmAnchors in FWorkModes) then
  begin
    bP := Viewport.BackgroundImage.ImageCoordinate(X,Y);

    if (abs(FActiveTexture.BitmapTargetPoint2.X - bP.X) <= 3)
      and (abs(FActiveTexture.BitmapTargetPoint2.Y - bP.Y) <= 3)
    then FFoundAnchorNo := 2;

    if (abs(FActiveTexture.BitmapTargetPoint1.X - bP.X) <= 3)
      and (abs(FActiveTexture.BitmapTargetPoint1.Y - bP.Y) <= 3)
    then FFoundAnchorNo := 1;

    if (wmAnchors in FWorkModes) and (FFoundAnchorNo > 0)
    then
    begin
      Viewport.Cursor := crSizeAll;
      Viewport.Invalidate();
    end;

    StatusBar1.Panels[3].Text := String.Format(
      'Anchor:%0:d CM:%1:d', [FFoundAnchorNo,FControlMode]);
  end;


  if not Assigned(MP) then
  begin
    Edge := FActiveTexture.FindSubdivionEdgeByScreen(X,Y,Viewport,3,UP1,UP2);
    if Assigned(Edge) then
    begin
      FFoundEdge := Edge;
      Viewport.Cursor := crSizeAll;
    end;
  end;


  if FControlMode <> cmNone then
  if (FControlMode = cmViewportZoom) and (ssRight in Shift) then
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
  else if (FControlMode = cmViewportPan) and (ssLeft in Shift) then
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
  else if (FControlMode = cmAnchorMove) and (ssLeft in Shift)
     //and ((abs(FInitialAnchorPosition.X - X) > 0) or (abs(FInitialAnchorPosition.Y - Y) > 0))
    then
    begin
      //bP := Viewport.BackgroundImage.ImageCoordinate(X,Y);

      Anchor1screen := Viewport.Project(FActiveTexture.Project2DtoViewport(FActiveTexture.DevelopedPatchAnchorPoint1));
      Anchor2screen := Viewport.Project(FActiveTexture.Project2DtoViewport(FActiveTexture.DevelopedPatchAnchorPoint2));

      if FInitialFoundAnchorNo = 1 then Anchor1screen := Point(X,Y);
      if FInitialFoundAnchorNo = 2 then Anchor2screen := Point(X,Y);

      StatusBar1.Panels[3].Text := String.Format(
        'IniAnchr1Scr %0:5d:%1:5d  Anchr1Scr %2:5d:%3:5d  Anc %4:1d',
        [FInitialAnchor1screen.X, FInitialAnchor1screen.Y,
         Anchor1screen.X, Anchor1screen.Y, FInitialFoundAnchorNo]);

      CalculateNewPatchTransform(
        FInitialAnchor1screen, FInitialAnchor2screen,
        Anchor1screen, Anchor2screen);

      SetBitmapTargetPoints;
      FActiveTexture.IsManuallyAdjusted := true;
      FActiveTexture.IsCorelated := true;
      setControlsAndLabels();
      Viewport.Invalidate;
    end
  else if (FControlMode = cmPatchMove) and (ssLeft in Shift)
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
  else if (FControlMode = cmPatchScale) and (ssRight in Shift) then
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
  else if (FControlMode = cmPatchRotate) and (ssLeft in Shift) then
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
  else if (FControlMode = cmBgImageMove) and (ssLeft in Shift)
    and ((abs(FInitialBGimagePosition.X - X) > 0) or (abs(FInitialBGimagePosition.Y - Y) > 0)) then
    begin
      Po := FActiveTexture.BitmapOrigin;
      Pt := Viewport.Project(ZERO);
      Z := (Viewport.Scale * Viewport.Zoom * Viewport.BackgroundImage.Scale);
      P.X := round(Po.X + ((FInitialBGimagePosition.X - X))/Z);
      P.Y := round(Po.Y + ((FInitialBGimagePosition.Y - Y))/Z);

      FActiveTexture.BitmapOrigin := P;
      Viewport.BackgroundImage.Origin := P;
      Viewport.BackgroundMode := emSetFrame;

      FInitialBGimagePosition.X := X;
      FInitialBGimagePosition.Y := Y;
      FActiveTexture.IsManuallyAdjusted := true;
      SetBitmapTargetPoints;
      Viewport.Invalidate;
    end
  else if (FControlMode = cmBgImageScale) and (ssRight in Shift)then
    begin
      Viewport.BackgroundMode := emSetFrame;
    end;

  P.X := X;
  P.Y := Y;
  P2 := Viewport.ProjectBackTo2D(P);

  p3d.x:=P2.X; p3d.y:=P2.y; p3d.z:=0;
  P:=Viewport.Project(p3d);

  P3 := FActiveTexture.ProjectViewportTo2D(P2);

  //MP := FActiveTexture.FindSubdivionPoint(P3);

end;{TFreeTextureForm.ViewportMouseMove}

procedure TFreeTextureForm.ViewportMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  //Active: TFreeDevelopedPatch;
  I, Dist: integer;
begin
  FInitialPosition.X := X;
  FInitialPosition.Y := Y;
  FInitialAnchorPosition.X := X;
  FInitialAnchorPosition.Y := Y;
  FInitialPatchPosition.X := X;
  FInitialPatchPosition.Y := Y;
  FInitialBGimagePosition.X := X;
  FInitialBGimagePosition.Y := Y;
  FBitmapTargetPoint1old := FActiveTexture.BitmapTargetPoint1;
  FBitmapTargetPoint2old := FActiveTexture.BitmapTargetPoint2;

  FInitialFoundAnchorNo := FFoundAnchorNo;
  FInitialAnchor1VP := FActiveTexture.Project2DtoViewport(FActiveTexture.DevelopedPatchAnchorPoint1);
  FInitialAnchor2VP := FActiveTexture.Project2DtoViewport(FActiveTexture.DevelopedPatchAnchorPoint2);
  FInitialAnchor1screen := Viewport.Project(FActiveTexture.Project2DtoViewport(FActiveTexture.DevelopedPatchAnchorPoint1));
  FInitialAnchor2screen := Viewport.Project(FActiveTexture.Project2DtoViewport(FActiveTexture.DevelopedPatchAnchorPoint2));
  FInitialScale := FActiveTexture.Scale;
  FInitialRotation := FActiveTexture.Rotation;
  FInitialTranslation := FActiveTexture.Translation;

  //FAllowPanOrZoom := True;
  FWorkModes := [wmViewport];
  if self.ToolButtonAnchors.Down then Include(FWorkModes, wmAnchors);
  if self.ToolButtonLayer.Down then Include(FWorkModes, wmPatch);
  if self.ToolButtonTexture.Down then Include(FWorkModes, wmBGimage);

  FControlMode := cmNone;
  if (wmAnchors in FWorkModes) then
     if (FFoundAnchorNo > 0) and (ssLeft in Shift)
       then FControlMode := cmAnchorMove
  else if (wmPatch in FWorkModes) then
     if Assigned(FFoundEdge) and (ssLeft in Shift) then FControlMode := cmPatchMove
     else if Assigned(FFoundPoint) and (ssLeft in Shift) then FControlMode := cmPatchRotate
     else if Assigned(FFoundPoint) and (ssRight in Shift) then FControlMode := cmPatchScale
  else if (wmBGimage in FWorkModes) then
     if Assigned(FFoundBGimage) and (ssLeft in Shift) then FControlMode := cmBGimageMove
     else if Assigned(FFoundBGimage) and (ssRight in Shift) then FControlMode := cmBGimageScale
  else if (ssLeft in Shift) then FControlMode := cmViewportPan
  else if (ssRight in Shift) then FControlMode := cmViewportZoom;

  //SetBitmapTargetPoints;
end;{TFreeTextureForm.ViewportMouseDown}

procedure TFreeTextureForm.ViewportMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FWorkModes := [];
  FControlMode := cmNone;
  FFoundPoint := nil;
  FFoundEdge := nil;
  FFoundBGimage := nil;
end;{TFreeTextureForm.ViewportMouseUp}

procedure TFreeTextureForm.ViewportMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var S,d: TFloatType; oldVPPan:Tpoint; oldVPZoom: TFloatType;
begin
  FWorkModes := [wmViewport];
  if self.ToolButtonAnchors.Down then Include(FWorkModes, wmAnchors);
  if self.ToolButtonLayer.Down then Include(FWorkModes, wmPatch);
  if self.ToolButtonTexture.Down then Include(FWorkModes, wmBGimage);

  FControlMode := cmNone;
  if Assigned(FFoundEdge) then FControlMode := cmPatchScale
  else if Assigned(FFoundPoint) then FControlMode := cmPatchScale
  else if Assigned(FFoundPoint) then FControlMode := cmPatchScale
  else if Assigned(FFoundBGimage) then FControlMode := cmBGimageScale
  else if Assigned(FFoundBGimage) then FControlMode := cmBGimageScale
  else FControlMode := cmViewportZoom;

  if (wmViewport in FWorkModes)and(FControlMode = cmViewportZoom) then
  begin
  end
  else if (wmPatch in FWorkModes)and(FControlMode = cmPatchScale) then
    begin
      S := FActiveTexture.Scale;
      S := S + 0.01 * (WheelDelta/120);
      FloatSpinEditScale.OnChange := nil;
      FActiveTexture.Scale  := S;
      FloatSpinEditScale.Value := S;
      FloatSpinEditScale.OnChange := @FloatSpinEditScaleChange;
      FActiveTexture.IsManuallyAdjusted := true;
      SetBitmapTargetPoints;
      Viewport.Invalidate;
      Handled := true;
    end
  else if (wmBGimage in FWorkModes)and(FControlMode = cmBGimageScale) then
    begin
      S := Viewport.BackgroundImage.Scale;
      d := 0.01 * Viewport.BackgroundImage.Scale;
      Viewport.BackgroundImage.Scale := S + d * (WheelDelta/120);
      FActiveTexture.IsManuallyAdjusted := true;
      SetBitmapTargetPoints;
      Viewport.Invalidate;
      Handled := true;
    end;

  FControlMode := cmNone;
end;

procedure TFreeTextureForm.SetBitmapTargetPoints;
var p1,p2: T3DCoordinate;
  tp1, tp2, tp1cur, tp2cur: TPoint;
  z: TFloatType;
begin
  if (Assigned(ViewPort.BackgroundImage.Bitmap)
     and (ViewPort.BackgroundImage.Bitmap.Width > 0)
     and (ViewPort.BackgroundImage.Bitmap.Height > 0))
  then
  begin
    // remember current values
    tp1cur := FActiveTexture.BitmapTargetPoint1;
    tp2cur := FActiveTexture.BitmapTargetPoint2;

    p1 := FActiveTexture.Project2DtoViewport(FActiveTexture.DevelopedPatchAnchorPoint1);
    tp1 := ViewPort.Project(p1);
    FActiveTexture.BitmapTargetPoint1 := ViewPort.BackgroundImage.ImageCoordinate(tp1.X,tp1.Y);

    p2 := FActiveTexture.Project2DtoViewport(FActiveTexture.DevelopedPatchAnchorPoint2);
    tp2 := ViewPort.Project(p2);
    FActiveTexture.BitmapTargetPoint2 := ViewPort.BackgroundImage.ImageCoordinate(tp2.X,tp2.Y);

    FActiveTexture.IsCorelated := true;

    FActiveTexture.IsManuallyAdjusted := FActiveTexture.IsManuallyAdjusted or (
      (tp1cur <> FActiveTexture.BitmapTargetPoint1)
    or(tp2cur <> FActiveTexture.BitmapTargetPoint2) );

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
  PatchW, PatchH, SclW, SclH: TFloatType;
  Bm1,Bm2,P2D: T2DCoordinate; Bm3: T3DCoordinate;
begin
  Viewport.BackgroundImage.Visible := false;
  if not Assigned(FActiveTexture.Bitmap) then exit;
  Viewport.ZoomExtents;
  W := FActiveTexture.Bitmap.Width;
  H := FActiveTexture.Bitmap.Height;
  if (W=0) or (H=0) then exit;
  SclW := (Viewport.ClientWidth - 40) / W / Viewport.Scale;
  SclH := (Viewport.ClientHeight - 40) / H / Viewport.Scale;
  FActiveTexture.BitmapScale := SclW;
  if SclW > SclH then FActiveTexture.BitmapScale := SclH;
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

procedure TFreeTextureForm.AdjustPatchScaleTranslationAndRotation(origBTP1,origBTP2:TPoint);
var Pt0, vzP, Pt1, Pt2, bmP1,bmP2, vbP0,vbP1,vbP2, vmP1,vmP2: TPoint;
  mP1, mP2, trl : T2DCoordinate;
  vP1,vP2 : T3DCoordinate;
  bmR: TRect; Z, A, L, Scl, TrlX, TrlY, D0, D: TFloatType;
  vX, vY: longint;
begin
  bmP1 := FActiveTexture.BitmapTargetPoint1;
  bmP2 := FActiveTexture.BitmapTargetPoint2;
  mP1 := FActiveTexture.DevelopedPatchAnchorPoint1;
  mP2 := FActiveTexture.DevelopedPatchAnchorPoint2;

  D0 := sqrt(sqr(1.0*origBTP1.X - origBTP2.X) + sqr(1.0*origBTP1.Y - origBTP2.Y));
  D := sqrt(sqr(1.0*bmP1.X - bmP2.X) + sqr(1.0*bmP1.Y - bmP2.Y));
  Scl := D / D0;
  FActiveTexture.Scale := Scl;

  if bmP1.X < origBTP1.X then
  begin
    trl := FActiveTexture.Translation;
    trl.X := (mP1.X - mp2.X)/(1.0*origBTP1.X - origBTP2.X)
            *(1.0*bmP1.X - origBTP1.X);
    //FActiveTexture.Translation := trl;
  end;

  if bmP1.Y < origBTP1.Y then
  begin
    trl := FActiveTexture.Translation;
    trl.Y := (mP1.Y - mp2.Y)/(1.0*origBTP1.Y - origBTP2.Y)
            *(1.0*bmP1.Y - origBTP1.Y);
    //FActiveTexture.Translation := trl;
  end;

  Viewport.BackgroundImage.Origin := ToPoint(0,0);
  vbP1 := Viewport.BackgroundImage.TargetCoordinate(bmP1.X,bmP1.Y);
  vbP2 := Viewport.BackgroundImage.TargetCoordinate(bmP2.X,bmP2.Y);
  vmP1 := Viewport.Project(Point3D(mP1.X,mP1.Y,0));
  vmP2 := Viewport.Project(Point3D(mP2.X,mP2.Y,0));

  A := Angle( Point2D(vbP1.X,vbP1.Y), Point2D(vbP2.X,vbP2.Y),
              Point2D(vmP1.X,vmP1.Y), Point2D(vmP2.X,vmP2.Y));
  FActiveTexture.Rotation := -A;

  {
  vP1 := FActiveTexture.Project2DtoViewport(mP1);
  vP2 := FActiveTexture.Project2DtoViewport(mP2);
  vmP1 := Viewport.Project(vP1);
  vmP2 := Viewport.Project(vP2);

  vzP := Viewport.Project(ZERO);
  bmR := Viewport.BackgroundImage.TargetRect;

  Pt1 := Viewport.BackgroundImage.ImageCoordinate(bmR.Left + (vmP1.X - vbP1.X), bmR.Top + (vmP1.Y - vbP1.Y));
  }
end;


  // --- Helper Functions ---

  // Calculates the Euclidean distance between two points.
  function Distance(P1, P2: TPoint): Single;
  begin
    Result := Sqrt(Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y));
  end;

  // Applies rotation and scale to a model point, assuming rotation is around (0,0).
  // Returns the scaled and rotated point.
  function ApplyRotationAndScale(const ModelPoint: T2DCoordinate;
    const ScaleFactor: Single; const AngleRad: Single): T2DCoordinate;
  var
    CosR, SinR: Single;
  begin
    CosR := Cos(AngleRad);
    SinR := Sin(AngleRad);

    // X' = (X * CosR - Y * SinR) * Scale
    Result.X := (ModelPoint.X * CosR - ModelPoint.Y * SinR) * ScaleFactor;

    // Y' = (X * SinR + Y * CosR) * Scale
    Result.Y := (ModelPoint.X * SinR + ModelPoint.Y * CosR) * ScaleFactor;
  end;

procedure TFreeTextureForm.CalculateNewPatchTransform(
    const OldScreen1, OldScreen2: TPoint;
    const NewScreen1, NewScreen2: TPoint
    );
var
  OldAnchor1VP, OldAnchor2VP: T3DCoordinate;
  NewAnchor1VP, NewAnchor2VP: T3DCoordinate;
  NewScrAnchor1VP, NewScrAnchor2VP, OldTxAnchor1VP, OldTxAnchor2VP: T2DCoordinate;
  NewScale: TFloatType;
  NewRotation: TFloatType;
  NewTranslation: T2DCoordinate;
begin
  NewScrAnchor1VP := Viewport.ProjectBackTo2D(NewScreen1);
  NewScrAnchor2VP := Viewport.ProjectBackTo2D(NewScreen2);

  FActiveTexture.CalculateNewTransform(
    NewScrAnchor1VP, NewScrAnchor2VP
  );

  StatusBar1.Panels[3].Text := String.Format(
    'Anchor1VP %0:8.5f:%1:8.5f  Anchor2VP %2:8.5f:%3:8.5f  Tx %4:8.5f:%5:8.5f',
    [NewScrAnchor1VP.X, NewScrAnchor1VP.Y,
     NewScrAnchor2VP.X, NewScrAnchor2VP.Y,
     FActiveTexture.Translation.X, FActiveTexture.Translation.Y]);
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

  ComboBoxWrapMode.ItemIndex := ord( FActiveTexture.WrapMode );
  ColorButton1.ButtonColor := FActiveTexture.Color;
  CheckBoxSymmetric.Checked := FActiveTexture.Symmetric;
  CheckBoxSymmetric.Enabled := FActiveTexture.Layer.Symmetric;

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


  SpinEditTextureShiftX.OnChange := nil;
  SpinEditTextureShiftY.OnChange := nil;
  FloatSpinEditTextureScale.OnChange := nil;

  SpinEditTextureShiftX.Value := FActiveTexture.BitmapOrigin.X;
  SpinEditTextureShiftY.Value := FActiveTexture.BitmapOrigin.Y;
  FloatSpinEditTextureScale.Value := FActiveTexture.BitmapScale;

  SpinEditTextureShiftX.OnChange := @SpinEditTextureShiftXChange;
  SpinEditTextureShiftY.OnChange := @SpinEditTextureShiftYChange;
  FloatSpinEditTextureScale.OnChange := @FloatSpinEditTextureScaleChange;


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

procedure TFreeTextureForm.SpinEditTextureShiftXChange(Sender: TObject);
begin
  FActiveTexture.BitmapOrigin.X := SpinEditTextureShiftX.Value;
  Viewport.BackgroundImage.Origin := FActiveTexture.BitmapOrigin;
  FActiveTexture.IsManuallyAdjusted := true;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;

procedure TFreeTextureForm.SpinEditTextureShiftYChange(Sender: TObject);
begin
  FActiveTexture.BitmapOrigin.Y := SpinEditTextureShiftY.Value;
  Viewport.BackgroundImage.Origin := FActiveTexture.BitmapOrigin;
  FActiveTexture.IsManuallyAdjusted := true;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;

procedure TFreeTextureForm.FloatSpinEditTextureScaleChange(Sender: TObject);
begin
  FActiveTexture.BitmapScale := FloatSpinEditTextureScale.Value;
  Viewport.BackgroundImage.Scale := FloatSpinEditTextureScale.Value;
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
    FActiveTexture.Symmetric := FActiveTexture.Layer.Symmetric;
    CheckBoxSymmetric.Checked := FActiveTexture.Symmetric;
    CheckBoxSymmetric.Enabled := FActiveTexture.Layer.Symmetric;

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
  FTextures.DeleteItem(FActiveTexture);
  FLayer.Textures.DeleteItem(FActiveTexture);
  FreeAndNil(FActiveTexture);
end;

procedure TFreeTextureForm.ComboBoxWrapModeSelect(Sender: TObject);
begin
  case ComboBoxWrapMode.ItemIndex of
    0: FActiveTexture.WrapMode := twmNone;
    1: FActiveTexture.WrapMode := twmColor;
    2: FActiveTexture.WrapMode := twmTile;
  end;
end;

procedure TFreeTextureForm.CheckBoxSymmetricChange(Sender: TObject);
begin
  FActiveTexture.Symmetric := CheckBoxSymmetric.Checked;
end;

procedure TFreeTextureForm.ViewportRedraw(Sender: TObject);
var
  Pt0, Pt: TPoint;
begin
  if not Assigned(FActiveTexture) then exit;
  FActiveTexture.Color := ColorButton1.ButtonColor;
  FActiveTexture.Symmetric := CheckBoxSymmetric.Checked;
  if FFoundAnchorNo>0 then
    FActiveTexture.HiglightMode := thmAnchor;
  FActiveTexture.Draw(Viewport);
end;{TFreeTextureForm.ViewportRedraw}

end.

