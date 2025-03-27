unit FreeTextureDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ExtDlgs, ActnList, ComCtrls, StdCtrls,
  Math,
  FreeTypes, FreeGeometry,
  FreeShipUnit,
  LCLTranslator, Spin,
  FreeStringUtils;

type

  { TFreeTextureForm }

  TFreeTextureForm = class(TForm)
    Button1: TButton;
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
    ShowInteriorEdges: TAction;
    CloseDialog: TAction;
    LoadFile: TAction;
    ActionList1: TActionList;
    ComboBoxSelectTexture: TComboBox;
    ImageList1: TImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    PanelTop: TPanel;
    PanelRight: TPanel;
    SpeedButtonClose: TSpeedButton;
    SpeedButtonOpenImage: TSpeedButton;
    SpinEditRotate: TSpinEdit;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButtonShowInteriorEdges: TToolButton;
    Viewport: TFreeViewport;
    procedure Button1Click(Sender: TObject);
    procedure CloseDialogExecute(Sender: TObject);
    procedure ComboBoxSelectTextureSelect(Sender: TObject);
    procedure CreateUnrolledPatches;
    procedure FitBitmap;
    procedure FloatSpinEditShiftXChange(Sender: TObject);
    procedure FloatSpinEditShiftYChange(Sender: TObject);
    procedure FloatSpinEditScaleChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LoadFileExecute(Sender: TObject);
    procedure SpinEditRotateChange(Sender: TObject);
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
    FPatchScale: TFloatType;
    FPatchAngle: TFloatType;
    FImageScale: TFloatType;
    FImageAngle: TFloatType;
    FFontSize: integer;
    FXGridSpacing: integer;
    FYGridSpacing: integer;
    FAllowPanOrZoom: boolean;
    FInitialPosition: TPoint;
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
  Texture: TFreeTexture;
begin
  DPL:= TFasterListTFreeDevelopedPatch.Create;
  if not FLayer.Surface.Built then FLayer.Surface.Rebuild;
  FLayer.Unroll(DPL);
  FTextures.Clear;
  for i:=0 to DPL.Count-1 do
    if (not FLayer.Symmetric) or (FLayer.Symmetric and (DPL[i].Side=fsPort)) then
    begin
      Texture := TFreeTexture.Create(FLayer, DPL[i]);
      FTextures.Add(Texture);
      ComboBoxSelectTexture.AddItem(Texture.DevelopedPatchName, Texture);
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
  Freeship.Preferences.LoadImageListByActions(ImageList1, ActionList1);

  //FUpdateListBox;

  ShowInteriorEdges.Checked := FreeShip.Visibility.ShowInteriorEdges;

  FFontSize:=8;
  FXGridSpacing:=20;
  FYGridSpacing:=20;
  FAllowPanOrZoom:=true;
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
  P, bP: TPoint;
  P1, P2, P3, uP: T2DCoordinate;
  p3d:T3DCoordinate;
  Diff: T2DCoordinate;
  Patch: TFreeDevelopedPatch;
  MP: TFreeSubdivisionPoint;
begin

  if FAllowPanOrZoom then
  begin
    if ssLeft in Shift then
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
        SetBitmapTargetPoints;
      end;
    end
    else if ssRight in Shift then
      if (abs(FInitialPosition.X - X) > 4) or (abs(FInitialPosition.Y - Y) > 4) then
      begin
        P.X := Viewport.Pan.X + X - FInitialPosition.X;
        P.Y := Viewport.Pan.Y + Y - FInitialPosition.Y;
        Viewport.Pan := P;
        FInitialPosition.X := X;
        FInitialPosition.Y := Y;
        SetBitmapTargetPoints;
      end// Pan the window left, right, top or bottom
    ;
  end
  else
  begin
    //Patch := FActiveTexture.DevelopedPatch;
    if (ssLeft in Shift) then //and (Patch <> nil) then
      if (abs(FInitialPosition.X - X) > 0) or (abs(FInitialPosition.Y - Y) > 0) then
      begin
        P.X := X;
        P.Y := Y;
        P1 := Viewport.ProjectBackTo2D(FInitialPosition);
        P2 := Viewport.ProjectBackTo2D(P);
        Diff.X := FActiveTexture.Translation.X + (P2.X - P1.X);
        Diff.Y := FActiveTexture.Translation.Y + (P2.Y - P1.Y);
        FActiveTexture.Translation := Diff;
        Viewport.Refresh;
        FInitialPosition.X := X;
        FInitialPosition.Y := Y;
      end;// Translate the selected patch
  end;

  P.X := X;
  P.Y := Y;
  P2 := Viewport.ProjectBackTo2D(P);

  p3d.x:=P2.X; p3d.y:=P2.y; p3d.z:=0;
  P:=Viewport.Project(p3d);

  P3 := FActiveTexture.ProjectViewportTo2D(P2);

  //MP := FActiveTexture.FindSubdivionPoint(P3);

  StatusBar1.Panels[0].Text := String.Format('Scr %0:d:%1:d',[X,Y]);

  if Assigned(Viewport.BackgroundImage.Bitmap)
      and (Viewport.BackgroundImage.Bitmap.Width > 0)
      and (Viewport.BackgroundImage.Bitmap.Height > 0) then
  begin
    bP := Viewport.BackgroundImage.ImageCoordinate(X,Y);
    StatusBar1.Panels[1].Text := String.Format('Bmp %0:d:%1:d',[bP.X,bP.Y]);
  end;

  StatusBar1.Panels[2].Text := String.Format('Viw %0:.3f:%1:.3f',[P3.X, P3.Y]);

  MP:=nil;
  MP := FActiveTexture.FindSubdivionPointByScreen(X,Y,Viewport);
  if Assigned(MP) then
  begin
    uP := FActiveTexture.FindUnrolledPointForSubdivionPoint(MP);
    if not IsNAN(uP.X) then
    begin
      StatusBar1.Panels[3].Text := String.Format(
        'Plt %0:8.3f:%1:8.3f  Mdl %2:8.3f:%3:8.3f:%4:8.3f',
        [uP.X, uP.Y,
         MP.Coordinate.X, MP.Coordinate.Y, MP.Coordinate.Z]);

      bP := FActiveTexture.ProjectOnBitmap(uP);
      StatusBar1.Panels[0].Text := String.Format('Txt %0:d:%1:d',[bP.X,bP.Y]);
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
  FAllowPanOrZoom := True;
  SetBitmapTargetPoints;

  {if Button = mbLeft then
    for I := FPlates.Count downto 1 do
    begin
      Active := FPlates[I - 1];
      if Active.Visible then
      begin
        Dist := Active.DistanceToCursor(X, Y, Viewport);
        if Dist <= Active.Owner.Surface.ControlPointSize then
        begin
          if ActivePatch <> Active then
            ActivePatch := Active;
          FAllowPanOrZoom := False;
          break;
        end;
      end;
    end;}
end;{TFreeTextureForm.ViewportMouseDown}

procedure TFreeTextureForm.ViewportMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (not FAllowPanOrZoom) and (Viewport.Zoom = 1.0) then
    Viewport.ZoomExtents;
  FAllowPanOrZoom := True;
end;{TFreeTextureForm.ViewportMouseUp}


procedure TFreeTextureForm.SetBitmapTargetPoints;
var p1,p2: T3DCoordinate;
  tp1, tp2: TPoint;
  z: TFloatType;
begin
  LabelAnchor1.Caption:= String.Format('%0:.3f : %1:.3f',
    [FActiveTexture.DevelopedPatchAnchorPoint1.X, FActiveTexture.DevelopedPatchAnchorPoint1.Y]);

  LabelAnchor2.Caption:= String.Format('%0:.3f : %1:.3f',
    [FActiveTexture.DevelopedPatchAnchorPoint2.X, FActiveTexture.DevelopedPatchAnchorPoint2.Y]);

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

    LabelTarget1.Caption:= String.Format('%0:d : %1:d',
      [FActiveTexture.BitmapTargetPoint1.X, FActiveTexture.BitmapTargetPoint1.Y]);

    LabelTarget2.Caption:= String.Format('%0:d : %1:d',
      [FActiveTexture.BitmapTargetPoint2.X, FActiveTexture.BitmapTargetPoint2.Y]);
  end;
end;

procedure TFreeTextureForm.ViewportRequestExtents(Sender: TObject;
  var Min, Max: T3DCoordinate);
begin
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
  Viewport.ZoomExtents;
  if not Assigned(FActiveTexture.Bitmap) then exit;
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
  SetBitmapTargetPoints;
end;

procedure TFreeTextureForm.ComboBoxSelectTextureSelect(Sender: TObject);
var Pt0, Pt, Pt1, Pt2: TPoint; W,H: integer;
  PatchW, PatchH: TFloatType;
  Bm2: T2DCoordinate; Bm3: T3DCoordinate;
begin
  FActiveTexture := TFreeTexture(ComboBoxSelectTexture.Items.Objects[ComboBoxSelectTexture.ItemIndex]);
  Viewport.ZoomExtents;
  if Assigned(FActiveTexture.Bitmap) then
  begin
    Viewport.BackgroundImage.Bitmap := FActiveTexture.Bitmap;
    Viewport.BackgroundImage.Origin := FActiveTexture.BitmapOrigin;
    Viewport.BackgroundImage.Scale := FActiveTexture.BitmapScale;
    Viewport.BackgroundImage.ShowInView := Viewport.ViewType;
    Viewport.BackgroundImage.Visible := true;
    Viewport.BackgroundMode:=emNormal;
    if (not FActiveTexture.IsCorelated)
       then FitBitmap;
  end
  else
    Viewport.BackgroundImage.Visible := false;

  SpinEditRotate.OnChange := nil;
  FloatSpinEditShiftX.OnChange := nil;
  FloatSpinEditShiftY.OnChange := nil;
  FloatSpinEditScale.OnChange := nil;

  SpinEditRotate.Value := FActiveTexture.Rotation;
  FloatSpinEditShiftX.Value := FActiveTexture.Translation.X;
  FloatSpinEditShiftY.Value := FActiveTexture.Translation.Y;
  FloatSpinEditScale.Value := FActiveTexture.Scale;

  SpinEditRotate.OnChange := @SpinEditRotateChange;
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

  Viewport.Invalidate;
end;

procedure TFreeTextureForm.FloatSpinEditShiftXChange(Sender: TObject);
var t:T2DCoordinate;
begin
  t.X := FloatSpinEditShiftX.Value;
  t.Y := FloatSpinEditShiftY.Value;
  FActiveTexture.Translation := t;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;

procedure TFreeTextureForm.FloatSpinEditShiftYChange(Sender: TObject);
var t:T2DCoordinate;
begin
  t.X := FloatSpinEditShiftX.Value;
  t.Y := FloatSpinEditShiftY.Value;
  FActiveTexture.Translation := t;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;

procedure TFreeTextureForm.FloatSpinEditScaleChange(Sender: TObject);
begin
  FActiveTexture.Scale := FloatSpinEditScale.Value;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;

procedure TFreeTextureForm.SpinEditRotateChange(Sender: TObject);
begin
  FActiveTexture.Rotation := SpinEditRotate.Value;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;

procedure TFreeTextureForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Viewport.BackgroundImage.Visible:=false;
  //Viewport.BackgroundImage.Bitmap.Free;
  Viewport.BackgroundImage.Bitmap:=nil;
end;

procedure TFreeTextureForm.FormCreate(Sender: TObject);
begin

end;

procedure TFreeTextureForm.LoadFileExecute(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    FActiveTexture.LoadIntfImage(OpenPictureDialog1.FileName);
    FActiveTexture.IsCorelated := false;
    FitBitmap;
  end;
end;

procedure TFreeTextureForm.CloseDialogExecute(Sender: TObject);
begin
  ModalResult := mrOk;
  //Close;
end;

procedure TFreeTextureForm.Button1Click(Sender: TObject);
begin
  FActiveTexture.FindOptimalRotation;
  SpinEditRotate.Value := FActiveTexture.Rotation;
  FActiveTexture.AutoSetDevelopedPatchAnchorPoints;
  SetBitmapTargetPoints;
  Viewport.Invalidate;
end;


procedure TFreeTextureForm.ViewportRedraw(Sender: TObject);
var
  Pt0, Pt: TPoint;
begin

  FActiveTexture.Draw(Viewport);

end;{TFreeTextureForm.ViewportRedraw}

end.

