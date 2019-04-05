unit MDIPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  ExtCtrls, ComCtrls, Buttons, Menus; //, ActnList;

type
  TWindowPositionState = (wpsNone, wpsMoving, wpsResizing);
  TWindowResizingSide = (
    wrszsNW, wrszsN, wrszsNE,
    wrszsW, wrszsZ, wrszsE,
    wrszsSW, wrszsS, wrszsSE);

  TCaptionButton = ( // Form title bar items
    cbSystemMenu, // system menu
    cbMinimize,   // minimize button
    cbMaximize,   // maximize button
    cbRestore,   // minimize button
    cbClose
    );
  TCaptionButtons = set of TCaptionButton;

type
  TMDIClientPanel = class(TPanel)
    public
      procedure ActiveDefaultControlChanged(NewControl: TControl); override;
  end;

type
  TMDIPanel = class(TCustomPanel)
    CaptionPanel: TPanel;
    ClientPanel: TMDIClientPanel; //TScrollBox;
    SystemButton: TSpeedButton;
    CloseButton: TSpeedButton;
    MaximizeButton: TSpeedButton;
    MinimizeButton: TSpeedButton;
    RestoreButton: TSpeedButton;
    SystemPopupMenu: TPopupMenu;
    MenuItemMinimize: TMenuItem;
    MenuItemMaximize: TMenuItem;
    MenuItemRestore: TMenuItem;
  private
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnClose: TCloseEvent;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnHelp: THelpEvent;
    FOnHide: TNotifyEvent;
    FOnShortcut: TShortCutEvent;
    FOnShow: TNotifyEvent;

    FNormalBounds: Trect; // bounds when not maximized, minimized or hidden
    FWindowState: TWindowState;
    FCornerSize: integer;
    FBorderColor: TColor;
    FActiveBorderColor: TColor;
    FInactiveBorderColor: TColor;
    FActive: boolean;
    FParentForm: TCustomForm;
    FPassiveBevel: TGraphicControl;
    FClickProxies: TFPList;
    FCaptionButtons: TCaptionButtons;
    FWindowResizingSide: TWindowResizingSide;

  private

    WindowPositionState: TWindowPositionState;
    WindowCaptionMouseX, WindowCaptionMouseY: integer;

    function getUniqueName(nameBase: string): string;
    procedure CreateCaptionPanel;
    procedure CreateClientPanel;
    procedure CreateSystemPopupMenu;
    //procedure CreateSystemActions;

    function drawCloseIcon(size: integer): TBitmap;
    function drawMaximizeIcon(size: integer): TBitmap;
    function drawMinimizeIcon(size: integer): TBitmap;
    function drawRestoreIcon(size: integer): TBitmap;

    procedure SystemButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure DoClose(CloseAction: TCloseAction);
    procedure MaximizeButtonClick(Sender: TObject);
    procedure DoMaximize;
    procedure MinimizeButtonClick(Sender: TObject);
    procedure DoMinimize;
    procedure RestoreButtonClick(Sender: TObject);
    procedure DoRestore;
    procedure OrderButtons;

    //procedure ActionActionSystemMenuExecute(Sender: TObject);

    procedure CaptionPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure CaptionPanelMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure CaptionPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

    procedure BorderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BorderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure BorderMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SetBorderCursor(X, Y: integer);

    function deriveCaptionHeight:integer;
    function deriveCaptionIconHeight:integer;

    procedure SetCaption(const Value: TCaption);
    function GetCaption: TCaption;
    function GetControl(const Index: integer): TControl;
    function GetControlCount: integer;

    procedure setMouseClickProxiesRecursively(Ctrl: TControl);
    procedure unsetMouseClickProxiesRecursively(Ctrl: TControl);
    procedure setMouseClickProxies;
    procedure unsetMouseClickProxies;
    procedure PassivePanelOnClick(Sender: TObject);

  protected
    procedure Deactivate; virtual;
    procedure Paint; override;
    function  GetBorderColor: TColor;
    procedure SetActiveBorderColor(AValue: TColor);
    procedure SetInactiveBorderColor(AValue: TColor);
    procedure SetParent(NewParent: TWinControl); override;
    procedure AdjustClientRect(var ARect: TRect); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure InsertControl(AControl: TControl);
    procedure InsertControl(AControl: TControl; Index: integer); override;
    procedure RemoveControl(AControl: TControl); override;

    procedure setActive(val: boolean);
    procedure InactivateSiblings;

    function  getIcon :TIcon;
    procedure setIcon(val:TIcon);
    property Controls[Index: integer]: TControl read GetControl;
    property ControlCount: integer read GetControlCount;

  published
    property Active: boolean read FActive write setActive;
    property BorderColor: TColor read GetBorderColor;
    property ActiveBorderColor: TColor read FActiveBorderColor write SetActiveBorderColor default clDefault;
    property InactiveBorderColor: TColor read FInactiveBorderColor write SetInactiveBorderColor default clDefault;
    property Caption: TCaption read GetCaption write SetCaption;
    property Icon: TIcon read getIcon write setIcon;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnClose: TCloseEvent read FOnClose write FOnClose;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TMouseClickProxy = class
  private
    FDestroyed: boolean;
    FOwner: TControl;
    FOwnerOnClick: TNotifyEvent;
    FProxyOnClick: TNotifyEvent;
  public
    constructor Create(TheOwner: TControl);
    destructor Destroy; override;
    procedure OnClick(Sender: TObject);
    property OwnerOnClick: TNotifyEvent read FOwnerOnClick;
    property ProxyOnClick: TNotifyEvent read FProxyOnClick write FProxyOnClick;
    class function IsOnClick(P: TNotifyEvent): boolean;
  end;

implementation

uses  graphtype, intfgraphics, lazcanvas, LCLType, FPImage,
  types, lclproc, lcl;

constructor TMDIPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //UpdateSysColorMap();

  FClickProxies := TFPList.Create;
  Name := getUniqueName('MDIPanel');
  Color := clForm;
  FActive:=false;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  BevelWidth := 1;

  BorderStyle := bsNone; //bsSingle;
  BorderWidth := 4;
  FActiveBorderColor := clDefault;
  FInactiveBorderColor := clDefault;

  FCornerSize := 10;
  FWindowState := wsNormal;
  WindowPositionState := wpsNone;
  FCaptionButtons := [cbSystemMenu, cbMinimize, cbMaximize, cbRestore, cbClose];
  CreateClientPanel;
  CreateCaptionPanel;
  CreateSystemPopupMenu;
  OrderButtons;
  SetActive(false);
  self.setMouseClickProxies;
  Invalidate;
end;

destructor TMDIPanel.Destroy;
begin
  unsetMouseClickProxies;
  FClickProxies.Free;
  if assigned(onDestroy) then onDestroy(Self);
  inherited Destroy;
end;

function TMDIPanel.getUniqueName(nameBase: string): string;
var
  i: integer;
begin
  i := 1;
  while assigned(Owner.FindComponent(nameBase + IntToStr(i))) do
    Inc(i);
  Result := nameBase + IntToStr(i);
end;

function createCompatibleBpp32LazCanvas(w,h: integer):TLazCanvas;
var AlphaShift: integer;
  AImage: TLazIntfImage;
  ACanvas: TLazCanvas;
  lRawImage: TRawImage;
begin
  lRawImage.Description:=GetDescriptionFromDevice(0);
  AlphaShift := lRawImage.Description.AlphaShift;
  lRawImage.Init;
  if AlphaShift = 0 then
    lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(w, h) // linux
  else
    lRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(w, h); // windows
  lRawImage.CreateData(True);
  AImage := TLazIntfImage.Create(0, 0);
  AImage.SetRawImage(lRawImage);
  ACanvas := TLazCanvas.Create(AImage);
  result:=ACanvas;
end;

function TMDIPanel.drawCloseIcon(size: integer): TBitmap;
var
  scale, i, r, k: integer;
  ACanvas: TLazCanvas;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  ACanvas := createCompatibleBpp32LazCanvas(size, size);

  scale := size div 8;
  ACanvas.Pen.FPColor := FPColor(0, 0, 0, $FFFF);
  i := scale * 2;
  r := size - i;
  ACanvas.Pen.Width := 1;

  ACanvas.Line(i, i, r, r);
  for k := 0 to scale do
  begin
    ACanvas.Line(i + k, i, r, r - k);
    ACanvas.Line(i, i + k, r - k, r);
  end;

  ACanvas.Line(i, r, r, i);
  for k := 0 to scale do
  begin
    ACanvas.Line(i, r - k, r - k, i);
    ACanvas.Line(i + k, r, r, i + k);
  end;

  TLazIntfImage(ACanvas.Image).CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  Result := TBitmap.Create;
  Result.Handle := ImgHandle;
  Result.MaskHandle := ImgMaskHandle;

  ACanvas.Image.Free;
  ACanvas.Free;
end;

function TMDIPanel.drawMaximizeIcon(size: integer): TBitmap;
var
  scale, i, r: integer;
  ACanvas: TLazCanvas;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  ACanvas := createCompatibleBpp32LazCanvas(size, size);

  scale := size div 8;
  i := scale;
  r := size - i;
  ACanvas.Pen.FPColor := FPColor(0, 0, 0, $FFFF);
  ACanvas.Pen.Width := scale;
  ACanvas.Rectangle(i, i, r, r);

  TLazIntfImage(ACanvas.Image).CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  Result := TBitmap.Create;
  Result.Handle := ImgHandle;
  Result.MaskHandle := ImgMaskHandle;

  ACanvas.Image.Free;
  ACanvas.Free;
end;

function TMDIPanel.drawMinimizeIcon(size: integer): TBitmap;
var
  scale, i, r : integer;
  ACanvas: TLazCanvas;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  ACanvas := createCompatibleBpp32LazCanvas(size, size);

  scale := size div 8;
  ACanvas.Pen.FPColor := FPColor(0, 0, 0, $FFFF);
  i := scale * 2;
  r := size - i;
  ACanvas.Pen.Width := scale;
  ACanvas.Line(i, size div 2, r, size div 2);

  TLazIntfImage(ACanvas.Image).CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  Result := TBitmap.Create;
  Result.Handle := ImgHandle;
  Result.MaskHandle := ImgMaskHandle;

  ACanvas.Image.Free;
  ACanvas.Free;
end;

function TMDIPanel.drawRestoreIcon(size: integer): TBitmap;
var
  scale, i, r : integer;
  ACanvas: TLazCanvas;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  ACanvas := createCompatibleBpp32LazCanvas(size, size);

  scale := size div 8;
  ACanvas.Brush.FPColor := FPColor(0, 0, 0, 0);
  ACanvas.FillRect(0, 0, size, size);
  ACanvas.Pen.FPColor := FPColor(0, 0, 0, $FFFF);
  i := scale * 2;
  r := size - i;
  ACanvas.Pen.Width := scale;
  ACanvas.Rectangle(i, i, r, r);

  TLazIntfImage(ACanvas.Image).CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  Result := TBitmap.Create;
  Result.Handle := ImgHandle;
  Result.MaskHandle := ImgMaskHandle;

  ACanvas.Image.Free;
  ACanvas.Free;
end;

function TMDIPanel.deriveCaptionHeight:integer;
var h:integer; fd:TFontData;
begin
  //detect default font height to derive caption height
  fd := GetFontData(CaptionPanel.Font.Handle);
  // TFontData.Height is in logical units, convert it to pixels
  h := abs(round(1.0 * fd.Height * Font.PixelsPerInch / 72.0));
  result := h + 4; // add 4 so font does not touch borders
end;

function TMDIPanel.deriveCaptionIconHeight:integer;
var h:integer;
begin
  h:=deriveCaptionHeight;
  if (h < 24) then
    result := 16
  else if (h < 34) then
    result := 22
  else
    result := 32;
end;

procedure TMDIPanel.CreateCaptionPanel;
var
  pic: TPicture;
  h, fppi, IconSize: integer;
  IconSizeString: string;
  icn: TIcon;
begin
  OnMouseMove := @BorderMouseMove;
  OnMouseDown := @BorderMouseDown;
  OnMouseMove := @BorderMouseMove;
  OnMouseUp := @BorderMouseUp;

  CaptionPanel := TPanel.Create(Self);
  with CaptionPanel do
  begin
    Parent := Self;
    Name := 'CaptionPanel';
    //Height := 26;
    AutoSize := True;
    Align := alTop;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    BorderStyle := bsNone; //bsSingle;
    BorderWidth := 0;
    Caption := 'MDIPanel';
    Color := clInactiveCaption;
    Font.Color := clCaptionText;
    Font.Style := [fsBold];
    ParentColor := False;
    ParentFont := False;
    TabStop := False;
    OnMouseDown := @CaptionPanelMouseDown;
    OnMouseMove := @CaptionPanelMouseMove;
    OnMouseUp := @CaptionPanelMouseUp;
  end;

  h := deriveCaptionHeight;
  CaptionPanel.Constraints.MinHeight := h;

  IconSize:=deriveCaptionIconHeight;
  IconSizeString := IntToStr(IconSize) + 'x' + IntToStr(IconSize);
  IconSizeString := 'X';

  if cbSystemMenu in FCaptionButtons then
  begin
    SystemButton := TSpeedButton.Create(Self);
    with SystemButton do
    begin
      Parent := CaptionPanel;
      Align := alLeft;
      onClick := @SystemButtonClick;
      AutoSize := True;
      ParentFont := False;
      Font.Color := clWindowText;
      Font.Style := [fsBold];
      Transparent := true;
    end;
  end;

  if cbClose in FCaptionButtons then
  begin
    CloseButton := TSpeedButton.Create(Self);
    with CloseButton do
    begin
      Parent := CaptionPanel;
      Left := 200;
      Align := alRight;
      onClick := @CloseButtonClick;
      {pic:=TPicture.Create;
      try
        pic.LoadFromFile('Images/'+IconSizeString+'/window-close.png');
        Glyph.Assign(pic.Bitmap);
      except
      end;
      pic.Free;}
      AutoSize := True;
      ParentFont := False;
      Font.Color := clWindowText;
      Font.Style := [fsBold];
      if not assigned(Glyph) or (Glyph.Height = 0)
      //then Caption := ' x ';
      then
        Glyph.Assign(drawCloseIcon(IconSize));
    end;
  end;

  if cbMaximize in FCaptionButtons then
  begin
    MaximizeButton := TSpeedButton.Create(Self);
    with MaximizeButton do
    begin
      Parent := CaptionPanel;
      Left := 100;
      Align := alRight;
      AutoSize := True;
      AllowAllUp := True;
      onClick := @MaximizeButtonClick;
      {pic:=TPicture.Create;
      try
        pic.LoadFromFile('Images/'+IconSizeString+'/view-fullscreen.png');
        Glyph.Assign(pic.Bitmap);
      except
      end;
      pic.Free;}
      Font.Color := clWindowText;
      Font.Style := [fsBold];
      ParentFont := False;
      if not assigned(Glyph) or (Glyph.Height = 0)
      //then Caption := '[ ]';
      then
        Glyph.Assign(drawMaximizeIcon(IconSize));
    end;
  end;

  if cbMinimize in FCaptionButtons then
  begin
    MinimizeButton := TSpeedButton.Create(Self);
    with MinimizeButton do
    begin
      Parent := CaptionPanel;
      Left := 80;
      Align := alRight;
      AutoSize := True;
      AllowAllUp := True;
      onClick := @MinimizeButtonClick;
      {pic:=TPicture.Create;
      try
        pic.LoadFromFile('Images/'+IconSizeString+'/window-minimize.png');
        Glyph.Assign(pic.Bitmap);
      except
      end;
      pic.Free;}
      Font.Color := clWindowText;
      Font.Style := [fsBold];
      ParentFont := False;
      if not assigned(Glyph) or (Glyph.Height = 0) then
        Glyph.Assign(drawMinimizeIcon(IconSize));
    end;
  end;

  if cbRestore in FCaptionButtons then
  begin
    RestoreButton := TSpeedButton.Create(Self);
    with RestoreButton do
    begin
      Parent := CaptionPanel;
      Left := 60;
      Align := alRight;
      AutoSize := True;
      AllowAllUp := True;
      onClick := @RestoreButtonClick;
      {pic:=TPicture.Create;
      try
        pic.LoadFromFile('Images/'+IconSizeString+'/window-restore.png');
        Glyph.Assign(pic.Bitmap);
      except
      end;
      pic.Free;}
      Font.Color := clWindowText;
      Font.Style := [fsBold];
      ParentFont := False;
      if not assigned(Glyph) or (Glyph.Height = 0) then
        Glyph.Assign(drawRestoreIcon(IconSize));
      Visible := False;
    end;
  end;
end;

procedure TMDIPanel.Deactivate;
begin
  setActive(False);
end;

function TMDIPanel.getIcon :TIcon;
begin
  result := TIcon.Create;
  result.Assign(SystemButton.Glyph);
end;

procedure TMDIPanel.setIcon(val:TIcon);
begin
  SystemButton.Glyph.Assign(val);
end;

procedure TMDIPanel.Paint;
var
  IRect: TRect; ibw:integer;
begin
  inherited Paint;
  IRect := GetClientRect;
  ibw:=0;
  if self.BevelInner <> bvNone then ibw:=BevelWidth;
  InflateRect(IRect, -ibw, -ibw);
  Canvas.Frame3d(IRect, BorderColor, BorderColor, BorderWidth);
end;

function TMDIPanel.GetBorderColor: TColor;
begin
  if Active then
    if FActiveBorderColor <> clDefault
      then result := FActiveBorderColor
      else if ColorToRGB(clActiveBorder)<>ColorToRGB(clInactiveBorder)
         then result := clActiveBorder
         else result := clActiveCaption
  else
    if FInactiveBorderColor <> clDefault
      then result := FInactiveBorderColor
      else if ColorToRGB(clActiveBorder)<>ColorToRGB(clInactiveBorder)
         then result := clInactiveBorder
         else result := clInactiveCaption;
end;

procedure TMDIPanel.SetActiveBorderColor(AValue: TColor);
begin
  if FActiveBorderColor <> AValue then
  begin
    FActiveBorderColor := AValue;
    Invalidate;
  end;
end;

procedure TMDIPanel.SetInactiveBorderColor(AValue: TColor);
begin
  if FInactiveBorderColor <> AValue then
  begin
    FInactiveBorderColor := AValue;
    Invalidate;
  end;
end;

procedure TMDIPanel.AdjustClientRect(var ARect: TRect);
begin
  inherited AdjustClientRect(ARect);
  self.FCornerSize:=10;
  if ClientRect.Left > 10 then self.FCornerSize:=ClientRect.Left;
end;

procedure TMDIPanel.SetParent(NewParent: TWinControl);
var H:integer; bm:TBitmap; bv:TPanelBevel; bw:integer;  bs:TBorderStyle;
begin
  if Parent = NewParent then exit;
  inherited SetParent(NewParent);
  FParentForm := GetParentForm(Self);

  bv:=BevelOuter;
  bv:=BevelInner;
  bw:=BevelWidth;
  bs:=BorderStyle;
  bw:=BorderWidth;


  if assigned(SystemButton) then
    with SystemButton do
    begin
      Transparent:=true;
      if assigned(FParentForm) then
         Glyph.Assign(FParentForm.Icon);
      if not assigned(Glyph) or (Glyph.Height = 0) then
        begin
        h:=deriveCaptionIconHeight;
        if Application.Icon.Height = h then
          Glyph.Assign(Application.Icon)
        else
          begin
            bm := TBitmap.Create;
            bm.SetSize(h,h);
            bm.Canvas.AntialiasingMode:=amOn;
            bm.Canvas.StretchDraw(Rect(0,0,h,h),Application.Icon);
            Glyph.Assign(bm);
            bm.free;
          end;
        end;
      if not assigned(Glyph) or (Glyph.Height = 0) then
        Caption := ' $ ';
      if assigned(Glyph) and (Glyph.Height > 0) then
        Constraints.MinWidth := Height;
    end;

  Invalidate;

  CaptionPanel.AdjustSize;
  H := CaptionPanel.Height + 2*BorderWidth;
  if BevelInner<>bvNone then H := H + 2*BevelWidth;
  if BevelOuter<>bvNone then H := H + 2*BevelWidth;
  Constraints.MinHeight := H + 4; // +4 - to prevent lower border disappear
  Constraints.MinWidth  := 200;
end;

procedure TMDIPanel.CreateClientPanel;
var Image1:TImage;
begin
  ClientPanel := TMDIClientPanel.Create(Self);
  with ClientPanel do
  begin
    Parent := Self;
    Name := 'ClientPanel';
    //HorzScrollBar.Page := 1;
    //VertScrollBar.Page := 1;
    Align := alClient;
    TabStop := False;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    BorderStyle := bsNone; //bsSingle;
    BorderWidth := 0;
  end;
end;

resourcestring
  rsMaximize = 'Maximize';
  rsMinimize = 'Minimize';
  rsRestore = 'Restore';
  rsClose = 'Close';

procedure TMDIPanel.CreateSystemPopupMenu;
var
  MenuItem: TMenuItem;
begin
  SystemPopupMenu := TPopupMenu.Create(Self);

  MenuItemMaximize := TMenuItem.Create(Self);
  MenuItemMaximize.Name := rsMaximize;
  MenuItemMaximize.OnClick := self.MaximizeButton.OnClick;
  SystemPopupMenu.Items.Add(MenuItemMaximize);

  MenuItemMinimize := TMenuItem.Create(Self);
  MenuItemMinimize.Name := rsMinimize;
  MenuItemMinimize.OnClick := self.MinimizeButton.OnClick;
  SystemPopupMenu.Items.Add(MenuItemMinimize);

  MenuItemRestore := TMenuItem.Create(Self);
  MenuItemRestore.Name := rsRestore;
  MenuItemRestore.OnClick := self.RestoreButton.OnClick;
  SystemPopupMenu.Items.Add(MenuItemRestore);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := '-';
  MenuItem.Name := 'Separator';
  SystemPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Name := rsClose;
  MenuItem.OnClick := self.CloseButton.OnClick;
  SystemPopupMenu.Items.Add(MenuItem);

  self.CaptionPanel.PopupMenu := SystemPopupMenu;
end;

{
procedure TMDIPanel.ActionActionSystemMenuExecute(Sender: TObject);
begin

end;

procedure TMDIPanel.CreateSystemActions;
var Act:TAction;
begin
   FActionList:=TActionList.Create(Self);
   FActionList.name:='ActionList';

   Act:=TAction.Create(Self);
   Act.ActionList:=FActionList;
   Act.Name:='ActionSystemMenu';
   Act.ShortCut:=TextToShortCut('Alt+Space');
   Act.OnExecute:=@ActionActionSystemMenuExecute;

   Act:=TAction.Create(Self);
   Act.ActionList:=FActionList;
   Act.Name:='ActionSystemMenu';
   Act.ShortCut:=TextToShortCut('Alt+F4');
   Act.OnExecute:=@CloseButtonClick;

end;
}

procedure TMDIPanel.SetBorderCursor(X, Y: integer);
begin
  FWindowResizingSide := wrszsZ;
  Cursor := crDefault;
  if X <= FCornerSize then
  begin
    if Y <= FCornerSize then
    begin
      FWindowResizingSide := wrszsNW;
      Cursor := crSizeNW;
    end
    else if Y >= Height - FCornerSize then
    begin
      FWindowResizingSide := wrszsSW;
      Cursor := crSizeSW;
    end
    else
    begin
      FWindowResizingSide := wrszsW;
      Cursor := crSizeWE;
    end;
  end
  else if X >= Width - FCornerSize then
  begin
    if Y <= FCornerSize then
    begin
      FWindowResizingSide := wrszsNE;
      Cursor := crSizeNE;
    end
    else if Y >= Height - FCornerSize then
    begin
      FWindowResizingSide := wrszsSE;
      Cursor := crSizeSE;
    end
    else
    begin
      FWindowResizingSide := wrszsE;
      Cursor := crSizeWE;
    end;
  end
  else   // X in mid
  begin
    if Y <= FCornerSize then
    begin
      FWindowResizingSide := wrszsN;
      Cursor := crSizeNS;
    end
    else if Y >= Height - FCornerSize then
    begin
      FWindowResizingSide := wrszsS;
      Cursor := crSizeNS;
    end
    else
    begin
      FWindowResizingSide := wrszsZ;
      Cursor := crSizeNS;
    end;
  end;
end;

procedure TMDIPanel.BorderMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  TL: TPoint;
begin
  if (Button = mbLeft) and (Shift = [ssLeft]) then
  begin
    WindowPositionState := wpsResizing;
    TL := Parent.ClientToScreen(BoundsRect.TopLeft);
    //writeln('d:',Mouse.CursorPos.X, ':', Mouse.CursorPos.Y);
    WindowCaptionMouseX := Mouse.CursorPos.X - TL.X;
    WindowCaptionMouseY := Mouse.CursorPos.Y - TL.Y;
    setActive(True);
  end;
end;

procedure TMDIPanel.BorderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  TL, SL: TPoint;
  L, T, W, H: integer;
begin
  if not (WindowPositionState = wpsResizing) then
    SetBorderCursor(X, Y);

  if (Shift = [ssLeft]) and (WindowPositionState = wpsResizing) then
  begin
    //writeln('m:', Mouse.CursorPos.X, ':', Mouse.CursorPos.Y, ' sender:',
      //TControl(Sender).Name);
    L := Left;
    T := Top;
    W := Width;
    H := Height;
    case FWindowResizingSide of
      wrszsNW:
      begin
        SL := Point(Mouse.CursorPos.X - WindowCaptionMouseX,
          Mouse.CursorPos.Y - WindowCaptionMouseY);
        TL := Parent.ScreenToClient(SL);
        H := H - (TL.Y - T);
        W := W - (TL.X - L);
        L := TL.X;
        T := TL.Y;
      end;
      wrszsN:
      begin
        SL := Point(Mouse.CursorPos.X - WindowCaptionMouseX,
          Mouse.CursorPos.Y - WindowCaptionMouseY);
        TL := Parent.ScreenToClient(SL);
        H := H - (TL.Y - T);
        T := TL.Y;
      end;
      wrszsNE:
      begin
        W := X;
        SL := Point(Mouse.CursorPos.X - WindowCaptionMouseX,
          Mouse.CursorPos.Y - WindowCaptionMouseY);
        TL := Parent.ScreenToClient(SL);
        H := H - (TL.Y - T);
        T := TL.Y;
      end;
      wrszsE: W := X;
      wrszsSE:
      begin
        W := X;
        H := Y;
      end;
      wrszsS: H := Y;
      wrszsSW:
      begin
        H := Y;
        SL := Point(Mouse.CursorPos.X - WindowCaptionMouseX,
          Mouse.CursorPos.Y - WindowCaptionMouseY);
        TL := Parent.ScreenToClient(SL);
        W := W - (TL.X - L);
        L := TL.X;
      end;
      wrszsW:
      begin
        SL := Point(Mouse.CursorPos.X - WindowCaptionMouseX,
          Mouse.CursorPos.Y - WindowCaptionMouseY);
        TL := Parent.ScreenToClient(SL);
        W := W - (TL.X - L);
        L := TL.X;
      end;
    end;

    writeln('setBounds(', L, ',', T, ',', W, ',', H, ')');
    setBounds(L, T, W, H);
  end;
end;

procedure TMDIPanel.BorderMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (WindowPositionState = wpsResizing) then
    WindowPositionState := wpsNone;
end;



procedure TMDIPanel.CaptionPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  TL: TPoint;
begin
  if (Button = mbLeft) and (Shift = [ssLeft]) then
  begin
    WindowPositionState := wpsMoving;
    //TL:=ClientToScreen(TPanel(TPanel(Sender).Parent).BoundsRect.TopLeft);
    TL := Parent.ClientToScreen(BoundsRect.TopLeft);
    //writeln('d:',Mouse.CursorPos.X, ':', Mouse.CursorPos.Y);
    WindowCaptionMouseX := Mouse.CursorPos.X - TL.X;
    WindowCaptionMouseY := Mouse.CursorPos.Y - TL.Y;
    setActive(True);
  end;
end;

procedure TMDIPanel.CaptionPanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  TL, SL: TPoint;
begin
  if (Shift = [ssLeft]) and (WindowPositionState = wpsMoving) then
  begin
    //writeln('m:',Mouse.CursorPos.X, ':', Mouse.CursorPos.Y);
    SL := Point(Mouse.CursorPos.X - WindowCaptionMouseX, Mouse.CursorPos.Y -
      WindowCaptionMouseY);
    TL := Parent.ScreenToClient(SL);
    Left := TL.X;
    Top := TL.Y;
    //writeln('w:',TL.X, ':', TL.Y);
  end;
end;

procedure TMDIPanel.CaptionPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (WindowPositionState = wpsMoving) then
    WindowPositionState := wpsNone;
end;

procedure TMDIPanel.SetCaption(const Value: TCaption);
begin
  CaptionPanel.Caption := Value;
end;

function TMDIPanel.GetCaption: TCaption;
begin
  Result := CaptionPanel.Caption;
end;

procedure TMDIPanel.DoClose(CloseAction: TCloseAction);
begin
  if not assigned(self) then exit;
  if assigned(FOnClose) then
     FOnClose(self,CloseAction);

  self.unsetMouseClickProxies;

  SystemPopupMenu.Close;
  Application.ProcessMessages;

  if CloseAction = caHide then
    Visible := False;
  if CloseAction = caMinimize then
    doMinimize;
  if CloseAction = caFree then
    Free;
end;

procedure TMDIPanel.SystemButtonClick(Sender: TObject);
begin
  SystemPopupMenu.PopUp;
end;

procedure TMDIPanel.CloseButtonClick(Sender: TObject);
begin
  DoClose(caFree);
end;

procedure TMDIPanel.OrderButtons;
begin
  if assigned(RestoreButton) then
    RestoreButton.Visible := (FWindowState <> wsNormal);
  if assigned(MaximizeButton) then
    MaximizeButton.Visible := (FWindowState <> wsMaximized);
  if assigned(MinimizeButton) then
    MinimizeButton.Visible := (FWindowState <> wsMinimized);

  MenuItemMinimize.Enabled := (FWindowState <> wsMinimized);
  MenuItemMaximize.Enabled := (FWindowState <> wsMaximized);
  MenuItemRestore.Enabled  := (FWindowState <> wsNormal);

  if assigned(CloseButton) then
    CloseButton.Left := 120;
  if assigned(MaximizeButton) then
    MaximizeButton.Left := 100;
  if assigned(RestoreButton) then
    RestoreButton.Left := 80;
  if assigned(MinimizeButton) then
    MinimizeButton.Left := 60;
end;

procedure TMDIPanel.DoMaximize;
begin
  if FWindowState = wsNormal then
    FNormalBounds := Self.BoundsRect;
  Self.setBounds(0, 0, Parent.ClientWidth, Parent.ClientHeight);
  FWindowState := wsMaximized;
  OrderButtons;
end;

procedure TMDIPanel.DoMinimize;
begin
  if FWindowState = wsNormal then
    FNormalBounds := Self.BoundsRect;
  Self.setBounds(0, 0, 200, CaptionPanel.Height + BevelWidth * 2);
  FWindowState := wsMinimized;
  OrderButtons;
end;

procedure TMDIPanel.DoRestore;
begin
  Self.BoundsRect := FNormalBounds;
  FWindowState := wsNormal;
  OrderButtons;
end;

procedure TMDIPanel.MaximizeButtonClick(Sender: TObject);
begin
  DoMaximize;
end;

procedure TMDIPanel.MinimizeButtonClick(Sender: TObject);
begin
  DoMinimize;
end;

procedure TMDIPanel.RestoreButtonClick(Sender: TObject);
begin
  DoRestore;
end;


function TMDIPanel.GetControl(const Index: integer): TControl;
begin
  Result := ClientPanel.Controls[Index];
end;

function TMDIPanel.GetControlCount: integer;
begin
  Result := ClientPanel.ControlCount;
end;

procedure TMDIPanel.InsertControl(AControl: TControl);
begin
  if (AControl = CaptionPanel) or (AControl = ClientPanel)
  //or (AControl = FPassiveBevel)
  then
    inherited InsertControl(AControl)
  else
    ClientPanel.InsertControl(AControl);
end;

procedure TMDIPanel.InsertControl(AControl: TControl; Index: integer);
begin
  if (AControl = CaptionPanel) or (AControl = ClientPanel)
  //or (AControl = FPassiveBevel)
  then
    inherited InsertControl(AControl, Index)
  else
    begin
    ClientPanel.InsertControl(AControl, Index - 2); // -2 because ClientPanel and CaptionPanel
    if not FActive then
      setMouseClickProxiesRecursively(AControl);
    end;
end;

procedure TMDIPanel.RemoveControl(AControl: TControl);
begin
  if (AControl = CaptionPanel) or (AControl = ClientPanel) or
    (AControl = FPassiveBevel) then
    inherited RemoveControl(AControl)
  else
    begin
    ClientPanel.RemoveControl(AControl);
    if not Active then
      unsetMouseClickProxiesRecursively(AControl);
    end;
end;

procedure TMDIPanel.unsetMouseClickProxiesRecursively(Ctrl: TControl);
var
  i: integer;
  C: TControl;

  procedure unsetProxyFor(C: TControl);
  var i: integer;
    L: TMouseClickProxy;
  begin
    if TMouseClickProxy.IsOnClick(C.OnClick) then
    begin
      i:=0;
      repeat
        L:=TMouseClickProxy(FClickProxies.Items[i]);
        inc(i);
      until L.FOwner = C;
      C.OnClick := L.FOwnerOnClick;
      FClickProxies.Remove(L);
      L.Free;
    end;
  end;

begin
  if Ctrl is TWinControl then
    for i := 0 to TWinControl(Ctrl).ControlCount - 1 do
    begin
      C := TWinControl(Ctrl).Controls[i];
      unsetMouseClickProxiesRecursively(C);
    end;
  unsetProxyFor(Ctrl);
end;

procedure TMDIPanel.setMouseClickProxiesRecursively(Ctrl: TControl);
var
  i: integer;
  C: TControl;

  procedure setProxyFor(C: TControl);
  var
    L: TMouseClickProxy;
  begin
    if not TMouseClickProxy.IsOnClick(C.OnClick) then
    begin
      L := TMouseClickProxy.Create(C);
      L.ProxyOnClick := @PassivePanelOnClick;
      FClickProxies.Add(L);
    end;
  end;

begin
  setProxyFor(Ctrl);
  if Ctrl is TWinControl then
    for i := 0 to TWinControl(Ctrl).ControlCount - 1 do
    begin
      C := TWinControl(Ctrl).Controls[i];
      setMouseClickProxiesRecursively(C);
    end;
end;

procedure TMDIPanel.setMouseClickProxies;
var
  i: integer;
  C: TControl;
  L: TMouseClickProxy;
  M, MP: TMethod;
begin
  setMouseClickProxiesRecursively(CaptionPanel);
  unsetMouseClickProxiesRecursively(self.CloseButton);
  setMouseClickProxiesRecursively(ClientPanel);
end;

procedure TMDIPanel.unsetMouseClickProxies;
var
  i: integer;
  C: TControl;
  L: TMouseClickProxy;
begin
  for i := 0 to FClickProxies.Count - 1 do
  begin
    L := TMouseClickProxy(FClickProxies[i]);
    L.FOwner.OnClick := L.FOwnerOnClick;
    L.Free;
  end;
  FClickProxies.Clear;
end;

procedure TMDIPanel.InactivateSiblings;
var
  i: integer;
  C: TControl;
begin
  if not assigned(Parent) then
    exit;
  for i := 0 to Parent.ControlCount - 1 do
  begin
    C := Parent.Controls[i];
    if (C is TMDIPanel) and (C <> Self) then
      TMDIPanel(C).setActive(False);
  end;
end;


procedure TMDIPanel.setActive(val: boolean);
var H:integer; bm:TBitmap; bv:TPanelBevel; bw:integer;  bs:TBorderStyle;
begin

  bv:=BevelOuter;
  bv:=BevelInner;
  bw:=BevelWidth;
  bs:=BorderStyle;
  bw:=BorderWidth;

  if FActive = val then exit;
  FActive := val;
  if val then
  begin
    //BevelColor := clActiveBorder;
    //BevelColor := clRed;
    //Color := clActiveBorder;
    //Color := clActiveCaption;
    //BorderColor:=clActiveBorder;
    //BorderColor:=clRed;
    CaptionPanel.Color := clActiveCaption;
    setZOrder(True);
    InactivateSiblings;
    unsetMouseClickProxies;
    if assigned(FOnActivate) then FOnActivate(Self);
  end
  else
  begin
    //BevelColor := clInactiveBorder;
    //Color := clInactiveBorder;
    //Color := clInactiveCaption;
    //BorderColor:=clInactiveBorder;
    CaptionPanel.Color := clInactiveCaption;
    setMouseClickProxies;
    if assigned(FOnDeactivate) then FOnDeactivate(Self);
  end;
  invalidate;
end;

procedure TMDIPanel.PassivePanelOnClick(Sender: TObject);
begin
  setActive(True);
end;


{----------------------  TMouseClickProxy -------------------------
  This object substitutes OnClick method of a TheOwner control
  with its OnClick method.
  When the OnClick called it will call first a ProxyOnClick method
  then the original Owner OnClick method.
  Here in MDIPanel it is used for intersept OnClick to children controls of
  inactive MDIPanel, so activating it before calling OnClick of the clicked
  children control.
 }
constructor TMouseClickProxy.Create(TheOwner: TControl);
var
  N: string;
begin
  FDestroyed := False;
  FOwner := TheOwner;
  N := FOwner.Name;
  FOwnerOnClick := FOwner.onClick;
  FOwner.onClick := @OnClick;
end;

destructor TMouseClickProxy.Destroy;
begin
  FDestroyed := True;
  FOwner.onClick := FOwnerOnClick;
  FOwnerOnClick := nil;
  FProxyOnClick := nil;
  FOwner := nil;
  inherited Destroy;
end;

procedure TMouseClickProxy.OnClick(Sender: TObject);
var
  vOwnerOnClick: TNotifyEvent;
begin
  if FDestroyed then
    exit;
  vOwnerOnClick := FOwnerOnClick;
  if assigned(FProxyOnClick) then
    FProxyOnClick(Sender);
  if assigned(vOwnerOnClick) then
    vOwnerOnClick(Sender);
end;

class function TMouseClickProxy.IsOnClick(P: TNotifyEvent): boolean;
var
  PM: TMethod;
begin
  Result := TMethod(P).Code = TMethod(@OnClick).Code;
end;

procedure TMDIClientPanel.ActiveDefaultControlChanged(NewControl: TControl);
begin
  if assigned(NewControl) and assigned(Parent)
     and (Parent is TMDIPanel)
  then
    TMDIPanel(Parent).setActive(true);
end;

procedure Register;
begin
  RegisterComponents('MDIPanel', [TMDIPanel]);
end;

end.
