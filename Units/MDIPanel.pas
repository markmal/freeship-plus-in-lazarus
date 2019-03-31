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
  TMDIPanel = class(TPanel)
    CaptionPanel: TPanel;
    ClientPanel: TPanel; //TScrollBox;
    SystemButton: TSpeedButton;
    CloseButton: TSpeedButton;
    MaximizeButton: TSpeedButton;
    MinimizeButton: TSpeedButton;
    RestoreButton: TSpeedButton;
    SystemPopupMenu: TPopupMenu;
    MenuItemMinimize: TMenuItem;
    MenuItemMaximize: TMenuItem;
  private
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
    FWindowResizingSide: TWindowResizingSide;
    FActive: boolean;
    FPassiveBevel: TGraphicControl;
    FClickProxies: TFPList;
    FCaptionButtons: TCaptionButtons;
    //FActionList: TActionList;
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


    procedure SetCaption(const Value: TCaption);
    function GetCaption: TCaption;
    function GetControl(const Index: integer): TControl;
    function GetControlCount: integer;

    procedure setMouseClickProxiesRecursively(WinCtrl: TWinControl);
    procedure setMouseClickProxies;
    procedure unsetMouseClickProxies;
    procedure PassivePanelOnClick(Sender: TObject);

  protected
    procedure Deactivate; virtual;
    procedure SetParent(NewParent: TWinControl); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Caption: TCaption read GetCaption write SetCaption;
    property Controls[Index: integer]: TControl read GetControl;
    property ControlCount: integer read GetControlCount;

    procedure InsertControl(AControl: TControl);
    procedure InsertControl(AControl: TControl; Index: integer); override;
    procedure RemoveControl(AControl: TControl); override;

    procedure setActive(val: boolean);
    procedure InactivateSiblings;

    function getIcon :TIcon;
    procedure setIcon(val:TIcon);
  published
    property Icon: TIcon read getIcon write setIcon;
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
  lclproc;

//{$R *.lfm}

constructor TMDIPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FClickProxies := TFPList.Create;
  Name := getUniqueName('MDIPanel');
  Color := clForm;
  BevelColor := clActiveBorder;
  BevelInner := bvLowered;
  BevelWidth := 4;
  FCornerSize := 10;
  FWindowState := wsNormal; //wsMinimized, wsMaximized
  WindowPositionState := wpsNone;
  FCaptionButtons := [cbSystemMenu, cbMinimize, cbMaximize, cbRestore, cbClose];
  CreateClientPanel;
  CreateCaptionPanel;
  OrderButtons;
  CreateSystemPopupMenu;
end;

destructor TMDIPanel.Destroy;
begin
  unsetMouseClickProxies;
  FClickProxies.Free;
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

function TMDIPanel.drawCloseIcon(size: integer): TBitmap;
var
  scale, i, r, k: integer;
  AImage: TLazIntfImage;
  ACanvas: TLazCanvas;
  lRawImage: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  lRawImage.Init;
  lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(size, size);
  lRawImage.CreateData(True);
  AImage := TLazIntfImage.Create(0, 0);
  AImage.SetRawImage(lRawImage);
  ACanvas := TLazCanvas.Create(AImage);

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

  AImage.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  Result := TBitmap.Create;
  Result.Handle := ImgHandle;
  Result.MaskHandle := ImgMaskHandle;

  ACanvas.Free;
  AImage.Free;
end;

function TMDIPanel.drawMaximizeIcon(size: integer): TBitmap;
var
  scale, i, r: integer;
  AImage: TLazIntfImage;
  ACanvas: TLazCanvas;
  lRawImage: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  lRawImage.Init;
  lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(size, size);
  lRawImage.CreateData(True);
  AImage := TLazIntfImage.Create(0, 0);
  AImage.SetRawImage(lRawImage);
  ACanvas := TLazCanvas.Create(AImage);

  scale := size div 8;
  i := scale;
  r := size - i;
  ACanvas.Pen.FPColor := FPColor(0, 0, 0, $FFFF);
  ACanvas.Pen.Width := scale;
  ACanvas.Rectangle(i, i, r, r);

  AImage.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  Result := TBitmap.Create;
  Result.Handle := ImgHandle;
  Result.MaskHandle := ImgMaskHandle;

  ACanvas.Free;
  AImage.Free;
end;

function TMDIPanel.drawMinimizeIcon(size: integer): TBitmap;
var
  scale, i, r: integer;
  AImage: TLazIntfImage;
  ACanvas: TLazCanvas;
  lRawImage: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  lRawImage.Init;
  lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(size, size);
  lRawImage.CreateData(True);
  AImage := TLazIntfImage.Create(0, 0);
  AImage.SetRawImage(lRawImage);
  ACanvas := TLazCanvas.Create(AImage);

  scale := size div 8;
  ACanvas.Pen.FPColor := FPColor(0, 0, 0, $FFFF);
  i := scale * 2;
  r := size - i;
  ACanvas.Pen.Width := scale;
  ACanvas.Line(i, size div 2, r, size div 2);

  AImage.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  Result := TBitmap.Create;
  Result.Handle := ImgHandle;
  Result.MaskHandle := ImgMaskHandle;

  ACanvas.Free;
  AImage.Free;
end;

function TMDIPanel.drawRestoreIcon(size: integer): TBitmap;
var
  scale, i, r: integer;
  AImage: TLazIntfImage;
  ACanvas: TLazCanvas;
  lRawImage: TRawImage;
  ImgHandle, ImgMaskHandle: HBitmap;
begin
  lRawImage.Init;
  lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(size, size);
  lRawImage.CreateData(True);
  AImage := TLazIntfImage.Create(0, 0);
  AImage.SetRawImage(lRawImage);
  ACanvas := TLazCanvas.Create(AImage);

  scale := size div 8;
  ACanvas.Pen.FPColor := FPColor(0, 0, 0, $FFFF);
  i := scale * 2;
  r := size - i;
  ACanvas.Pen.Width := scale;
  ACanvas.Rectangle(i, i, r, r);

  AImage.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  Result := TBitmap.Create;
  Result.Handle := ImgHandle;
  Result.MaskHandle := ImgMaskHandle;

  ACanvas.Free;
  AImage.Free;
end;


procedure TMDIPanel.CreateCaptionPanel;
var
  pic: TPicture;
  h, fppi, IconSize: integer;
  IconSizeString: string;
  fd: TFontData;
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
    Caption := 'MDIPanel';
    Color := clActiveCaption;
    Font.Color := clCaptionText;
    Font.Style := [fsBold];
    ParentColor := False;
    ParentFont := False;
    TabStop := False;
    OnMouseDown := @CaptionPanelMouseDown;
    OnMouseMove := @CaptionPanelMouseMove;
    OnMouseUp := @CaptionPanelMouseUp;
  end;

  //detect default font height to derive caption height
  fd := GetFontData(CaptionPanel.Font.Handle);
  // TFontData.Height is in logical units, convert it to pixels
  h := abs(round(1.0 * fd.Height * Font.PixelsPerInch / 72.0));
  CaptionPanel.Constraints.MinHeight := h + 4; // add 4 so font does not touch borders

  if (h < 24) then
    IconSize := 16
  else if (h < 34) then
    IconSize := 22
  else
    IconSize := 32;
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

procedure TMDIPanel.SetParent(NewParent: TWinControl);
var
  icn: TIcon;
begin
  inherited SetParent(NewParent);
  if self.Parent is TForm then
  begin
    icn := TForm(self.Parent).Icon;
    if assigned(SystemButton) then
      with SystemButton do
      begin
        Glyph.Assign(TForm(self.Parent).Icon);
        if not assigned(Glyph) or (Glyph.Height = 0) then
          begin
          Glyph.Height := CloseButton.Glyph.Height;
          Glyph.Width := CloseButton.Glyph.Width;
          Glyph.Canvas.StretchDraw(
                Rect(0,0,Glyph.Width,Glyph.Height),
                Application.Icon);
          //Assign(Application.Icon);
          end;
        if not assigned(Glyph) or (Glyph.Height = 0) then
          Caption := ' $ ';
        if assigned(Glyph) and (Glyph.Height > 0) then
        begin
          Constraints.MinWidth := Height;
        end;

      end;
  end;
end;

procedure TMDIPanel.CreateClientPanel;
begin
  ClientPanel := TPanel.Create(Self); //TScrollBox.Create(Self);
  with ClientPanel do
  begin
    Parent := Self;
    Name := 'ClientPanel';
    //HorzScrollBar.Page := 1;
    //VertScrollBar.Page := 1;
    Align := alClient;
    TabStop := False;
    BevelOuter := bvNone;
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

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Name := rsMaximize;
  MenuItem.OnClick := self.MaximizeButton.OnClick;
  SystemPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Name := rsMinimize;
  MenuItem.OnClick := self.MinimizeButton.OnClick;
  SystemPopupMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Name := rsRestore;
  MenuItem.OnClick := self.RestoreButton.OnClick;
  SystemPopupMenu.Items.Add(MenuItem);

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
    writeln('m:', Mouse.CursorPos.X, ':', Mouse.CursorPos.Y, ' sender:',
      TControl(Sender).Name);
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
  SystemPopupMenu.Close;
  Application.ProcessMessages;

  if CloseAction = caFree then
    Free;
  if CloseAction = caHide then
    Visible := False;
  if CloseAction = caMinimize then
    doMinimize;
end;

procedure TMDIPanel.SystemButtonClick(Sender: TObject);
begin
  SystemPopupMenu.PopUp;
end;

procedure TMDIPanel.CloseButtonClick(Sender: TObject);
var
  CloseAction: TCloseAction;
begin
  CloseAction := caFree;
  if Assigned(FOnClose) then
    FOnClose(Self, CloseAction);
  DoClose(CloseAction);
end;

procedure TMDIPanel.OrderButtons;
begin
  if assigned(RestoreButton) then
    RestoreButton.Visible := (FWindowState <> wsNormal);
  if assigned(MaximizeButton) then
    MaximizeButton.Visible := (FWindowState <> wsMaximized);
  if assigned(MinimizeButton) then
    MinimizeButton.Visible := (FWindowState <> wsMinimized);

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
    ClientPanel.InsertControl(AControl, Index - 2); // -2 because ClientPanel and CaptionPanel
end;

procedure TMDIPanel.RemoveControl(AControl: TControl);
begin
  if (AControl = CaptionPanel) or (AControl = ClientPanel) or
    (AControl = FPassiveBevel) then
    inherited RemoveControl(AControl)
  else
    ClientPanel.RemoveControl(AControl);
end;


procedure TMDIPanel.setMouseClickProxiesRecursively(WinCtrl: TWinControl);
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
  setProxyFor(WinCtrl);
  for i := 0 to WinCtrl.ControlCount - 1 do
  begin
    C := WinCtrl.Controls[i];
    setProxyFor(C);
    if C is TWinControl then
      setMouseClickProxiesRecursively(TWinControl(C));
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
begin
  FActive := val;
  if val then
  begin
    BevelColor := clActiveBorder;
    CaptionPanel.Color := clActiveCaption;
    setZOrder(True);
    InactivateSiblings;
    unsetMouseClickProxies;
  end
  else
  begin
    BevelColor := clInactiveBorder;
    CaptionPanel.Color := clInactiveCaption;
    setMouseClickProxies;
  end;
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

end.
