{%MainUnit MDIPanel.pas}

function TMDIPanel.LCLVersionIsStored: boolean;
begin
  Result:=Parent=nil;
end;

constructor TCustomMDIPanel.CreateNew(AOwner: TComponent);
begin
  Include(FFormState,fsCreating);
  inherited Create(AOwner);
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

  FClientControls:=TFPList.Create;

  FCornerSize := 10;
  FWindowState := wsNormal;
  WindowPositionState := wpsNone;
  FCaptionButtons := [];

  CreateCaptionPanel([cbSystemMenu, cbMinimize, cbMaximize, cbRestore, cbClose]);
  CreateClientPanel;
  CreateSystemPopupMenu;
  OrderButtons;

  SetActive(false);
  //SetMouseClickProxies;
  Screen.AddHandlerActiveControlChanged(@ScreenOnActiveControlChanged,False);

  Exclude(FFormState,fsCreating);
  Invalidate;
end;

constructor TCustomMDIPanel.Create(AOwner: TComponent);
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner); // this calls BeginFormUpdate, which is ended in AfterConstruction
    if (ClassType <> TMDIPanel) and not (csDesigning in ComponentState) then
    begin
      Include(FFormState, fsCreating);
      try
        //load controls from the program resources to FClientControls
        ProcessResource;
      finally
        Exclude(FFormState, fsCreating);
      end;
      // Now place loaded controls to FClientPanel
      PopulateClientPanel;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

procedure TCustomMDIPanel.PopulateClientPanel;
var i:integer; control:TControl;
begin
  for i:=0 to FClientControls.Count-1 do
  begin
    control:=TControl(FClientControls[i]);
    FClientPanel.InsertControl(control);
  end;

  FClientPanel.OnMouseDown := @ClientPanelOnMouseDown;

  FClientPanel.OnChangeBounds := OnChangeBounds;
  FClientPanel.OnClick := OnClick;
  //FClientPanel.OnCloseQuery := OnCloseQuery;
  FClientPanel.OnConstrainedResize := OnConstrainedResize;
  FClientPanel.OnContextPopup := OnContextPopup;
  FClientPanel.OnDblClick := OnDblClick;
  FClientPanel.OnDockDrop := OnDockDrop;
  FClientPanel.OnDockOver := OnDockOver;
  FClientPanel.OnDragDrop := OnDragDrop;
  FClientPanel.OnDragOver := OnDragOver;
  //FClientPanel.OnDropFiles := OnDropFiles;
  FClientPanel.OnEndDock := OnEndDock;
  FClientPanel.OnGetSiteInfo := OnGetSiteInfo;
  //FClientPanel.OnHelp := OnHelp;
  //FClientPanel.OnHide := OnHide;
  FClientPanel.OnKeyDown := OnKeyDown;
  FClientPanel.OnKeyPress := OnKeyPress;
  FClientPanel.OnKeyUp := OnKeyUp;
  FClientPanel.OnMouseEnter := OnMouseEnter;
  FClientPanel.OnMouseLeave := OnMouseLeave;
  FClientPanel.OnMouseMove := OnMouseMove;
  FClientPanel.OnMouseUp := OnMouseUp;
  FClientPanel.OnMouseWheel := OnMouseWheel;
  FClientPanel.OnMouseWheelDown := OnMouseWheelDown;
  FClientPanel.OnMouseWheelUp := OnMouseWheelUp;
  FClientPanel.OnMouseWheelHorz := OnMouseWheelHorz;
  FClientPanel.OnMouseWheelLeft := OnMouseWheelLeft;
  FClientPanel.OnMouseWheelRight := OnMouseWheelRight;
  FClientPanel.OnPaint := OnPaint;
  FClientPanel.OnResize := OnResize;
  //FClientPanel.OnShortCut := OnShortCut;
  FClientPanel.OnShowHint := OnShowHint;
  FClientPanel.OnStartDock := OnStartDock;
  FClientPanel.OnUnDock := OnUnDock;
  FClientPanel.OnUTF8KeyPress := OnUTF8KeyPress;
  //FClientPanel.OnWindowStateChange := OnWindowStateChange;

end;

procedure TCustomMDIPanel.ProcessResource;
begin
  if not InitResourceComponent(Self, TMDIPanel) then
    if RequireDerivedFormResource then
      raise EResNotFound.CreateFmt(
        rsFormResourceSNotFoundForResourcelessFormsCreateNew, [ClassName])
    else
      DebugLn(Format(rsFormResourceSNotFoundForResourcelessFormsCreateNew, [ClassName]));
end;

procedure TCustomMDIPanel.SetMDIPanelManager(AValue: TMDIPanelManager);
begin
  if FMDIPanelManager=AValue then Exit;
  if FMDIPanelManager<>nil then // another manager
     FMDIPanelManager.Remove(Self);
  FMDIPanelManager:=AValue;
end;

destructor TCustomMDIPanel.Destroy;
begin
  //unsetMouseClickProxies;
  Screen.RemoveHandlerActiveControlChanged(@ScreenOnActiveControlChanged);
  FreeAndNil(FClickProxies);
  DeleteSystemPopupMenu;
  FreeAndNil(FClientControls);
  if assigned(onDestroy) then onDestroy(Self);
  inherited Destroy;
end;

procedure TCustomMDIPanel.Close;
begin
  DoClose(caFree);
end;

procedure TCustomMDIPanel.Release;
begin
  if Application <> nil then
    Application.ReleaseComponent(Self)
  else
    Free;
end;

procedure TCustomMDIPanel.LMSysCommand(var message: TLMessage);
begin
  if (message.WParam and $FFF0) = SC_CLOSE then
  begin
    DoClose(caFree);
  end;
  message.Result:=0;
end;

procedure TCustomMDIPanel.FActionCloseOnExecute(sender:TObject);
begin
  //PostMessage(self.Handle, LM_SYSCOMMAND, WPARAM(SC_CLOSE), LPARAM(nil));
  DoClose(caFree);
end;

function TCustomMDIPanel.getUniqueName(nameBase: string): string;
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

function TCustomMDIPanel.drawCloseIcon(size: integer): TBitmap;
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

function TCustomMDIPanel.drawMaximizeIcon(size: integer): TBitmap;
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

function TCustomMDIPanel.drawMinimizeIcon(size: integer): TBitmap;
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

function TCustomMDIPanel.drawRestoreIcon(size: integer): TBitmap;
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

function TCustomMDIPanel.deriveCaptionHeight:integer;
var h:integer; fd:TFontData;
begin
  H := GetSystemMetrics(SM_CYCAPTION); //header height
  if H>0 then
  begin
    result := H;
    exit;
  end;
  //detect default font height to derive caption height
  fd := GetFontData(FCaptionPanel.Font.Handle);
  // TFontData.Height is in logical units, convert it to pixels
  h := abs(round(1.0 * fd.Height * Font.PixelsPerInch / 72.0));
  result := h + 4; // add 4 so font does not touch borders
end;

function TCustomMDIPanel.deriveCaptionIconHeight:integer;
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

procedure TCustomMDIPanel.CreateCaptionPanel(aCaptionButtons:TCaptionButtons);
var SCH: integer;
begin
  OnMouseDown := @BorderMouseDown;
  OnMouseMove := @BorderMouseMove;
  OnMouseUp := @BorderMouseUp;
  SCH := GetSystemMetrics(SM_CYCAPTION); //header height
  //SFW := GetSystemMetrics(SM_CYDLGFRAME); //frame width

  FCaptionPanel := TPanel.Create(Self);
  FCaptionPanel.Parent:=Self;
  with FCaptionPanel do
  begin
    Name := 'CaptionPanel';
    //Height := 26;
    //AutoSize := True;
    Align := alTop;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    BorderStyle := bsNone; //bsSingle;
    BorderWidth := 0;
    Caption := '';
    Color := clInactiveCaption;
    Font.Color := clCaptionText;
    Font.Style := [fsBold];
    Height := SCH; // Font.Height + 8;
    ParentColor := False;
    ParentFont := False;
    TabStop := False;
    OnMouseMove := @CaptionPanelMouseMove;
    OnMouseUp := @CaptionPanelMouseUp;
    OnMouseDown := @CaptionPanelMouseDown;
  end;

  FCaptionLabel := TLabel.Create(Self);
  FCaptionLabel.Parent := FCaptionPanel;
  FCaptionLabel.ParentFont := true;
  FCaptionLabel.Align := alClient;
  FCaptionLabel.Alignment := taCenter;
  FCaptionLabel.Layout := tlCenter;
  FCaptionLabel.Caption := 'MDIPanel';
  FCaptionLabel.OnMouseMove := @CaptionPanelMouseMove;
  FCaptionLabel.OnMouseUp := @CaptionPanelMouseUp;
  FCaptionLabel.OnMouseDown := @CaptionPanelMouseDown;

  FCaptionPanel.Constraints.MinHeight := deriveCaptionHeight;
  SetCaptionButtons(aCaptionButtons);
end;

procedure TCustomMDIPanel.SetCaptionButtons(aVal:TCaptionButtons);
var
   IconSize: integer;
   //IconSizeString:string;
begin
  if (aVal = FCaptionButtons)then exit;
  FCaptionButtons := aVal;

  IconSize:=deriveCaptionIconHeight;
  //IconSizeString := IntToStr(IconSize) + 'x' + IntToStr(IconSize);
  //IconSizeString := 'X';

  if not (cbSystemMenu in FCaptionButtons) and assigned(FSystemButton)
     then FreeAndNil(FSystemButton);
  if not (cbClose in FCaptionButtons) and assigned(FCloseButton)
     then FreeAndNil(FCloseButton);
  if not (cbMinimize in FCaptionButtons) and assigned(FMinimizeButton)
     then FreeAndNil(FMinimizeButton);
  if not (cbMaximize in FCaptionButtons) and assigned(FMaximizeButton)
     then FreeAndNil(FMaximizeButton);
  if not (cbRestore in FCaptionButtons) and assigned(FRestoreButton)
     then FreeAndNil(FRestoreButton);


  //h:=deriveCaptionIconHeight;
  if (cbSystemMenu in FCaptionButtons) and (FSystemButton = nil) then
  begin
    FSystemButton := TImage.Create(Self);
    with FSystemButton do
    begin
      onClick := @SystemButtonClick;
      setDefaultSystemIcon;
    end;
  end;

  if (cbClose in FCaptionButtons) and (FCloseButton = nil)  then
  begin
    FCloseButton := TSpeedButton.Create(Self);
    with FCloseButton do
    begin
      Parent := FCaptionPanel;
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

  if (cbMaximize in FCaptionButtons) and (FMaximizeButton = nil)  then
  begin
    FMaximizeButton := TSpeedButton.Create(Self);
    with FMaximizeButton do
    begin
      Parent := FCaptionPanel;
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

  if (cbMinimize in FCaptionButtons) and (FMinimizeButton = nil)  then
  begin
    FMinimizeButton := TSpeedButton.Create(Self);
    with FMinimizeButton do
    begin
      Parent := FCaptionPanel;
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

  if (cbRestore in FCaptionButtons) and (FRestoreButton = nil)  then
  begin
    FRestoreButton := TSpeedButton.Create(Self);
    with FRestoreButton do
    begin
      Parent := FCaptionPanel;
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


procedure TCustomMDIPanel.Deactivate;
begin
  setActive(False);
end;

function TCustomMDIPanel.GetIcon: TIcon;
begin
  result := nil;
  if not assigned(FSystemButton) then exit;
  if not assigned(FSystemButton.Picture) then exit;
  if not assigned(FSystemButton.Picture.Icon) then exit;
  result := TIcon.Create;
  result.Assign(FSystemButton.Picture.Icon);
end;

procedure TCustomMDIPanel.SetIcon(val: TIcon);
begin
  FSystemButton.Picture.Icon.Assign(val);
end;

procedure TCustomMDIPanel.Paint;
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

{
procedure TCustomMDIPanel.Resize;
var cw,ch:integer;
begin
  inherited Resize;
  cw:=ClientWidth; ch:=ClientHeight;
  //AdjustClientRect(Rect();
  {
  if ([csLoading,csDestroying]*ComponentState<>[]) then exit;
  if AutoSizeDelayed then exit;
  cw:=ClientWidth; ch:=ClientHeight;
  if (FLastResizeWidth<>Width) or (FLastResizeHeight<>Height)
  or (FLastResizeClientWidth<>ClientWidth)
  or (FLastResizeClientHeight<>ClientHeight) then
  begin
    inherited Resize;
    FLastResizeWidth:=Width;
    FLastResizeHeight:=Height;
    FLastResizeClientWidth:=ClientWidth;
    FLastResizeClientHeight:=ClientHeight;
  end;
  }
end;
}

function TCustomMDIPanel.GetBorderColor: TColor;
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

procedure TCustomMDIPanel.SetActiveBorderColor(AValue: TColor);
begin
  if FActiveBorderColor <> AValue then
  begin
    FActiveBorderColor := AValue;
    Invalidate;
  end;
end;

procedure TCustomMDIPanel.SetInactiveBorderColor(AValue: TColor);
begin
  if FInactiveBorderColor <> AValue then
  begin
    FInactiveBorderColor := AValue;
    Invalidate;
  end;
end;

procedure TCustomMDIPanel.AdjustClientRect(var ARect: TRect);
begin
  inherited AdjustClientRect(ARect);
  self.FCornerSize:=10;
  if ClientRect.Left > 10 then self.FCornerSize:=ClientRect.Left;
end;

procedure TCustomMDIPanel.SetName(const Value: TComponentName);
var Newname:string;  gCnt:integer;
begin
  if Owner.FindComponent(Value) = nil then
    inherited SetName(Value)
  else
  begin
    gCnt := Owner.ComponentCount;
    Newname := Value+'__'+IntToStr(gCnt+1);
    inherited SetName(NewName);
  end;
end;

// does not help
procedure TCustomMDIPanel.WndProc(var TheMessage: TLMessage);
var
  Form : TCustomForm;
begin
    //DebugLn('CCC TControl.WndPRoc ',Name,':',ClassName);
{    if (csDesigning in ComponentState) then
    begin
      // redirect messages to designer
      Form := GetDesignerForm(Self);
      //debugln(['TControl.WndProc ',dbgsname(Self)]);
      if Assigned(Form) and Assigned(Form.Designer) and Form.Designer.IsDesignMsg(Self, TheMessage) then
        Exit;
    end
    else }
  if (TheMessage.Msg >= LM_KEYFIRST) and (TheMessage.Msg <= LM_KEYLAST)
    then begin
      // keyboard messages
      Form := GetParentForm(Self);
      if (Form <> nil) and (Form.WantChildKey(Self,TheMessage)) then exit;
    end;

    Dispatch(TheMessage);
end;

function TCustomMDIPanel.IsIconStored: Boolean;
begin
  result := Icon <> nil;
end;

procedure TCustomMDIPanel.setDefaultSystemIcon;
  var icn:TIcon; sz,ch:integer; pf:TCustomForm;
 {
  function getParentForm(c:TControl):TCustomForm;
  begin
    result:=nil;
    if assigned(c.Parent) then
       if (c.Parent is TCustomForm)
       then result:=TCustomForm(c.Parent)
       else getParentForm(c.Parent);
  end;
  }
begin
  pf:=getParentForm(Self);
  if assigned(pf) and assigned(TCustomForm(pf).Icon)
     and (TCustomForm(pf).Icon.Width>0)
  then icn:=TCustomForm(pf).Icon
  else icn:=Application.Icon;

  FSystemButton.Parent:=nil;
  FSystemButton.Picture.Icon.Assign(icn);
  FSystemButton.Stretch:=true;
  ch:=FCaptionPanel.ClientHeight;
  sz:=deriveCaptionIconHeight;
  FSystemButton.Constraints.MaxHeight:=sz;
  FSystemButton.Constraints.MaxWidth:=sz;
  FSystemButton.BorderSpacing.Around:=(ch-sz)div 2;
  FSystemButton.Parent:=FCaptionPanel;
end;


procedure TCustomMDIPanel.SetParent(NewParent: TWinControl);
var H:integer; //bv:TPanelBevel; bw:integer;  bs:TBorderStyle;
begin
  if Parent = NewParent then exit;
  inherited SetParent(NewParent);
  FParentForm := GetParentForm(Self);
  {
  bv:=BevelOuter;
  bv:=BevelInner;
  bw:=BevelWidth;
  bs:=BorderStyle;
  bw:=BorderWidth;
  }

  if assigned(FSystemButton) then
    with FSystemButton do
    begin
      setDefaultSystemIcon;
      {
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
      }
    end;

  Invalidate;

  FCaptionPanel.AdjustSize;
  H := FCaptionPanel.Height + 2*BorderWidth;
  if BevelInner<>bvNone then H := H + 2*BevelWidth;
  if BevelOuter<>bvNone then H := H + 2*BevelWidth;
  Constraints.MinHeight := H + 4; // +4 - to prevent lower border disappear
  Constraints.MinWidth  := 200;
end;

procedure TCustomMDIPanel.CreateClientPanel;
begin
  FClientPanel := TMDIClientPanel.Create(Self);
  with FClientPanel do
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
    TabStop := true; // to get keys
  end;
end;

resourcestring
  rsMaximize = 'Maximize';
  rsMinimize = 'Minimize';
  rsRestore = 'Restore';
  rsClose = 'Close';

procedure TCustomMDIPanel.DeleteSystemPopupMenu;
var MenuItem: TMenuItem;
begin
  FActionClose.ActionList:=nil;
  FreeAndNil(FActionClose);
  FreeAndNil(FActionList);

  FreeAndNil(FMenuItemMaximize);
  FreeAndNil(FMenuItemMinimize);
  FreeAndNil(FMenuItemRestore);
  FreeAndNil(FMenuItemClose);
  while FSystemPopupMenu.Items.Count > 0 do
    begin
      MenuItem:=FSystemPopupMenu.Items[0];
      FreeAndNil(MenuItem);
    end;
  FreeAndNil(FSystemPopupMenu);
end;

procedure TCustomMDIPanel.CreateSystemPopupMenu;
var
  MenuItem: TMenuItem;
begin
  FSystemPopupMenu := TPopupMenu.Create(Self);

  FMenuItemMaximize := TMenuItem.Create(Self);
  FMenuItemMaximize.Name := 'Maximize';
  FMenuItemMaximize.Caption := rsMaximize;
  FMenuItemMaximize.OnClick := @MaximizeButtonClick;
  FSystemPopupMenu.Items.Add(FMenuItemMaximize);

  FMenuItemMinimize := TMenuItem.Create(Self);
  FMenuItemMinimize.Name := 'Minimize';
  FMenuItemMinimize.Caption := rsMinimize;
  FMenuItemMinimize.OnClick := @MinimizeButtonClick;
  FSystemPopupMenu.Items.Add(FMenuItemMinimize);

  FMenuItemRestore := TMenuItem.Create(Self);
  FMenuItemRestore.Name := 'Restore';
  FMenuItemRestore.Caption := rsRestore;
  FMenuItemRestore.OnClick := @RestoreButtonClick;
  FSystemPopupMenu.Items.Add(FMenuItemRestore);

  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := '-';
  MenuItem.Name := 'Separator';
  FSystemPopupMenu.Items.Add(MenuItem);

  FActionList := TActionList.Create(self);
  FActionClose := TAction.Create(self);
  FActionClose.Name:='ActionClose';
  FActionClose.Caption:=rsClose;
  FActionClose.OnExecute:=@FActionCloseOnExecute;
  FActionClose.ActionList:=FActionList;

  FMenuItemClose := TMenuItem.Create(Self);
  FMenuItemClose.Name := 'Close';
  FMenuItemClose.Caption:=rsClose;
  FMenuItemClose.Action:=FActionClose;
  //FMenuItemClose.OnClick := @CloseButtonClick;
  FSystemPopupMenu.Items.Add(FMenuItemClose);

  self.FCaptionPanel.PopupMenu := FSystemPopupMenu;
end;

{
procedure TCustomMDIPanel.ActionActionSystemMenuExecute(Sender: TObject);
begin

end;

procedure TCustomMDIPanel.CreateSystemActions;
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

procedure TCustomMDIPanel.SetBorderCursor(X, Y: integer);
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

procedure TCustomMDIPanel.BorderMouseDown(Sender: TObject; Button: TMouseButton;
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

procedure TCustomMDIPanel.BorderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
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

    //writeln('setBounds(', L, ',', T, ',', W, ',', H, ')');
    setBounds(L, T, W, H);
  end;
end;

procedure TCustomMDIPanel.BorderMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (WindowPositionState = wpsResizing) then
    WindowPositionState := wpsNone;
end;

procedure TCustomMDIPanel.CaptionPanelMouseDown(Sender: TObject; Button: TMouseButton;
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

procedure TCustomMDIPanel.CaptionPanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  TL, SL: TPoint;
begin
  if (Shift = [ssLeft]) and (WindowPositionState = wpsMoving) then
  begin
    //writeln('m:',Mouse.CursorPos.X, ':', Mouse.CursorPos.Y);
    FCaptionLabel.Cursor := crSizeAll;
    SL := Point(Mouse.CursorPos.X - WindowCaptionMouseX,
                Mouse.CursorPos.Y - WindowCaptionMouseY);
    TL := Parent.ScreenToClient(SL);
    Left := TL.X;
    Top := TL.Y;
    //writeln('w:',TL.X, ':', TL.Y);
  end;
end;

procedure TCustomMDIPanel.CaptionPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (WindowPositionState = wpsMoving) then
  begin
    WindowPositionState := wpsNone;
    FCaptionLabel.Cursor := crDefault;
  end;
end;

procedure TCustomMDIPanel.SetCaption(const Value: TCaption);
begin
  //FCaptionPanel.Caption := Value;
  FCaptionLabel.Caption := Value;
end;

function TCustomMDIPanel.GetCaption: TCaption;
begin
  //Result := FCaptionPanel.Caption;
  Result := FCaptionLabel.Caption;
end;

procedure TCustomMDIPanel.DoClose(CloseAction: TCloseAction);
begin
  if not assigned(self) then exit;
  if FClosing then exit;
  FClosing := true;
  if assigned(FOnClose) then
     FOnClose(self,CloseAction);

  //self.unsetMouseClickProxies;

  FSystemPopupMenu.Close;

  if CloseAction = caHide then
    Visible := False;
  if CloseAction = caMinimize then
    doMinimize;
  if CloseAction = caFree then
  begin
    if (FMDIPanelManager<>nil)
       and (FMDIPanelManager.IndexOf(Self)>=0) then
       FMDIPanelManager.Remove(Self);
    Release;
  end;
  FClosing := false;
end;

procedure TCustomMDIPanel.SystemButtonClick(Sender: TObject);
begin
  FSystemPopupMenu.PopUp;
end;

procedure TCustomMDIPanel.CloseButtonClick(Sender: TObject);
begin
   //Application.QueueAsyncCall(
  DoClose(caFree);
end;


procedure TCustomMDIPanel.OrderButtons;
begin
  if assigned(FRestoreButton) then
    FRestoreButton.Visible := (FWindowState <> wsNormal);
  if assigned(FMaximizeButton) then
    FMaximizeButton.Visible := (FWindowState <> wsMaximized);
  if assigned(FMinimizeButton) then
    FMinimizeButton.Visible := (FWindowState <> wsMinimized);

  FMenuItemMinimize.Enabled := (FWindowState <> wsMinimized);
  FMenuItemMaximize.Enabled := (FWindowState <> wsMaximized);
  FMenuItemRestore.Enabled  := (FWindowState <> wsNormal);

  if assigned(FCloseButton) then
    FCloseButton.Left := 120;
  if assigned(FMaximizeButton) then
    FMaximizeButton.Left := 100;
  if assigned(FRestoreButton) then
    FRestoreButton.Left := 80;
  if assigned(FMinimizeButton) then
    FMinimizeButton.Left := 60;
end;

procedure TCustomMDIPanel.DoMaximize;
begin
  if FWindowState = wsNormal then
    FNormalBounds := Self.BoundsRect;
  Self.setBounds(0, 0, Parent.ClientWidth, Parent.ClientHeight);
  FWindowState := wsMaximized;
  OrderButtons;
end;

procedure TCustomMDIPanel.DoMinimize;
begin
  if FWindowState = wsNormal then
    FNormalBounds := Self.BoundsRect;
  Self.setBounds(0, 0, 200, FCaptionPanel.Height + BevelWidth * 2);
  FWindowState := wsMinimized;
  OrderButtons;
end;

procedure TCustomMDIPanel.DoRestore;
begin
  Self.BoundsRect := FNormalBounds;
  FWindowState := wsNormal;
  OrderButtons;
end;

procedure TCustomMDIPanel.MaximizeButtonClick(Sender: TObject);
begin
  SetActive(true);
  DoMaximize;
end;

procedure TCustomMDIPanel.MinimizeButtonClick(Sender: TObject);
begin
  SetActive(true);
  DoMinimize;
end;

procedure TCustomMDIPanel.RestoreButtonClick(Sender: TObject);
begin
  SetActive(true);
  DoRestore;
end;


function TCustomMDIPanel.GetControl(const Index: integer): TControl;
begin
  Result := FClientPanel.Controls[Index];
end;

function TCustomMDIPanel.GetControlCount: integer;
begin
  Result := FClientPanel.ControlCount;
end;

procedure TCustomMDIPanel.InsertControl(AControl: TControl);
begin
  if (AControl = FCaptionPanel) or (AControl = FClientPanel)
  //or (AControl = FPassiveBevel)
  then
    inherited InsertControl(AControl)
  else
    FClientPanel.InsertControl(AControl);
end;

procedure TCustomMDIPanel.InsertControl(AControl: TControl; Index: integer);
begin
  if (AControl = FCaptionPanel) or (AControl = FClientPanel)
  //or (AControl = FPassiveBevel)
  then
    inherited InsertControl(AControl, Index)
  else
    begin
    // Add to FClientControls during loading from resources.
    // Immediate inserting into FClientPanel causes corruption of sizes and adjustments.
    // We du that later after load
    if fsCreating in FFormState then
      FClientControls.Add(AControl)
    else
      FClientPanel.InsertControl(AControl,index-2); // -2 because FClientPanel and FCaptionPanel

   { if not FActive then
      setMouseClickProxiesRecursively(AControl); }
    end;
end;

procedure TCustomMDIPanel.RemoveControl(AControl: TControl);
begin
  if (AControl = FCaptionPanel) or (AControl = FClientPanel) or
    (AControl = FPassiveBevel) then
    inherited RemoveControl(AControl)
  else
    begin
    FClientPanel.RemoveControl(AControl);
    if not Active then
      //unsetMouseClickProxiesRecursively(AControl);
    end;
end;

{$ifdef Use_MouseClickProxy}
procedure TCustomMDIPanel.unsetMouseClickProxiesRecursively(Ctrl: TControl);
var
  i: integer;
  C: TControl;

  procedure unsetProxyFor(C: TControl);
  var i: integer;
    L: TMouseClickProxy;
  begin
    if not assigned(C) then exit;
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
  if not assigned(Ctrl) then exit;
  if Ctrl is TWinControl then
    for i := 0 to TWinControl(Ctrl).ControlCount - 1 do
    begin
      C := TWinControl(Ctrl).Controls[i];
      unsetMouseClickProxiesRecursively(C);
    end;
  unsetProxyFor(Ctrl);
end;

procedure TCustomMDIPanel.setMouseClickProxiesRecursively(Ctrl: TControl);
var
  i: integer;
  C: TControl;

  procedure setProxyFor(C: TControl);
  var
    L: TMouseClickProxy;
  begin
    if not assigned(C) then exit;
    if not TMouseClickProxy.IsOnClick(C.OnClick) then
    begin
      L := TMouseClickProxy.Create(C);
      L.ProxyOnClick := @PassivePanelOnClick;
      FClickProxies.Add(L);
    end;
  end;

begin
  if not assigned(Ctrl) then exit;
  setProxyFor(Ctrl);
  if Ctrl is TWinControl then
    for i := 0 to TWinControl(Ctrl).ControlCount - 1 do
    begin
      C := TWinControl(Ctrl).Controls[i];
      setMouseClickProxiesRecursively(C);
    end;
end;

procedure TCustomMDIPanel.setMouseClickProxies;
var
  i: integer;
  C: TControl;
  L: TMouseClickProxy;
  M, MP: TMethod;
begin
  setMouseClickProxiesRecursively(FCaptionPanel);
  unsetMouseClickProxiesRecursively(self.FCloseButton);
  setMouseClickProxiesRecursively(FClientPanel);
end;

procedure TCustomMDIPanel.unsetMouseClickProxies;
var
  i: integer;
  C: TControl;
  L: TMouseClickProxy;
begin
  for i := 0 to FClickProxies.Count - 1 do
  begin
    L := TMouseClickProxy(FClickProxies[i]);
    if assigned(L) and assigned(L.FOwner) then
    begin
      L.FOwner.OnClick := L.FOwnerOnClick;
      L.Free;
    end;
  end;
  FClickProxies.Clear;
end;
{$endif Use_MouseClickProxy}

procedure TCustomMDIPanel.InactivateSiblings;
var
  i: integer;
  C: TControl;
begin
  if not assigned(Parent) then
    exit;
  for i := 0 to Parent.ControlCount - 1 do
  begin
    C := Parent.Controls[i];
    if (C is TCustomMDIPanel) and (C <> Self) then
      TCustomMDIPanel(C).setActive(False);
  end;
end;

procedure TCustomMDIPanel.ScreenOnActiveControlChanged(Sender: TObject; LastControl: TControl);
begin
  if LastControl = nil then exit;
  if LastControl.Owner = Self then
    begin
    SetActive(true);
    TCustomForm(Owner).FocusControl(LastControl as TWinControl);
    end
  else
    begin
    SetActive(false);
    end;
end;

procedure TCustomMDIPanel.ClientPanelOnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetActive(true);
  if self.OnMouseDown<>nil then
    self.OnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TCustomMDIPanel.SetActive(val: boolean);
//var bv:TPanelBevel; bw:integer;  bs:TBorderStyle;
begin
  {
  bv:=BevelOuter;
  bv:=BevelInner;
  bw:=BevelWidth;
  bs:=BorderStyle;
  bw:=BorderWidth;
  }
  if val and not self.Focused and not(csLoading in ComponentState)
  then
  begin
    TCustomForm(Owner).ActiveControl:=self;
    TCustomForm(Owner).FocusControl(self);
    if assigned(FOnActivate) then FOnActivate(Self);
  end;

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
    FCaptionPanel.Color := clActiveCaption;
    setZOrder(True);
    InactivateSiblings;
    //unsetMouseClickProxies;
  end
  else
  begin
    //BevelColor := clInactiveBorder;
    //Color := clInactiveBorder;
    //Color := clInactiveCaption;
    //BorderColor:=clInactiveBorder;
    FCaptionPanel.Color := clInactiveCaption;
    //setMouseClickProxies;
    if assigned(FOnDeactivate) then FOnDeactivate(Self);
  end;
  invalidate;
end;

procedure TCustomMDIPanel.PassivePanelOnClick(Sender: TObject);
begin
  setActive(True);
end;

{$ifdef Use_MouseClickProxy}
{----------------------  TMouseClickProxy -------------------------
  This object substitutes OnClick method of a TheOwner control
  with its OnClick method.
  When the OnClick called it will call first a ProxyOnClick method
  then the original Owner OnClick method.
  Here in MDIPanel it is used for interseption of OnClick to children controls of
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
  if assigned(FOwner) then
  begin
    FOwner.onClick := FOwnerOnClick;
    FOwnerOnClick := nil;
    FProxyOnClick := nil;
    FOwner := nil;
  end;
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
{$endif Use_MouseClickProxy}

procedure TMDIClientPanel.ActiveDefaultControlChanged(NewControl: TControl);
begin
  if assigned(NewControl) and assigned(Parent)
     and (Parent is TCustomMDIPanel)
  then
    TCustomMDIPanel(Parent).setActive(true);
end;

{
procedure Register;
begin
  RegisterComponents('MDIPanel', [TMDIPanel]);
end;
}
