{%MainUnit freehullformwindow_panel.pas}

procedure TFreeHullWindow.CreateComponents;
begin
  {FreeHullForm := TFreeHullForm.Create(Self);
  with FreeHullForm do
    begin
    Left := 807;
    Height := 285;
    Top := 201;
    Width := 440;
    Caption := 'FreeHullWindow';
    ClientHeight := 285;
    ClientWidth := 440;
    Color := clBtnFace;
    FormStyle := fsMDIChild;
    OnClose := @FormClose;
    OnCreate := @FormCreate;
    OnKeyPress := @FormKeyPress;
    OnKeyUp := @FormKeyUp;
    OnShow := @FormShow;
    Position := poDefault;
    LCLVersion := '2.0.0.4';
    end;}
  ScrollBar1 := TScrollBar.Create(Self);
  with ScrollBar1 do
    begin
    Parent := Self; //FreeHullForm;
    Left := 0;
    Height := 15;
    Top := 270;
    Width := 440;
    Align := alBottom;
    LargeChange := 5;
    Max := 180;
    Min := -180;
    PageSize := 0;
    Position := 20;
    TabOrder := 0;
    end;
  ScrollBar2 := TScrollBar.Create(Self);
  with ScrollBar2 do
    begin
    Parent := Self; //FreeHullForm;
    Left := 425;
    Height := 270;
    Top := 0;
    Width := 15;
    Align := alRight;
    Kind := sbVertical;
    Max := 180;
    Min := -180;
    PageSize := 0;
    Position := 20;
    TabOrder := 1;
    end;

{  PopupMenu := TPopupMenu.Create(Self);
  with PopupMenu do
    begin
    Parent := FreeHullForm;
    Images := Images;
    AutoPopup := False;
    OnPopup := PopupMenuPopup;
    left := 128;
    top := 40;
  Deselectall1 := TMenuItem.Create(Self);
  with Deselectall1 do
    begin
    PopupMenu.Items.Add(Deselectall1);
    Action := DeselectAll;
    end;
  View1 := TMenuItem.Create(Self);
  with View1 do
    begin
    PopupMenu.Items.Add(View1);
    Caption := 'View';
    ImageIndex := 2;
  Bodyplan1 := TMenuItem.Create(Self);
  with Bodyplan1 do
    begin
    View1.Add(Bodyplan1);
    Action := ViewBodyPlan;
    end;
  Profile1 := TMenuItem.Create(Self);
  with Profile1 do
    begin
    View1.Add(Profile1);
    Action := ViewProfile;
    end;
  Planview1 := TMenuItem.Create(Self);
  with Planview1 do
    begin
    View1.Add(Planview1);
    Action := ViewPlan;
    end;
  Perspective1 := TMenuItem.Create(Self);
  with Perspective1 do
    begin
    View1.Add(Perspective1);
    Action := ViewPerspective;
    end;
    end;
  Zoom1 := TMenuItem.Create(Self);
  with Zoom1 do
    begin
    PopupMenu.Items.Add(Zoom1);
    Caption := 'Zoom';
  ZoomIn1 := TMenuItem.Create(Self);
  with ZoomIn1 do
    begin
    Zoom1.Add(ZoomIn1);
    Action := ZoomIn;
    end;
  Zoomout1 := TMenuItem.Create(Self);
  with Zoomout1 do
    begin
    Zoom1.Add(Zoomout1);
    Action := ZoomOut;
    end;
  All1 := TMenuItem.Create(Self);
  with All1 do
    begin
    Zoom1.Add(All1);
    Action := ZoomExtents;
    end;
    end;
  Camera1 := TMenuItem.Create(Self);
  with Camera1 do
    begin
    PopupMenu.Items.Add(Camera1);
    Caption := 'Camera';
    ImageIndex := 0;
  Widelens28mm1 := TMenuItem.Create(Self);
  with Widelens28mm1 do
    begin
    Camera1.Add(Widelens28mm1);
    Action := WideLens;
    end;
  Standard50mm1 := TMenuItem.Create(Self);
  with Standard50mm1 do
    begin
    Camera1.Add(Standard50mm1);
    Action := StandardLens;
    end;
  Shorttelelens90mm1 := TMenuItem.Create(Self);
  with Shorttelelens90mm1 do
    begin
    Camera1.Add(Shorttelelens90mm1);
    Action := ShortTeleLens;
    end;
  Mediumtelelens130mm1 := TMenuItem.Create(Self);
  with Mediumtelelens130mm1 do
    begin
    Camera1.Add(Mediumtelelens130mm1);
    Action := MediumTeleLens;
    end;
  Longtelelens200mm1 := TMenuItem.Create(Self);
  with Longtelelens200mm1 do
    begin
    Camera1.Add(Longtelelens200mm1);
    Action := LongTeleLens;
    end;
    end;
  Mode1 := TMenuItem.Create(Self);
  with Mode1 do
    begin
    PopupMenu.Items.Add(Mode1);
    Caption := 'Mode';
  Wireframe1 := TMenuItem.Create(Self);
  with Wireframe1 do
    begin
    Mode1.Add(Wireframe1);
    Action := ShowWireFrame;
    end;
  Shade1 := TMenuItem.Create(Self);
  with Shade1 do
    begin
    Mode1.Add(Shade1);
    Action := ShowFlatShade;
    end;
  Gausscurvature1 := TMenuItem.Create(Self);
  with Gausscurvature1 do
    begin
    Mode1.Add(Gausscurvature1);
    Action := ShowGaussCurvature;
    end;
  Zebrashading1 := TMenuItem.Create(Self);
  with Zebrashading1 do
    begin
    Mode1.Add(Zebrashading1);
    Action := ShadeZebra;
    end;
  Developablitycheck1 := TMenuItem.Create(Self);
  with Developablitycheck1 do
    begin
    Mode1.Add(Developablitycheck1);
    Action := ShowDevelopablity;
    end;
    end;
  Backgroundimage2 := TMenuItem.Create(Self);
  with Backgroundimage2 do
    begin
    PopupMenu.Items.Add(Backgroundimage2);
    Caption := 'Backgroundimage';
    ImageIndex := 7;
  Visible1 := TMenuItem.Create(Self);
  with Visible1 do
    begin
    Backgroundimage2.Add(Visible1);
    Action := BackgroundVisible;
    end;
  Clear1 := TMenuItem.Create(Self);
  with Clear1 do
    begin
    Backgroundimage2.Add(Clear1);
    Action := Backgroundclear;
    end;
  Backgroundimage1 := TMenuItem.Create(Self);
  with Backgroundimage1 do
    begin
    Backgroundimage2.Add(Backgroundimage1);
    Action := ImportBackGround;
    end;
  Export1 := TMenuItem.Create(Self);
  with Export1 do
    begin
    Backgroundimage2.Add(Export1);
    Action := BackgroundExport;
    end;
  Origin1 := TMenuItem.Create(Self);
  with Origin1 do
    begin
    Backgroundimage2.Add(Origin1);
    Action := BackgroundOrigin;
    end;
  Setscale1 := TMenuItem.Create(Self);
  with Setscale1 do
    begin
    Backgroundimage2.Add(Setscale1);
    Action := BackgroundScale;
    end;
  ransparentcolor1 := TMenuItem.Create(Self);
  with ransparentcolor1 do
    begin
    Backgroundimage2.Add(ransparentcolor1);
    Action := BackgroundTransparentColor;
    end;
  Tolerance1 := TMenuItem.Create(Self);
  with Tolerance1 do
    begin
    Backgroundimage2.Add(Tolerance1);
    Action := BackgroundTolerance;
    end;
  Blending1 := TMenuItem.Create(Self);
  with Blending1 do
    begin
    Backgroundimage2.Add(Blending1);
    Action := BackgroundBlending;
    end;
    end;
  Print1 := TMenuItem.Create(Self);
  with Print1 do
    begin
    PopupMenu.Items.Add(Print1);
    Action := Print;
    end;
  Saveimage1 := TMenuItem.Create(Self);
  with Saveimage1 do
    begin
    PopupMenu.Items.Add(Saveimage1);
    Action := SaveAsBitmap;
    end;
    end;
  }

  {
  ActionListHull := TActionList.Create(Self);
  with ActionListHull do
    begin
    Parent := FreeHullForm;
    Images := Self.Images;
    left := 48;
    top := 40;
  end;

  StandardLens := TAction.Create(Self);
  with StandardLens do
    begin
    ActionList := ActionListHull;
    Category := 'Camera';
    Caption := 'Standardlens50mm.';
    OnExecute := StandardLensExecute;
    end;
  WideLens := TAction.Create(Self);
  with WideLens do
    begin
    ActionList := ActionListHull;
    Category := 'Camera';
    Caption := 'Widelens28mm.';
    OnExecute := WideLensExecute;
    end;
  ShortTeleLens := TAction.Create(Self);
  with ShortTeleLens do
    begin
    ActionList := ActionListHull;
    Category := 'Camera';
    Caption := 'Shorttelelens90mm.';
    OnExecute := ShortTeleLensExecute;
    end;
  MediumTeleLens := TAction.Create(Self);
  with MediumTeleLens do
    begin
    ActionList := ActionListHull;
    Category := 'Camera';
    Caption := 'Mediumtelelens130mm.';
    OnExecute := MediumTeleLensExecute;
    end;
  LongTeleLens := TAction.Create(Self);
  with LongTeleLens do
    begin
    ActionList := ActionListHull;
    Category := 'Camera';
    Caption := 'Longtelelens200mm.';
    OnExecute := LongTeleLensExecute;
    end;
  ViewBodyPlan := TAction.Create(Self);
  with ViewBodyPlan do
    begin
    ActionList := ActionListHull;
    Category := 'View';
    Caption := 'Bodyplan';
    OnExecute := ViewBodyPlanExecute;
    ShortCut := 16433;
    end;
  ViewProfile := TAction.Create(Self);
  with ViewProfile do
    begin
    ActionList := ActionListHull;
    Category := 'View';
    Caption := 'Profile';
    OnExecute := ViewProfileExecute;
    ShortCut := 16434;
    end;
  ViewPlan := TAction.Create(Self);
  with ViewPlan do
    begin
    ActionList := ActionListHull;
    Category := 'View';
    Caption := 'Planview';
    OnExecute := ViewPlanExecute;
    ShortCut := 16435;
    end;
  ViewPerspective := TAction.Create(Self);
  with ViewPerspective do
    begin
    ActionList := ActionListHull;
    Category := 'View';
    Caption := 'Perspective';
    OnExecute := ViewPerspectiveExecute;
    ShortCut := 16436;
    end;
  ZoomIn := TAction.Create(Self);
  with ZoomIn do
    begin
    ActionList := ActionListHull;
    Category := 'Zoom';
    Caption := 'Zoomin';
    ImageIndex := 5;
    OnExecute := ZoomInExecute;
    ShortCut := 16457;
    end;
  ZoomExtents := TAction.Create(Self);
  with ZoomExtents do
    begin
    ActionList := ActionListHull;
    Category := 'Zoom';
    Caption := 'All';
    ImageIndex := 4;
    OnExecute := ZoomExtentsExecute;
    ShortCut := 16449;
    end;
  ZoomOut := TAction.Create(Self);
  with ZoomOut do
    begin
    ActionList := ActionListHull;
    Category := 'Zoom';
    Caption := 'Zoomout';
    ImageIndex := 6;
    OnExecute := ZoomOutExecute;
    ShortCut := 16463;
    end;
  DeselectAll := TAction.Create(Self);
  with DeselectAll do
    begin
    ActionList := ActionListHull;
    Caption := 'DeselectallEsc';
    OnExecute := DeselectAllExecute;
    end;
  Print := TAction.Create(Self);
  with Print do
    begin
    ActionList := ActionListHull;
    Caption := 'Print';
    ImageIndex := 3;
    OnExecute := PrintExecute;
    end;
  ShowWireFrame := TAction.Create(Self);
  with ShowWireFrame do
    begin
    ActionList := ActionListHull;
    Category := 'Mode';
    Caption := 'Wireframe';
    OnExecute := ShowWireFrameExecute;
    ShortCut := 16471;
    end;
  ShowFlatShade := TAction.Create(Self);
  with ShowFlatShade do
    begin
    ActionList := ActionListHull;
    Category := 'Mode';
    Caption := 'Shade';
    OnExecute := ShowFlatShadeExecute;
    ShortCut := 16454;
    end;
  ShowGaussCurvature := TAction.Create(Self);
  with ShowGaussCurvature do
    begin
    ActionList := ActionListHull;
    Category := 'Mode';
    Caption := 'Gaussiancurvature';
    OnExecute := ShowGaussCurvatureExecute;
    ShortCut := 16455;
    end;
  ShowDevelopablity := TAction.Create(Self);
  with ShowDevelopablity do
    begin
    ActionList := ActionListHull;
    Category := 'Mode';
    Caption := 'Developablitycheck';
    OnExecute := ShowDevelopablityExecute;
    ShortCut := 16452;
    end;
  SaveAsBitmap := TAction.Create(Self);
  with SaveAsBitmap do
    begin
    ActionList := ActionListHull;
    Caption := 'Saveimage';
    OnExecute := SaveAsBitmapExecute;
    end;
  ShadeZebra := TAction.Create(Self);
  with ShadeZebra do
    begin
    ActionList := ActionListHull;
    Category := 'Mode';
    Caption := 'Zebrashading';
    OnExecute := ShadeZebraExecute;
    ShortCut := 16453;
    end;
  ImportBackGround := TAction.Create(Self);
  with ImportBackGround do
    begin
    ActionList := ActionListHull;
    Category := 'Background';
    Caption := 'Load';
    ImageIndex := 9;
    OnExecute := ImportBackGroundExecute;
    end;
  BackgroundOrigin := TAction.Create(Self);
  with BackgroundOrigin do
    begin
    ActionList := ActionListHull;
    Category := 'Background';
    Caption := 'Origin';
    OnExecute := BackgroundOriginExecute;
    end;
  BackgroundScale := TAction.Create(Self);
  with BackgroundScale do
    begin
    ActionList := ActionListHull;
    Category := 'Background';
    Caption := 'Setscale';
    OnExecute := BackgroundScaleExecute;
    end;
  BackgroundTransparentColor := TAction.Create(Self);
  with BackgroundTransparentColor do
    begin
    ActionList := ActionListHull;
    Category := 'Background';
    Caption := 'Transparentcolor';
    OnExecute := BackgroundTransparentColorExecute;
    end;
  Backgroundclear := TAction.Create(Self);
  with Backgroundclear do
    begin
    ActionList := ActionListHull;
    Category := 'Background';
    Caption := 'Clear';
    OnExecute := BackgroundclearExecute;
    end;
  BackgroundBlending := TAction.Create(Self);
  with BackgroundBlending do
    begin
    ActionList := ActionListHull;
    Category := 'Background';
    Caption := 'Blending';
    OnExecute := BackgroundBlendingExecute;
    end;
  BackgroundExport := TAction.Create(Self);
  with BackgroundExport do
    begin
    ActionList := ActionListHull;
    Category := 'Background';
    Caption := 'Save';
    ImageIndex := 8;
    OnExecute := BackgroundExportExecute;
    end;
  BackgroundTolerance := TAction.Create(Self);
  with BackgroundTolerance do
    begin
    ActionList := ActionListHull;
    Category := 'Background';
    Caption := 'Tolerance';
    OnExecute := BackgroundToleranceExecute;
    end;
  BackgroundVisible := TAction.Create(Self);
  with BackgroundVisible do
    begin
    ActionList := ActionListHull;
    Category := 'Background';
    Caption := 'Visible';
    OnExecute := BackgroundVisibleExecute;
    end;
  }
end; // CreateComponents

