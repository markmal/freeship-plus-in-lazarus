{%MainUnit freehullformwindow_panel.pas}

procedure TFreeHullWindow.CreateComponents;
begin
  ScrollBar1 := TScrollBar.Create(Self);
  with ScrollBar1 do
    begin
    Parent := Self;
    Left := 0;
    Height := 15;
    Constraints.MaxHeight:=15;
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
    Parent := Self;
    Left := 425;
    Height := 270;
    Top := 0;
    Width := 15;
    Constraints.MaxWidth:=15;
    Align := alRight;
    Kind := sbVertical;
    Max := 180;
    Min := -180;
    PageSize := 0;
    Position := 20;
    TabOrder := 1;
    end;
end; // CreateComponents

