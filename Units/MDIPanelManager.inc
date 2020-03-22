{%MainUnit MDIPanel.pas}

{ TMDIPanelManager }
function TMDIPanelManager.GetMDIPanel(Index: Integer): TCustomMDIPanel;
begin
  Result := TCustomMDIPanel(FMDIPanels[Index]);
end;

function TMDIPanelManager.GetPanelCount: integer;
begin
  result := FMDIPanels.Count;
end;

constructor TMDIPanelManager.Create;
begin
  inherited Create;
  FMDIPanels:= TFPList.Create;
end;

destructor TMDIPanelManager.Destroy;
begin
  FreeAndNil(FMDIPanels);
end;

function TMDIPanelManager.IndexOf(APanel: TCustomMDIPanel): integer;
begin
  result := FMDIPanels.IndexOf(APanel);
end;

procedure TMDIPanelManager.Add(APanel: TCustomMDIPanel);
begin
  if APanel = nil then exit;
  FMDIPanels.Add(APanel);
  APanel.MDIPanelManager := self;
end;

procedure TMDIPanelManager.Insert(APanel: TCustomMDIPanel);
begin
  Insert(APanel,PanelCount);
  APanel.MDIPanelManager := self;
end;

procedure TMDIPanelManager.Insert(APanel: TCustomMDIPanel; Index: integer);
begin
  if APanel = nil then exit;
  FMDIPanels.Insert(Index, APanel);
  APanel.MDIPanelManager := self;
end;

procedure TMDIPanelManager.Remove(APanel: TCustomMDIPanel);
begin
  if APanel = nil then exit;
  FMDIPanels.Remove(APanel);
  APanel.FMDIPanelManager := nil;
end;

procedure TMDIPanelManager.Delete(Index: integer);
begin
  FMDIPanels.Delete(Index);
end;

function TMDIPanelManager.FindActivePanel: TCustomMDIPanel;
var i:integer;
begin
  result:=nil;
  for i:=0 to FMDIPanels.Count - 1 do
    if TCustomMDIPanel(FMDIPanels[i]).Active then
      result:=TCustomMDIPanel(FMDIPanels[i]);
end;

procedure TMDIPanelManager.Tile;
var T,L,MW,MH, W,H, i, MCC, r,c,rows,cols : Integer;
    panel : TCustomMDIPanel;
    parent:TWinControl;
    swc:TScrollingWinControl;
begin
 if FMDIPanels.Count = 0 then exit;
 parent:=MDIPanels[0].Parent; // we get just parent of first MDIPanel. Assumed that all have same parent.
 if parent = nil then exit;

 MCC := FMDIPanels.Count;
 rows:=round(sqrt(MCC));
 cols:=round(sqrt(MCC));

 MW := parent.ClientWidth;
 MH := parent.ClientHeight;

 if MCC=2 then
   if MW > ((MH * 3) div 2)
     then cols:=2
     else rows:=2;

 if (parent is TScrollingWinControl) then
 begin
   swc := TScrollingWinControl(parent);
   if (parent is TCustomForm) and TCustomForm(parent).AutoScroll then
   begin
     if ((swc.HorzScrollBar<>nil) and swc.HorzScrollBar.Visible)
       or((swc.VertScrollBar<>nil) and swc.VertScrollBar.Visible) then
       begin
       MH := MH - swc.HorzScrollBar.Size;
       MW := MW - swc.VertScrollBar.Size;
       end;
   end
   else
   begin
     if (swc.HorzScrollBar<>nil) and swc.HorzScrollBar.IsScrollBarVisible then
       MH := MH - swc.HorzScrollBar.Size;
     if (swc.VertScrollBar<>nil) and swc.VertScrollBar.IsScrollBarVisible then
       MW := MW - swc.VertScrollBar.Size;
   end
 end;

 W := MW div cols;
 H := MH div rows;
 T := 0; L := 0;

 c:=0; r:=0;
 for i:= 0 to MCC - 1 do
   begin
   panel := MDIPanels[i];
   if Assigned(panel) then
     begin
     panel.SetBounds(L+(c*W), T+(r*H), W, H);
     inc(c);
     if c >= cols then begin c:=0; inc(r); end;
     end;
   end;

end;{TMDIPanelManager.Tile}

procedure TMDIPanelManager.Cascade;
var MW,MH, W,H, i, MCC, CH,BW, dH,dW : Integer;
    panel : TCustomMDIPanel;
    parent:TWinControl;
begin
  CH := GetSystemMetrics(SM_CYCAPTION); //header height
  BW := GetSystemMetrics(SM_CYDLGFRAME); //frame width
  if FMDIPanels.Count = 0 then exit;
  parent:=MDIPanels[0].Parent; // we get just parent of first MDIPanel. Assumed that all have same parent.
  if parent = nil then exit;
  MW := parent.ClientWidth;
  MH := parent.ClientHeight;
  MCC := FMDIPanels.Count;
  panel := MDIPanels[0];
  CH := panel.CaptionPanel.Height;
  BW := panel.BorderWidth;
  dW:=CH+BW*2;
  dH:=CH+BW;
  W := MW-dW*MCC;
  H := MH-dH*MCC;
  if W < panel.Constraints.MinWidth then W:=panel.Constraints.MinWidth;
  if H < panel.Constraints.MinHeight then H:=panel.Constraints.MinHeight;
  for i:= 0 to MCC - 1 do
  begin
    panel:=MDIPanels[i];
    panel.SetBounds(i*dW,i*dH, W,H);
  end;
end;{TMDIPanelManager.Cascade}

