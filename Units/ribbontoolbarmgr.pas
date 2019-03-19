unit RibbonToolBarMgr;

{
 Helps to make a ribbon-like panel with multiple toolbars.
 Toolbars are automatically resizeable with wrapping tool buttons.
 Ribbon panel is regular TPanel, usually placed inside TForm with alTop alignment.
 Set AutoSize=true for it.
 There can be couple of TToolBar panels placed inside this Ribbon panel.
 TToolBar panels should have alNone alignment and AutoSize=false.
 Create onResize method for Ribbon panel. Call ArrangeRibbonPanel(RibbonPanel) from it.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList,
  StdActns, ComCtrls, Toolwin,
  ImgList, IntfGraphics, ExtCtrls, StdCtrls, LCLType, Math;
{
type
TLeftSortedList = class(specialize TFPGObjectList<TControl>)
  procedure Sort(Compare: TCompareFunc);
  function SortByLeftAsc(const A1,A2:TControl): Integer;
end;
}
procedure ArrangeRibbonPanel(RibbonPanel: TPanel);

{
procedure AutoSetToolbarHeight(tlbar:TToolBar);
procedure ArrangeRibbonToolBars(rbn:TPanel);
function GetToolbarControlsTotalWidth(tlbar:TToolBar): integer;
function GetToolbarControlsWidth(tlbar:TToolBar): integer;
function GetToolbarControlsWidthTo(tlbar:TToolBar; towidth: integer): integer;
function GetRibbonControlsWidth(rbn:TPanel): integer;
function FindToolbarControlWithLeastRatio(rbn:TPanel): TToolBar;
function FindToolbarWithGreatestRatio(rbn:TPanel): TToolBar;
}


implementation
uses GraphType;

procedure autoSetToolbarHeight(tlbar:TToolBar);
var i, h, b: integer; contr: TControl; tb: TToolButton;
    rawimg:TRawImage; bmp:TCustomBitmap;
begin
  h:=0;
  for i:=0 to tlbar.ControlCount-1 do
    begin
      contr := tlbar.Controls[i];
      b := contr.BorderSpacing.Top + contr.BorderSpacing.Around
           + contr.Top + contr.Height
           + contr.BorderSpacing.Around + contr.BorderSpacing.Bottom;
      if h < b then h := b;

      {contr.Hint:=' L:'+IntToStr(contr.Left)+' T:'+IntToStr(contr.Top)
        +' W:'+IntToStr(contr.Width) +' H:'+IntToStr(contr.Height);
      tb := TToolButton(contr);
      //bmp:=TCustomBitmap.Create(contr.Width,contr.Height);
      tlbar.Images.GetRawImage(tb.ImageIndex, rawimg);
      contr.Hint:= contr.Hint + ' Img: '
        +' W:'+IntToStr(rawimg.Description.Width) +' H:'+IntToStr(rawimg.Description.Height);
      }
    end;
  tlbar.Height := h + tlbar.BorderSpacing.InnerBorder*2;
end;

{
procedure TLeftSortedList.Sort(Compare: TCompareFunc);
  begin
    inherited Sort(@SortByLeftAsc);
  end;

function TLeftSortedList.SortByLeftAsc(const A1,A2:TControl): Integer;
begin
  if (A1.Left > A2.Left) result := 1
  if (A1.Left < A2.Left) result := -1
  else result := 0;
end;
}

function compareByLeft(A1,A2: pointer): Integer;
begin
  result := TControl(A1).Left - TControl(A2).Left;
end;

procedure arrangeRibbonToolBars(rbn:TPanel);
var i,l,t: integer; c,cp: TControl;
  sortedList : TList;
begin
  sortedList := TList.Create;
  for i:=0 to rbn.ControlCount-1 do
      sortedList.Add(rbn.Controls[i]);
  sortedList.Sort(@compareByLeft);

  for i:=0 to sortedList.Count-1 do
  begin
    c := TControl(sortedList.Items[i]);
    t := c.Top; l := c.Left;
    if c is TToolBar then
      autoSetToolbarHeight(TToolBar(c));
    if i > 0 then
    begin
       cp := TControl(sortedList.Items[i-1]);
       t := cp.Top; l := cp.Left;
       c.Left := cp.left + cp.width
              + cp.BorderSpacing.Left + cp.BorderSpacing.Right + cp.BorderSpacing.Around ;
    end
    else c.Left:=2;
  end;

end;

function GetToolbarControlsTotalWidth(tlbar:TToolBar): integer;
var i, w : integer; contr: TControl;
begin
  w:=0;
  for i:=0 to tlbar.ControlCount-1 do
    begin
      contr := tlbar.Controls[i];
      w := w + contr.Width + contr.BorderSpacing.AroundLeft + contr.BorderSpacing.AroundRight;
    end;
  result := w;
end;

// Gets total width of all controls, usually toolbars in Ribbon panel
// it does not inclde InnerBorder of Ribbon
function GetRibbonControlsWidth(rbn:TPanel): integer;
var i, r, w : integer; contr: TControl;
begin
  w:=0;
  for i:=0 to rbn.ControlCount-1 do
    begin
      contr := rbn.Controls[i];
      r := contr.Left + contr.BorderSpacing.Left + contr.BorderSpacing.Around
         + contr.Width + contr.BorderSpacing.Around + contr.BorderSpacing.Right;
      if w < r then w := r;
      //memo1.Append('contr: ' +  contr.Name +' ' + IntToStr(contr.Top) +' '+ IntToStr(contr.Height));
    end;
//  memo1.Append('W: ' + IntToStr(w));
//  Memo1.SelStart := length(Memo1.Text);
  result := w;
end;

// Gets total width of all controls, usually tool buttons in Toolbar
// it does not inclde InnerBorder of Toolbar
function GetToolbarControlsWidth(tlbar:TToolBar): integer;
var i, r, w : integer; contr: TControl;
begin
  w:=0;
  for i:=0 to tlbar.ControlCount-1 do
    begin
      contr := tlbar.Controls[i];
      w := w + contr.BorderSpacing.Left + contr.BorderSpacing.Around
         + contr.Width + contr.BorderSpacing.Around + contr.BorderSpacing.Right;
      //memo1.Append('contr: ' +  contr.Name +' ' + IntToStr(contr.Top) +' '+ IntToStr(contr.Height));
    end;
  result := w;
end;

// Gets distance of all controls, usually tool buttons in Toolbar, before towidth
// We need it to calculate new width
function GetToolbarControlsWidthTo(tlbar:TToolBar; towidth: integer): integer;
var i, r, w : integer; contr: TControl;
begin
  w:=0; r:=0;
  for i:=0 to tlbar.ControlCount-1 do
    begin
      contr := tlbar.Controls[i];
      r := r + contr.BorderSpacing.Left + contr.BorderSpacing.Around
         + contr.Width + contr.BorderSpacing.Around + contr.BorderSpacing.Right;
      if (r < towidth) then w := r;
      //memo1.Append('contr: ' +  contr.Name +' ' + IntToStr(contr.Top) +' '+ IntToStr(contr.Height));
    end;
  result := w;
end;

// finds Toolbar Control with least Ratio of its ClientWidth to its TotalControl width
// This will be first candidate to be reduced
function findToolbarControlWithLeastRatio(rbn:TPanel): TToolBar;
var i, w, ClW, TCW : integer; contr: TToolBar; r: double;
begin
  w:=0; r:=MaxDouble;
  result := nil;
  for i:=0 to rbn.ControlCount-1 do
    if rbn.Controls[i] is TToolBar then
    begin
      contr := TToolBar(rbn.Controls[i]);
      ClW := contr.ClientWidth;
      TCW := GetToolbarControlsWidth(contr);
      if r > (1.0 * ClW / TCW)
      then
         begin
         r := 1.0 * ClW / TCW;
         result := contr;
         end;
    end;
end;

// finds Toolbar Control with greatest Ratio of its ClientWidth to its TotalControl width
// This will be first candidate to be increased
function findToolbarWithGreatestRatio(rbn:TPanel): TToolBar;
var i, w, ClW, TCW : integer; contr: TToolBar; r: double;
begin
  w:=0; r:=0.0;
  result := nil;
  for i:=0 to rbn.ControlCount-1 do
    if rbn.Controls[i] is TToolBar then
    begin
      contr := TToolBar(rbn.Controls[i]);
      ClW := contr.ClientWidth;
      TCW := GetToolbarControlsWidth(contr);
      if r < (1.0 * ClW / TCW)
      then
         begin
         r := 1.0 * ClW / TCW;
         result := contr;
         end;
    end;
end;

procedure ArrangeRibbonPanel(RibbonPanel: TPanel);
var targetWidth, AllToolbarsWidth, w, D: longint;  tlbar: TToolBar;
  edgel, edger: integer;
begin
  AllToolbarsWidth := GetRibbonControlsWidth(RibbonPanel);
  D := RibbonPanel.ClientWidth - AllToolbarsWidth;

  if (AllToolbarsWidth) > RibbonPanel.ClientWidth
  then
  begin
    tlbar := findToolbarWithGreatestRatio(RibbonPanel);
  end
  else
  begin
    tlbar := findToolbarControlWithLeastRatio(RibbonPanel);
  end;

  if ebLeft in tlbar.EdgeBorders then edgel := 2 else edgel := 0;
  if ebRight in tlbar.EdgeBorders then edger := 2 else edger := 0;

  targetWidth := RibbonPanel.ClientWidth - AllToolbarsWidth + tlbar.Width;
  targetWidth := targetWidth - (tlbar.Indent + edgel + edger);
  targetWidth := GetToolbarControlsWidthTo(tlbar, targetWidth);
  tlbar.ClientWidth := targetWidth + (tlbar.Indent + edgel + edger);

  arrangeRibbonToolBars(RibbonPanel);

  RibbonPanel.ReAlign;
end;


end.

