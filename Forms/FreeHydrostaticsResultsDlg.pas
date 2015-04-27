{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    e-mail                  : Info@FREEship.org                                              }
{    FREE!ship project page  : https://sourceforge.net/projects/freeship                      }
{    FREE!ship homepage      : www.FREEship.org                                               }
{                                                                                             }
{    This program is free software; you can redistribute it and/or modify it under            }
{    the terms of the GNU General Public License as published by the                          }
{    Free Software Foundation; either version 2 of the License, or (at your option)           }
{    any later version.                                                                       }
{                                                                                             }
{    This program is distributed in the hope that it will be useful, but WITHOUT ANY          }
{    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          }
{    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 }
{                                                                                             }
{    You should have received a copy of the GNU General Public License along with             }
{    this program; if not, write to the Free Software Foundation, Inc.,                       }
{    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    }
{                                                                                             }
{#############################################################################################}

unit FreeHydrostaticsResultsDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
     {$ifdef Windows}
     Windows,
     {$endif}
     {$ifdef LCL}
     LCLIntf, LCLType, LMessages, LResources,
     PrintersDlgs, Printer4Lazarus, FreePrinter,
     {$endif}
     Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     Grids,
     ExtCtrls,
     StdCtrls,
     Printers,
     Buttons;

type TFreeHydrostaticsResultsDialog  = class(TForm)
                                             Panel1: TPanel;
                                             Panel2: TPanel;
                                             Grid: TStringGrid;
                                             Header: TMemo;
                                             Splitter1: TSplitter;
                                             SpeedButton1: TSpeedButton;
                                             SpeedButton2: TSpeedButton;
                                             Button1: TSpeedButton;
    PrintDialog: TPrintDialog;
    SaveDialog: TSaveDialog;
                                             procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;Rect: TRect; State: TGridDrawState);
                                             procedure FormResize(Sender: TObject);
                                             procedure Button1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
                                           private { Private declarations }
                                              ColWidths : array of Integer;
                                           public  { Public declarations }
                                              function Execute:Boolean;
                                        end;

var FreeHydrostaticsResultsDialog: TFreeHydrostaticsResultsDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFreeHydrostaticsResultsDialog.GridDrawCell(Sender: TObject;ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var W    : Integer;
    Str  : string;
    Back : TColor;
begin
   if (ACol=0) or (ARow in [0,1]) then
   begin
      if Grid.Canvas.Font.Style=[] then Grid.Canvas.Font.Style:=[fsBold];
   end else
   begin
      if Grid.Canvas.Font.Style=[fsBold] then Grid.Canvas.Font.Style:=[];
   end;
   if (ARow>1) and (ACol>0) then
   begin
      if gdSelected in state then
      begin
         //Back:=clWhite;
         //Grid.Canvas.Font.Color:=clBlack;
      end else if Odd(ARow) then
      begin
         Back:=RGB(235,235,235);
         if Grid.Canvas.Brush.Color<>Back then Grid.Canvas.Brush.Color:=Back;
      end else
      begin
         if Grid.Canvas.Brush.Color<>clWindow then Grid.Canvas.Brush.Color:=clWindow;
      end;
      Grid.Canvas.Rectangle(Rect);
   end;
   Str:=Grid.Cells[ACol,ARow];
   W:=Grid.Canvas.TextWidth(Str);

   if (ARow in [0,1]) or (ACol = 0)
     then
      begin
        Grid.Canvas.FillRect(Rect);
        Grid.Canvas.TextRect(Rect,(Rect.Left+Rect.Right-W) div 2,Rect.Top+3,Str);
      end
     else Grid.Canvas.TextRect(Rect,Rect.Right-W-5,Rect.Top+3,Str);

end;{TFreeHydrostaticsResultsDialog.GridDrawCell}

function TFreeHydrostaticsResultsDialog.Execute:Boolean;
var I : Integer;
begin
   // Store the column width for resizing purposes
   setlength(ColWidths,Grid.ColCount);
   for I:=1 to Grid.ColCount do ColWidths[I-1]:=Grid.ColWidths[I-1];
   ShowModal;
   Result:=modalresult=mrOK;
end;{TFreeHydrostaticsResultsDialog.Execute}

procedure TFreeHydrostaticsResultsDialog.FormResize(Sender: TObject);
var I,Total : Integer;
    remain  : Integer;
    NewW    : Integer;
    Fraction: Double;
begin
   Button1.Left:=Panel1.Width-Button1.Width-5;
   Total:=0;
   for I:=1 to Grid.ColCount do Inc(Total,ColWidths[I-1]);
   if Total<>0 then
   begin
      Remain:=Grid.ClientWidth-10;
      for I:=1 to Grid.ColCount do
      begin
         Fraction:=ColWidths[I-1]/Total;// percentage of total width when shown for the first time
         NewW:=trunc(Fraction*remain);
         Grid.ColWidths[I-1]:=NewW;
      end;
   end;
end;{TFreeHydrostaticsResultsDialog.FormResize}

procedure TFreeHydrostaticsResultsDialog.Button1Click(Sender: TObject);
begin
   ModalResult:=mrOK;
end;{TFreeHydrostaticsResultsDialog.Button1Click}

procedure TFreeHydrostaticsResultsDialog.SpeedButton2Click(Sender: TObject);
var Strings    : TStringList;
    MaxWidth   : array of integer;
    I,J,L,N    : Integer;
    Str,Tmp    : Ansistring;
begin
   if SaveDialog.Execute then
   begin
      strings:=TStringList.Create;
      Strings.Assign(Header.Lines);
      Setlength(MaxWidth,Grid.ColCount);
      for I:=1 to Grid.ColCount do
      begin
         MaxWidth[I-1]:=0;
         for J:=1 to Grid.RowCount do
         begin
            L:=Length(Grid.Cells[I-1,J-1]);
            if L>MaxWidth[I-1] then MaxWidth[I-1]:=L;
         end;
      end;
      Strings.Add('');
      Strings.Add('');
      for I:=1 to Grid.RowCount do
      begin
         Str:='';
         for J:=1 to Grid.ColCount do
         begin
            Tmp:=Grid.Cells[J-1,I-1];
            L:=Length(Tmp);
            for N:=L+1 to MaxWidth[J-1] do Tmp:=#32+Tmp;
            if J=1 then Str:=Tmp
                   else Str:=Str+#32+Tmp;
         end;
         Strings.Add(Str);
      end;
      // Skip translation
      Strings.SaveToFile(ChangeFileExt(SaveDialog.Filename,'.txt'));
      // End Skip translation
      Strings.Destroy;
   end;
end;{TFreeHydrostaticsResultsDialog.SpeedButton2Click}

procedure TFreeHydrostaticsResultsDialog.SpeedButton1Click(Sender: TObject);
var PrintText : TextFile;
    MaxWidth  : array of integer;
    I,J,L,N   : Integer;
    Str,Tmp   : Ansistring;
begin
   if PrintDialog.Execute then
   begin
      AssignPrn(PrintText);
      Rewrite(PrintText);
      Printer.Canvas.Font.Assign(Header.Font);
      for I:=1 to Header.Lines.Count do Writeln(PrintText,#32,Header.Lines[I-1]);

      Setlength(MaxWidth,Grid.ColCount);
      for I:=1 to Grid.ColCount do
      begin
         MaxWidth[I-1]:=0;
         for J:=1 to Grid.RowCount do
         begin
            L:=Length(Grid.Cells[I-1,J-1]);
            if L>MaxWidth[I-1] then MaxWidth[I-1]:=L;
         end;
      end;
      Writeln(PrintText);
      Writeln(PrintText);
      Printer.Canvas.Font.Size:=Printer.Canvas.Font.Size-1;
      for I:=1 to Grid.RowCount do
      begin
         Str:='';
         for J:=1 to Grid.ColCount do
         begin
            Tmp:=Grid.Cells[J-1,I-1];
            L:=Length(Tmp);
            for N:=L+1 to MaxWidth[J-1] do Tmp:=#32+Tmp;
            if J=1 then Str:=Tmp
                   else Str:=Str+#32+Tmp;
         end;
         Writeln(PrintText,#32#32,Str);
      end;
      CloseFile(PrintText);
   end;
end;{TFreeHydrostaticsResultsDialog.SpeedButton1Click}

end.

