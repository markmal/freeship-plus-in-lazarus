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

unit FreeControlPointFrm;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     Math,
     FreeTypes,
     FreeGeometry,
     StdCtrls, Buttons, ExtCtrls, Spin;

type

{ TFreeControlPointForm }

 TFreeControlPointForm  = class(TForm)
    EditX: TFloatSpinEdit;
    LabelX: TLabel;
    EditY: TFloatSpinEdit;
    EditZ: TFloatSpinEdit;
    LabelY: TLabel;
    LabelZ: TLabel;
    CheckBoxCorner: TCheckBox;
    Label6: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Label4: TLabel;
    EditDistance: TEdit;
    Label5: TLabel;
    EditAngles: TEdit;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBoxCornerClick(Sender: TObject);
    procedure EditXChange(Sender: TObject);
    procedure EditXEditingDone(Sender: TObject);
    procedure EditXKeyPress(Sender: TObject; var Key: Char);
    procedure EditXExit(Sender: TObject);
    procedure EditYChange(Sender: TObject);
    procedure EditYEditingDone(Sender: TObject);
    procedure EditYKeyPress(Sender: TObject; var Key: Char);
    procedure EditYExit(Sender: TObject);
    procedure EditZChange(Sender: TObject);
    procedure EditZEditingDone(Sender: TObject);
    procedure EditZKeyPress(Sender: TObject; var Key: Char);
    procedure EditZExit(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
   private  { Private declarations }
      FActiveControlPoint  : TFreeSubdivisionControlPoint;
      FFreeShip            : TComponent;
      FSkipCheckbox1OnClick: boolean;
      procedure FSetActiveControlPoint(Val:TFreeSubdivisionControlPoint);
      procedure FSetActiveControlPointCorner(isCorner: boolean);
   public   { Public declarations }
      property ActiveControlPoint   : TFreeSubdivisionControlPoint read FActiveControlPoint write FSetActiveControlPoint;
      property FreeShip             : TComponent read FFreeShip write FFreeShip;
end;

var FreeControlPointForm: TFreeControlPointForm;

implementation

uses FreeLanguageSupport,
     FreeShipUnit;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFreeControlPointForm.FSetActiveControlPoint(Val:TFreeSubdivisionControlPoint);
var I,N : Integer;
    BCol:TColor;
    FCol:TColor;
    Npoi: Integer;  
    R,R1,R2,alfa,beta,gamma,acos,len    : single;
    cos1a,cos2a,cos1b,cos2b,cos1g,cos2g : single;
begin
   FActiveControlPoint:=Val;
   if ActiveControlPoint=nil then
   begin
      Visible:=False;
      EditX.Text:='';
      EditY.Text:='';
      EditZ.Text:='';
      CheckBoxCorner.Checked:=false;
   end else
   begin
      R:=0.0;
      EditDistance.Text := '0.0';
      Npoi:=TFreeShip(FreeShip).NumberOfSelectedControlPoints-1;
      if  Npoi>= 1 then begin
          Label5.Caption:=Userstring(1477);
          R:=SQRT(Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.X)+
          Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Y)+
          Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Z));
          EditDistance.Text := Truncate(R,4);
          EditAngles.Text :='0.0';
          end    
      else begin
          Label5.Caption:=Userstring(1477);
          EditDistance.Text :='0.0';
      end;

      if  Npoi>= 4 then begin
          Label5.Caption:=Userstring(1477);
          R:=SQRT(Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.X)+
          Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Y)+
          Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Z));
          EditDistance.Text := Truncate(R,4);
          len:=0;		  
          for i:=1 to Npoi do begin
           R:=SQRT(Sqr(TFreeShip(FreeShip).SelectedControlPoint[i-1].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[i].Coordinate.X)+
           Sqr(TFreeShip(FreeShip).SelectedControlPoint[i-1].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[i].Coordinate.Y)+
           Sqr(TFreeShip(FreeShip).SelectedControlPoint[i-1].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[i].Coordinate.Z));		  
		   len:=len+R;
          end;
          Label5.Caption:=Userstring(1493);
          EditAngles.Text := Truncate(len,4);
      end;

	  
      if R=0 then R:=0.0001;
      if  Npoi=1 then begin
        Label5.Caption:=Userstring(1477);
        alfa:=abs(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.X)/R;
        beta:=abs(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Y)/R;
        gamma:=abs(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Z)/R;
        EditAngles.Text := Truncate(ArcCos(alfa)*57.29578,2)+';'+Truncate(ArcCos(beta)*57.29578,2)+';'+Truncate(ArcCos(gamma)*57.29578,2);
{
        alfa:=(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X)/R;
        beta:=(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y)/R;
        gamma:=(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z)/R;		
        EditAngles.Text := Truncate(alfa,2)+';'+Truncate(beta,2)+';'+Truncate(gamma,2);
}		
        end
      else if Npoi=0 then begin
        Label5.Caption:=Userstring(1477); 
        R:=SQRT(Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X)+
                Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y)+
                Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z));		
        if R=0 then R:=0.0001;				
        alfa:=TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X/R;
        beta:=TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y/R;
        gamma:=TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z/R;
        EditAngles.Text := Truncate(ArcCos(alfa)*57.29578,2)+';'+Truncate(ArcCos(beta)*57.29578,2)+';'+Truncate(ArcCos(gamma)*57.29578,2);
      end;
      if  Npoi=2 then begin
        Label5.Caption:=Userstring(1476);
        R1:=SQRT(Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.X)+
        Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Y)+
        Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Z));
        R2:=SQRT(Sqr(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.X)+
        Sqr(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.Y)+
        Sqr(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.Z));
        cos1a:=abs(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.X)/R1;
        cos1b:=abs(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Y)/R1;
        cos1g:=abs(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Z)/R1;
        cos2a:=abs(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.X)/R2;
        cos2b:=abs(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.Y)/R2;
        cos2g:=abs(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.Z)/R2;
        acos:=cos1a*cos2a+cos1b*cos2b+cos1g*cos2g; 
        if acos>1 then acos:=1; 
        if acos<-1 then acos:=-1; 
        EditAngles.Text := Truncate(ArcCos(acos)*57.29578,2);
      end;
      if  Npoi=3 then begin
        Label5.Caption:=Userstring(1476);
        R1:=SQRT(Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.X)+
        Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Y)+
        Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Z));
        R2:=SQRT(Sqr(TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[3].Coordinate.X)+
        Sqr(TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[3].Coordinate.Y)+
        Sqr(TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[3].Coordinate.Z));
        cos1a:=(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.X)/R1;
        cos1b:=(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Y)/R1;
        cos1g:=(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Z)/R1;
        cos2a:=(TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[3].Coordinate.X)/R2;
        cos2b:=(TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[3].Coordinate.Y)/R2;
        cos2g:=(TFreeShip(FreeShip).SelectedControlPoint[2].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[3].Coordinate.Z)/R2;
        acos:=cos1a*cos2a+cos1b*cos2b+cos1g*cos2g; 
        if acos>1 then acos:=1; 
        if acos<-1 then acos:=-1; 
        EditAngles.Text := Truncate(ArcCos(acos)*57.29578,2);
      end;
	  
      EditX.Text:=Truncate(FActiveControlPoint.Coordinate.X,4);
      EditY.Text:=Truncate(FActiveControlPoint.Coordinate.Y,4);
      EditZ.Text:=Truncate(FActiveControlPoint.Coordinate.Z,4);

      if (CheckBoxCorner.Checked <> (FActiveControlPoint.VertexType=svCorner))
         then begin
            FSkipCheckbox1OnClick:=true;
            CheckBoxCorner.Checked:=FActiveControlPoint.VertexType=svCorner;
            FSkipCheckbox1OnClick:=false;
         end;

      // Count the number of crease edges connected to this point
      N:=0;
      for I:=1 to FActiveControlPoint.NumberOfEdges do if FActiveControlPoint.Edge[I-1].Crease then inc(N);
      CheckBoxCorner.Enabled:=((N>0) and (N<3) and (not Val.Locked)); // points with more than two crease edges must always be a corner

      EditX.Enabled:=not Val.Locked;
      EditY.Enabled:=not Val.Locked;
      EditZ.Enabled:=not Val.Locked;
      if Val.Locked then
      begin
         BCol:=clBtnFace;
         FCol:=clDkgray;
      end else
      begin
         BCol:=clWindow;
         FCol:=clBlack;
      end;
      if EditX.Color<>BCol then EditX.Color:=BCol;
      if EditX.Font.Color<>FCol then EditX.Font.Color:=FCol;
      if EditY.Color<>BCol then EditY.Color:=BCol;
      if EditY.Font.Color<>FCol then EditY.Font.Color:=FCol;
      if EditZ.Color<>BCol then EditZ.Color:=BCol;
      if EditZ.Font.Color<>FCol then EditZ.Font.Color:=FCol;
   end;
end;{TFreeControlPointForm.FSetActiveControlPoint}

procedure TFreeControlPointForm.EditXKeyPress(Sender: TObject;var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-','@',#13]) or
      ((TFreeShip(FreeShip).ProjectSettings.ProjectUnits=fuImperial) and (Key='+')) or
      (Key=FormatSettings.DecimalSeparator) then else key:=#0; //SAP: added the '@'
   if Key=#13 then EditXExit(Self);
end;{TFreeControlPointForm.Edit1KeyPress}

procedure TFreeControlPointForm.EditXExit(Sender: TObject);
begin
end;

procedure TFreeControlPointForm.EditYChange(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      //P.Y:=P.Y+TFreeShip(FreeShip).Visibility.CursorIncrement;
      P.Y:=EditY.Value;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;


procedure TFreeControlPointForm.EditXEditingDone(Sender: TObject);
var P    : T3DCoordinate;
    Val  : TFloatType;
    I    : Integer;
    saved: Boolean;
begin
   saved := false;
   if ActiveControlPoint<>nil then
   begin
// do only something, if the value has really been changed:
      P:=ActiveControlPoint.Coordinate;
      Val := ConvertCoordinate(EditX.Text, P.X);
      if (abs(P.X-Val)>1e-4) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
         I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            Val:=ConvertCoordinate(EditX.Text, P.X);
            if abs(P.X-Val)>1e-5 then
            begin
               if not saved then
               begin
                  TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(210),True);
                  saved := true;
               end;
               P.X:=Val;
               TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate := P;
            end;
            Inc(I);
         end;
//  finally update the text field:
         if EditX.Text<>'' then
         begin
            P:=ActiveControlPoint.Coordinate;
            Val:= ConvertCoordinate(EditX.Text, P.X);
         end
         else Val:= 0;
         EditX.Text:=Truncate(Val,4); // update the field in case of input errors

         if saved then
         begin
            TFreeShip(FreeShip).Build:=False;
            TFreeShip(FreeShip).FileChanged:=True;
            TFreeShip(FreeShip).Redraw;
            ActiveControlPoint:=ActiveControlPoint;
         end;
      end;
   end;
end;{TFreeControlPointForm.Edit1Exit}

procedure TFreeControlPointForm.EditYKeyPress(Sender: TObject;var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-','@',#13]) or
      ((TFreeShip(FreeShip).ProjectSettings.ProjectUnits=fuImperial) and (Key='+')) or
      (Key=FormatSettings.DecimalSeparator) then else key:=#0; //SAP: added the '@'
   if Key=#13 then EditYExit(Self);
end;{TFreeControlPointForm.Edit2KeyPress}

procedure TFreeControlPointForm.EditYExit(Sender: TObject);
begin

end;

procedure TFreeControlPointForm.EditZChange(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      //P.Z:=P.Z+TFreeShip(FreeShip).Visibility.CursorIncrement;
      P.Z:=EditZ.Value;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;

procedure TFreeControlPointForm.EditYEditingDone(Sender: TObject);
var P    : T3DCoordinate;
    Val  : TFloatType;
    I    : Integer;
    saved: Boolean;
begin
   saved := false;
   if ActiveControlPoint<>nil then
   begin
// do only something, if the value has really been changed:
      P:=ActiveControlPoint.Coordinate;
      Val := ConvertCoordinate(EditY.Text, P.Y);
      if (abs(P.Y-Val)>1e-4) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
        I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            Val:=ConvertCoordinate(EditY.Text, P.Y);
            if abs(P.Y-Val)>1e-5 then
            begin
               if not saved then
               begin
                  TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(211),True);
                  saved := true;
               end;
               P.Y:=Val;
               TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate := P;
            end;
            Inc(I);
         end;

//  finally update the text field:
         if EditY.Text<>'' then
         begin
            P:=ActiveControlPoint.Coordinate;
            Val:= ConvertCoordinate(EditY.Text, P.Y);
         end
         else Val:= 0;
         EditY.Text:=Truncate(Val,4); // update the field in case of input errors

         if saved then
         begin
            TFreeShip(FreeShip).Build:=False;
            TFreeShip(FreeShip).FileChanged:=True;
            TFreeShip(FreeShip).Redraw;
            ActiveControlPoint:=ActiveControlPoint;
         end;
      end;
   end;
end;{TFreeControlPointForm.Edit2Exit}

procedure TFreeControlPointForm.EditZKeyPress(Sender: TObject; var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-','@',#13]) or
      ((TFreeShip(FreeShip).ProjectSettings.ProjectUnits=fuImperial) and (Key='+')) or
      (Key=FormatSettings.DecimalSeparator) then else key:=#0; //SAP: added the '@'
   if Key=#13 then EditZExit(Self);
end;{TFreeControlPointForm.Edit3KeyPress}

procedure TFreeControlPointForm.EditZExit(Sender: TObject);
begin

end;

procedure TFreeControlPointForm.EditZEditingDone(Sender: TObject);
var P    : T3DCoordinate;
    Val  : TFloatType;
    I    : Integer;
    saved: Boolean;
begin
   saved := false;
   if ActiveControlPoint<>nil then
   begin
// do only something, if the value has really been changed:
      P:=ActiveControlPoint.Coordinate;
      Val := ConvertCoordinate(EditZ.Text, P.Z);
      if (abs(P.Z-Val)>1e-4) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
        I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            Val:=ConvertCoordinate(EditZ.Text, P.Z);
            if abs(P.Z-Val)>1e-5 then
            begin
               if not saved then
               begin
                  TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(212),True);
                  saved := true;
               end;
               P.Z:=Val;
               TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate := P;
            end;
            Inc(I);
         end;

//  finally update the text field:
         if EditZ.Text<>'' then
         begin
            P:=ActiveControlPoint.Coordinate;
            Val:=ConvertCoordinate(EditZ.Text, P.Z);
         end
         else Val:= 0;
         EditZ.Text:=Truncate(Val,4); // update the field in case of input errors

         if saved then
         begin
            TFreeShip(FreeShip).Build:=False;
            TFreeShip(FreeShip).FileChanged:=True;
            TFreeShip(FreeShip).Redraw;
            ActiveControlPoint:=ActiveControlPoint;
         end;
      end;
   end;
end;{TFreeControlPointForm.Edit3Exit}

procedure TFreeControlPointForm.CheckBox1Change(Sender: TObject);
begin
end;

procedure TFreeControlPointForm.CheckBoxCornerClick(Sender: TObject);
begin
   if FSkipCheckbox1OnClick then exit;
   FSetActiveControlPointCorner(CheckBoxCorner.Checked);
end;

procedure TFreeControlPointForm.EditXChange(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      //P.X:=P.X+TFreeShip(FreeShip).Visibility.CursorIncrement;
      P.X:=EditX.Value;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;

procedure TFreeControlPointForm.FSetActiveControlPointCorner(isCorner: boolean);
var I,N     : Integer;
    OldType : TFreeVertexType;
    Undo    : TFreeUndoObject;
begin
  if (ActiveControlPoint<>nil)
    and (isCorner <> (ActiveControlPoint.VertexType=svCorner))
  then
   begin
      // Count the number of crease edges connected to this point
      OldType:=ActiveControlPoint.VertexType;
      Undo:=TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(213),false);
      if (ActiveControlPoint.Vertextype=svCorner) and (not isCorner)
      then
         begin
            N:=0;
            // Count the number of incident crease edges
            for I:=1 to ActiveControlPoint.NumberOfEdges do
                if FActiveControlPoint.Edge[I-1].Crease then inc(N);
            Case N of
               0 : ActiveControlPoint.Vertextype:=svRegular;
               1 : ActiveControlPoint.VertexType:=svDart;
               2 : ActiveControlPoint.VertexType:=svCrease;
               // points with more than two crease edges must always be a corner
            end;
         end;
      if (ActiveControlPoint.Vertextype<>svCorner) and (isCorner)
         then ActiveControlPoint.VertexType:=svCorner;

      if ActiveControlPoint.VertexType<>OldType then
      begin
         Undo.Accept;
         TFreeShip(FreeShip).Build:=False;
         TFreeShip(FreeShip).FileChanged:=True;
         TFreeShip(FreeShip).Redraw;
         ActiveControlPoint:=ActiveControlPoint;
      end else Undo.Delete;
   end;
end;{TFreeControlPointForm.CheckBox1MouseUp}

procedure TFreeControlPointForm.FormActivate(Sender: TObject);
begin
  FSkipCheckbox1OnClick:=false;
  EditX.Increment := TFreeShip(FreeShip).Visibility.CursorIncrement;

  EditY.Increment := TFreeShip(FreeShip).Visibility.CursorIncrement;
  EditZ.Increment := TFreeShip(FreeShip).Visibility.CursorIncrement;
end;

procedure TFreeControlPointForm.SpeedButton1Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.X:=P.X+TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton1Click}

procedure TFreeControlPointForm.SpeedButton4Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.X:=P.X-TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton4Click}

procedure TFreeControlPointForm.SpeedButton2Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.Y:=P.Y+TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton2Click}

procedure TFreeControlPointForm.SpeedButton5Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.Y:=P.Y-TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton5Click}

procedure TFreeControlPointForm.SpeedButton3Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.Z:=P.Z+TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton3Click}

procedure TFreeControlPointForm.SpeedButton6Click(Sender: TObject);
var P   : T3DCoordinate;
begin
   if ActiveControlPoint<>nil then
   begin
      TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(190),True);
      P:=ActiveControlPoint.Coordinate;
      P.Z:=P.Z-TFreeShip(FreeShip).Visibility.CursorIncrement;
      ActiveControlPoint.Coordinate:=P;
      TFreeShip(FreeShip).Build:=False;
      TFreeShip(FreeShip).FileChanged:=True;
      TFreeShip(FreeShip).Redraw;
      ActiveControlPoint:=ActiveControlPoint;
   end;
end;{TFreeControlPointForm.SpeedButton6Click}

end.
