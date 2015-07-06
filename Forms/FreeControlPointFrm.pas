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
     StdCtrls, Buttons, ExtCtrls;

type

{ TFreeControlPointForm }

 TFreeControlPointForm  = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    Label6: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit1Exit(Sender: TObject);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure Edit2Exit(Sender: TObject);
    procedure Edit3KeyPress(Sender: TObject; var Key: Char);
    procedure Edit3Exit(Sender: TObject);
    procedure CheckBox1MouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
   private  { Private declarations }
      FActiveControlPoint  : TFreeSubdivisionControlPoint;
      FFreeShip            : TComponent;
      procedure FSetActiveControlPoint(Val:TFreeSubdivisionControlPoint);
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
      Edit1.Text:='';
      Edit2.Text:='';
      Edit3.Text:='';
      Checkbox1.Checked:=false;
   end else
   begin
      R:=0.0;
      Edit4.Text := '0.0';
      Npoi:=TFreeShip(FreeShip).NumberOfSelectedControlPoints-1;
      if  Npoi>= 1 then begin
          Label5.Caption:=Userstring(1477);
          R:=SQRT(Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.X)+
          Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Y)+
          Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Z));
          Edit4.Text := Truncate(R,4);
          Edit5.Text :='0.0'; 		  
          end    
      else begin
          Label5.Caption:=Userstring(1477);
          Edit4.Text :='0.0'; 
      end;

      if  Npoi>= 4 then begin
          Label5.Caption:=Userstring(1477);
          R:=SQRT(Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.X)+
          Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Y)+
          Sqr(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Z));
          Edit4.Text := Truncate(R,4);
          len:=0;		  
          for i:=1 to Npoi do begin
           R:=SQRT(Sqr(TFreeShip(FreeShip).SelectedControlPoint[i-1].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[i].Coordinate.X)+
           Sqr(TFreeShip(FreeShip).SelectedControlPoint[i-1].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[i].Coordinate.Y)+
           Sqr(TFreeShip(FreeShip).SelectedControlPoint[i-1].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[i].Coordinate.Z));		  
		   len:=len+R;
          end;
          Label5.Caption:=Userstring(1493);
          Edit5.Text := Truncate(len,4);		  
      end;

	  
      if R=0 then R:=0.0001;
      if  Npoi=1 then begin
        Label5.Caption:=Userstring(1477);
        alfa:=abs(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.X)/R;
        beta:=abs(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Y)/R;
        gamma:=abs(TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[Npoi].Coordinate.Z)/R;
        Edit5.Text := Truncate(ArcCos(alfa)*57.29578,2)+';'+Truncate(ArcCos(beta)*57.29578,2)+';'+Truncate(ArcCos(gamma)*57.29578,2);
{
        alfa:=(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.X-TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.X)/R;
        beta:=(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Y-TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Y)/R;
        gamma:=(TFreeShip(FreeShip).SelectedControlPoint[1].Coordinate.Z-TFreeShip(FreeShip).SelectedControlPoint[0].Coordinate.Z)/R;		
        Edit5.Text := Truncate(alfa,2)+';'+Truncate(beta,2)+';'+Truncate(gamma,2);		
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
        Edit5.Text := Truncate(ArcCos(alfa)*57.29578,2)+';'+Truncate(ArcCos(beta)*57.29578,2)+';'+Truncate(ArcCos(gamma)*57.29578,2);
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
        Edit5.Text := Truncate(ArcCos(acos)*57.29578,2);		
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
        Edit5.Text := Truncate(ArcCos(acos)*57.29578,2);		
      end;
	  
      Edit1.Text:=Truncate(FActiveControlPoint.Coordinate.X,4);
      Edit2.Text:=Truncate(FActiveControlPoint.Coordinate.Y,4);
      Edit3.Text:=Truncate(FActiveControlPoint.Coordinate.Z,4);
      Checkbox1.Checked:=FActiveControlPoint.VertexType=svCorner;
      // Count the number of crease edges connected to this point
      N:=0;
      for I:=1 to FActiveControlPoint.NumberOfEdges do if FActiveControlPoint.Edge[I-1].Crease then inc(N);
      Checkbox1.Enabled:=(N>0) and (N<3); // points with more than two crease edges must always be a corner
      Edit1.Enabled:=not Val.Locked;
      Edit2.Enabled:=not Val.Locked;
      Edit3.Enabled:=not Val.Locked;
      SpeedButton1.Enabled:=not val.locked;
      SpeedButton2.Enabled:=not val.locked;
      SpeedButton3.Enabled:=not val.locked;
      SpeedButton4.Enabled:=not val.locked;
      SpeedButton5.Enabled:=not val.locked;
      SpeedButton6.Enabled:=not val.locked;
      Checkbox1.Enabled:=not Val.Locked;
      if Val.Locked then
      begin
         BCol:=clBtnFace;
         FCol:=clDkgray;
      end else
      begin
         BCol:=clWindow;
         FCol:=clBlack;
      end;
      if Edit1.Color<>BCol then Edit1.Color:=BCol;
      if Edit1.Font.Color<>FCol then Edit1.Font.Color:=FCol;
      if Edit2.Color<>BCol then Edit2.Color:=BCol;
      if Edit2.Font.Color<>FCol then Edit2.Font.Color:=FCol;
      if Edit3.Color<>BCol then Edit3.Color:=BCol;
      if Edit3.Font.Color<>FCol then Edit3.Font.Color:=FCol;
   end;
end;{TFreeControlPointForm.FSetActiveControlPoint}

procedure TFreeControlPointForm.Edit1KeyPress(Sender: TObject;var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-','@',#13]) or
      ((TFreeShip(FreeShip).ProjectSettings.ProjectUnits=fuImperial) and (Key='+')) or
      (Key=FormatSettings.DecimalSeparator) then else key:=#0; //SAP: added the '@'
   if Key=#13 then Edit1Exit(Self);
end;{TFreeControlPointForm.Edit1KeyPress}

procedure TFreeControlPointForm.Edit1Exit(Sender: TObject);
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
      Val := ConvertCoordinate(Edit1.Text, P.X);
      if (abs(P.X-Val)>1e-4) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
         I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            Val:=ConvertCoordinate(Edit1.Text, P.X);
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
         if Edit1.Text<>'' then
         begin
            P:=ActiveControlPoint.Coordinate;
            Val:= ConvertCoordinate(Edit1.Text, P.X);
         end
         else Val:= 0;
         Edit1.Text:=Truncate(Val,4); // update the field in case of input errors

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

procedure TFreeControlPointForm.Edit2KeyPress(Sender: TObject;var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-','@',#13]) or
      ((TFreeShip(FreeShip).ProjectSettings.ProjectUnits=fuImperial) and (Key='+')) or
      (Key=FormatSettings.DecimalSeparator) then else key:=#0; //SAP: added the '@'
   if Key=#13 then Edit2Exit(Self);
end;{TFreeControlPointForm.Edit2KeyPress}

procedure TFreeControlPointForm.Edit2Exit(Sender: TObject);
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
      Val := ConvertCoordinate(Edit2.Text, P.Y);
      if (abs(P.Y-Val)>1e-4) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
        I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            Val:=ConvertCoordinate(Edit2.Text, P.Y);
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
         if Edit2.Text<>'' then
         begin
            P:=ActiveControlPoint.Coordinate;
            Val:= ConvertCoordinate(Edit2.Text, P.Y);
         end
         else Val:= 0;
         Edit2.Text:=Truncate(Val,4); // update the field in case of input errors

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

procedure TFreeControlPointForm.Edit3KeyPress(Sender: TObject; var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-','@',#13]) or
      ((TFreeShip(FreeShip).ProjectSettings.ProjectUnits=fuImperial) and (Key='+')) or
      (Key=FormatSettings.DecimalSeparator) then else key:=#0; //SAP: added the '@'
   if Key=#13 then Edit3Exit(Self);
end;{TFreeControlPointForm.Edit3KeyPress}

procedure TFreeControlPointForm.Edit3Exit(Sender: TObject);
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
      Val := ConvertCoordinate(Edit3.Text, P.Z);
      if (abs(P.Z-Val)>1e-4) or (TFreeShip(FreeShip).NumberOfSelectedControlPoints>1) then
      begin
// SAP change all selected points
        I := 1;
         while I <= TFreeShip(FreeShip).NumberOfSelectedControlPoints do
         begin
            P := TFreeShip(FreeShip).SelectedControlPoint[I-1].Coordinate;
            Val:=ConvertCoordinate(Edit3.Text, P.Z);
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
         if Edit3.Text<>'' then
         begin
            P:=ActiveControlPoint.Coordinate;
            Val:=ConvertCoordinate(Edit3.Text, P.Z);
         end
         else Val:= 0;
         Edit3.Text:=Truncate(Val,4); // update the field in case of input errors

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

procedure TFreeControlPointForm.CheckBox1MouseUp(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var I,N     : Integer;
    OldType : TFreeVertexType;
    Undo    : TFreeUndoObject;
begin
   if ActiveControlPoint<>nil then
   begin
      // Count the number of crease edges connected to this point
      OldType:=ActiveControlPoint.VertexType;
      Undo:=TFreeShip(FreeShip).Edit.CreateUndoObject(Userstring(213),false);
      Case ActiveControlPoint.Vertextype of
         svCorner  : begin
                        N:=0;
                        for I:=1 to ActiveControlPoint.NumberOfEdges do if FActiveControlPoint.Edge[I-1].Crease then inc(N);
                        // Count the number of incident crease edges
                        Case N of
                           0 : ActiveControlPoint.Vertextype:=svRegular;
                           1 : ActiveControlPoint.VertexType:=svDart;
                           2 : ActiveControlPoint.VertexType:=svCrease;
                        end;
                     end;
         else ActiveControlPoint.VertexType:=svCorner;
      end;
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
