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

unit FreeNewModelDlg;

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
  SysUtils,
     Classes,
     Graphics,
     Forms,
     Controls,
     StdCtrls,
     Buttons,
     ExtCtrls;

type TFreeNewModelDialog = class(TForm)
    Panel3: TPanel;
    BitBtn1: TSpeedButton;
    BitBtn2: TSpeedButton;
    Panel1: TPanel;
    Panel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    ComboBox1: TComboBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
                              private   { Private declarations }
                                 function FGetBreadth:Single;
                                 function FGetDraft:Single;
                                 function FGetLength:Single;
                                 function FGetNCols:Integer;
                                 function FGetNRows:Integer;
                                 procedure FSetBreadth(Val:single);
                                 procedure FSetDraft(Val:single);
                                 procedure FSetLength(Val:single);
                                 procedure FSetNCols(Val:Integer);
                                 procedure FSetNRows(Val:Integer);
                              public    { Public declarations }
                                 function Execute:Boolean;
                                 property Breadth  : Single read FGetBreadth write FSetBreadth;
                                 property Draft    : Single read FGetDraft write FSetDraft;
                                 property Length   : Single read FGetLength write FSetLength;
                                 property NCols    : Integer read FGetNCols write FSetNCols;
                                 property NRows    : Integer read FGetNRows write FSetNRows;
                           end;

var FreeNewModelDialog: TFreeNewModelDialog;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeNewModelDialog.FGetLength:Single;
begin
   Result:=StrToFloat(Edit3.Text);
end;{TFreeNewModelDialog.FGetLength}

procedure TFreeNewModelDialog.FSetLength(Val:single);
begin
   Edit3.Text:=FloatToStrF(Val,ffFixed,7,3);
end;{TFreeNewModelDialog.FSetLength}

function TFreeNewModelDialog.FGetBreadth:Single;
begin
   Result:=StrToFloat(Edit4.Text);
end;{TFreeNewModelDialog.FGetBreadth}

procedure TFreeNewModelDialog.FSetBreadth(Val:single);
begin
   Edit4.Text:=FloatToStrF(Val,ffFixed,7,3);
end;{TFreeNewModelDialog.FSetBreadth}

function TFreeNewModelDialog.FGetDraft:Single;
begin
   Result:=StrToFloat(Edit5.Text);
end;{TFreeNewModelDialog.FGetDraft}

procedure TFreeNewModelDialog.FSetDraft(Val:single);
begin
   Edit5.Text:=FloatToStrF(Val,ffFixed,7,3);
end;{TFreeNewModelDialog.FSetDraft}

function TFreeNewModelDialog.FGetNCols:Integer;
begin
   Result:=StrToInt(Edit1.Text);
end;{TFreeNewModelDialog.FGetNCols}

procedure TFreeNewModelDialog.FSetNCols(Val:Integer);
begin
   if Val<3 then Val:=3;
   Edit1.Text:=IntToStr(Val);
end;{TFreeNewModelDialog.FSetNCols}

function TFreeNewModelDialog.FGetNRows:Integer;
begin
   Result:=StrToInt(Edit2.Text);
end;{TFreeNewModelDialog.FGetNRows}

procedure TFreeNewModelDialog.FSetNRows(Val:Integer);
begin
   if Val<3 then Val:=3;
   Edit2.Text:=IntToStr(Val);
end;{TFreeNewModelDialog.FSetNRows}

function TFreeNewModelDialog.Execute:Boolean;
begin
   NCols:=NCols;
   NRows:=NRows;
   Length:=Length;
   Breadth:=Breadth;
   Draft:=Draft;
   Showmodal;
   Result:=ModalResult=mrOK;
end;{TFreeNewModelDialog.Execute}

procedure TFreeNewModelDialog.BitBtn1Click(Sender: TObject);
begin
   ModalResult:=mrOK;
end;{TFreeNewModelDialog.BitBtn1Click}

procedure TFreeNewModelDialog.BitBtn2Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeNewModelDialog.BitBtn2Click}

end.
