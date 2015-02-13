unit FreeNumInput;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
     {$ifdef Windows}
     Windows,
     WinTypes,
     WinProcs,
     {$else}
     LCLIntf, LCLType, LMessages, LResources,
     {$endif}
     SysUtils,
     Messages,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
     Menus;

type TDataType     = (dtInteger,dtFloat);
     TAlignment    = (taLeftJustify, taRightJustify);
     TFreeNumInput = class (TCustomEdit)
                        private
                           FAlignment        : TAlignment;
                           FCanvas           : TControlCanvas;
                           FDecimals         : word;
                           FDigits           : word;
                           FFocused          : Boolean;
                           FMax              : extended;
                           FMin              : extended;
                           OldColor          : TColor;
                           OldTNIColor       : TColor;
                           FDataType         : TDataType;
                           FTabOnEnterKey    : Boolean;
                           FIsEditing        : Boolean;
                           FTextMargin       : Integer;
                           FValue            : extended;
                           FInitialValue     : extended;
                           FValidate         : boolean;
                           FOutOfRangeMessage: boolean;
                           FOnBeforeSetValue : TNotifyEvent;
                           FOnAfterSetValue  : TNotifyEvent;
                           procedure CMExit(var Message: TCMExit); message CM_EXIT;
                           procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
                           procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
                           procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
                           procedure CalcTextMargin;
                           procedure SetAlignment(Value : TAlignment);
                           procedure SetDecimals(Value : word);
                           procedure SetDigits(Value : word);
                           procedure SetMax(Value : extended);
                           procedure SetMin(Value : extended);
                           procedure SetDataType(Value : TDataType);
                           procedure SetTabOnEnterKey(Value: Boolean);
                           procedure SetValue(Value : extended);
                           procedure SetValidate(Value : boolean);
                           function  FGetValue:extended;
                        protected
                           procedure FormatText; dynamic;
                           procedure CheckRange; dynamic;
                           procedure KeyPress(var Key: Char); override;
                           procedure KeyDown(var Key: Word; Shift: TShiftState); override;
                        public
                           IsValid : Boolean;
                           constructor Create(AOwner: TComponent); override;
                           destructor destroy;override;
                           function AsInteger : integer; dynamic;
                           function Valid (var Value : extended ) : boolean; dynamic;

                        published
                           property Alignment         : TAlignment Read FAlignment write SetAlignment;
                           property Decimals          : word read FDecimals write SetDecimals;
                           property Digits            : word read FDigits write SetDigits;
                           property Max               : extended read FMax write SetMax;
                           property Min               : extended read FMin write SetMin;
                           property DataType          : TDataType read FDataType write SetDataType default dtFloat;
                           property OutOfRangeMessage : boolean read FOutOfRangeMessage write FOutOfRangeMessage;
                           property TabOnEnterKey     : Boolean read FTabOnEnterKey write SetTabOnEnterKey ;
                           property Value             : extended read FGetValue write SetValue;
                           property Validate          : boolean read FValidate write SetValidate;
                           property OnBeforeSetValue  : TNotifyEvent read FOnBeforeSetValue write FOnBeforeSetValue;
                           property OnAfterSetValue   : TNotifyEvent read FOnAfterSetValue write FOnAfterSetValue;

                           property AutoSelect;
                           property AutoSize;
                           property BorderStyle;
                           property Color;
                           //property Ctl3D;
                           property Enabled;
                           property Font;
                           property HideSelection;
                           property ParentColor;
                           //property ParentCtl3D;
                           property ParentFont;
                           property ParentShowHint;
                           property PopupMenu;
                           property ReadOnly;
                           property ShowHint;
                           property TabOrder;
                           property Visible;
                           property OnClick;
                           property OnEnter;
                           property OnExit;
                           property OnKeyDown;
                           property OnKeyPress;
                           property OnKeyUp;
                           property OnMouseDown;
                           property OnMouseMove;
                           property OnMouseUp;


                     end;

procedure Register;

implementation

uses FreeLanguageSupport;

type TSetOfChar=set of char;

const MaxLongint : longint =2147483647;
      MinLongint : longint =-2147483647;

constructor TFreeNumInput.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FCanvas:=TControlCanvas.Create;
   FCanvas.Control:=Self;
   Width:=85;
   FDataType:=dtFloat;
   FDigits:=12;
   FDecimals:=2;
   FMax:=0.0;
   FMin:=0.0;
   AutoSelect:=true;
   FValidate:=False;
   IsValid:=true;
   FValue:=0.0;
   MaxLength:=FDigits;
   CalcTextMargin;
   Text:='0.0';
   OldColor:=Font.Color;
   OldTNIColor:=Color;
   FTabOnEnterKey:=false;
   FOutOfRangeMessage:=False;
   FIsEditing:=false;
   FormatText;
end;{TFreeNumInput.Create}

destructor TFreeNumInput.Destroy;
begin
   FCanvas.Destroy;
   inherited Destroy;
end;{TFreeNumInput.Destroy}

procedure TFreeNumInput.SetAlignment(Value : TAlignment);
begin
   if FAlignment<>Value then
   begin
      FAlignment:=Value;
      invalidate;
   end;
end;{TFreeNumInput.SetAlignment}

function TFreeNumInput.AsInteger: integer;
begin
   Result:=0;
   if (Value<=MaxLongInt) and (Value >= MinLongInt) then  Result:=round(Value)
end;{TFreeNumInput.AsInteger}

procedure TFreeNumInput.SetMin(Value: extended);
begin
   if FMin<>Value then
   begin
      FMin:=Value;
      CheckRange;
      if FMin>FMax then FMin:=FMax;
      if FValue<=FMin then FValue:=FMin;
      FormatText;
   end;
end;{TFreeNumInput.SetMin}

procedure TFreeNumInput.SetMax(Value: extended);
begin
   if FMax<>Value then
   begin
      FMax:=Value;
      CheckRange;
      if FMax<=FMin then FMax:=FMin;
      if FValue>FMax then FValue:=FMax;
      FormatText;
   end;
end;{TFreeNumInput.SetMax}

procedure TFreeNumInput.SetValue(Value: extended);
begin
   if Valid(Value) then
   begin
      // removed events to avoid circular loops!
      //if Assigned(FOnBeforeSetValue) then FOnBeforeSetValue(self);
      FValue:=Value;
      if not FIsEditing then
         FInitialValue:=FValue;
      FormatText;
      //if Assigned(FOnAfterSetValue) then FOnAfterSetValue(self);
   end else
   begin
      //FValue:=Value;
      //FormatText;
   end;
end;{TFreeNumInput.SetValue}

procedure TFreeNumInput.SetDigits(Value: word);
begin
   if FDigits<>Value then
   begin
      FDigits:=Value;
      MaxLength:=FDigits;
      FormatText;
   end;
end;{TFreeNumInput.SetDigits}

procedure TFreeNumInput.SetDecimals(Value: word);
begin
   if FDecimals<>Value then
   begin
      if FDataType=dtInteger then FDecimals:=0
                             else FDecimals:=Value;
      FormatText;
   end;
end;{TFreeNumInput.SetDecimals}

procedure TFreeNumInput.SetDataType(Value: TDataType);
begin
   if FDataType<>Value then
   begin
      FDataType:=Value;
      if FDataType=dtInteger then FDecimals:=0;
      CheckRange;
      FormatText;
  end;
end;{TFreeNumInput.SetDataType}

procedure TFreeNumInput.SetValidate(Value: boolean);
begin
   if FValidate<>Value then
   begin
      FValidate:=Value;
      if FValidate and ((FValue<=FMin) or (FValue>FMax)) then
      begin
         FValue:=FMin;
         FormatText;
      end;
   end;
end;{TFreeNumInput.SetValidate}

function TFreeNumInput.FGetValue:extended;
var Val : extended;
begin
   if Text<>'' then
   begin
      try
        Val:=StrToFloat(Text);
      except
        Val:=0;
      end;
   end else Val:=0;
   if val<>FValue then
   begin
      Value:=val;
   end;
   Result:=FValue;
end;{TFreeNumInput.FGetValue}

procedure TFreeNumInput.SetTabOnEnterKey(Value: Boolean);
begin
   if FTabOnEnterKey<>Value then
   begin
      FTabOnEnterKey:=Value;
   end;
end;{TFreeNumInput.SetTabOnEnterKey}

function TFreeNumInput.Valid(var Value: extended): boolean;
var Tmp:string;
begin
   Result:=true;

   if FValidate and ((Value<FMin) or (Value>FMax)) then
   begin
      if FOutOfRangeMessage then MessageDlg(Userstring(205)+#32+FloatToStrF(FMin,ffFixed,7,Decimals)+#32+Userstring(206)+#32+FloatToStrF(FMax,ffFixed,7,Decimals),mtError,[mbOk],0);
      Tmp:=FloatToStrF(Value,ffFixed,FDigits,FDecimals);
      Value:=StrToFloat(Tmp);

      if Value<FMin then Value:=FMin else
         if Value>FMax then Value:=FMax;
      if FValue<>Value then
      begin
         if Assigned(FOnBeforeSetValue) then FOnBeforeSetValue(self);

         FValue:=Value;
         FormatText;
         
         if Assigned(FOnAfterSetValue) then FOnAfterSetValue(self);
      end;
      Result:=false;
   end;
end;{TFreeNumInput.Valid}

procedure TFreeNumInput.KeyDown(var Key: Word; Shift: TShiftState);
var Ch : Char;
begin
   FIsEditing:=true;
   Ch:=Chr(Key);
   if (key=VK_UP) then //PostMessage(GetparentForm(Self).Handle, WM_NEXTDLGCTL ,1,0);
      Value := Value + 1;
   if (Key=VK_DOWN) then //PostMessage(GetparentForm(Self).Handle, WM_NEXTDLGCTL ,0,0);
      Value := Value - 1;
   if (Key=VK_DELETE) then Keypress(Ch);
   inherited KeyDown(Key, Shift);
   FIsEditing:=false;
end;{TFreeNumInput.KeyDown}

function MessageBeep(uType: UINT): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.MessageBeep(uType);
{$ELSE}
  Beep; //Most calls pass 0 as uType (MB_OK), which is system default sound}
  Result := true;
{$ENDIF}
end;

procedure TFreeNumInput.KeyPress(var Key: Char);
var I,X,Sel,OldLength:Integer;
    TRashText,OldText:String;
    Tmp:extended;
begin
   FIsEditing:=true;
   x:=0;
 try
   if (key=#13) and (FTabOnEnterKey) then
   begin
      if Valid(FValue) then
      begin
         if Assigned(FOnBeforeSetValue) then FOnBeforeSetValue(self);
         //PostMessage(GetparentForm(Self).Handle, WM_NEXTDLGCTL ,0,0);
         inherited KeyPress(Key);
         if Assigned(FOnAfterSetValue) then FOnAfterSetValue(self);
         Key:=#0;
         Exit;
      end else exit;
   end else if Key=#13 then
   begin
      Tmp:=StrToFloat(Text);
      if Valid(Tmp) then
      begin
         FValue:=Tmp;
         FInitialValue:=FValue;
         FormatText;
         if Assigned(FOnAfterSetValue) then FOnAfterSetValue(self);
         Exit;
      end else exit;
   end;

   // Restore last entry if ESC
   if (key=#27) then
   begin
      //SendMessage(Self.Handle, WM_UNDO,0,0);
      Value := FInitialValue;
      SelectAll;
      Key:=#0;
      exit;
   end;

   {if copy or cut selection}
   if (Key=^C) or (Key=^X) then exit;

   {if Paste check if valid else undo}
   if (Key=^V) then
   begin
      try
         OldText:=Text;
         SendMessage(Self.Handle, WM_PASTE ,0,0);
         FValue:=StrToFloat(Text);
         Valid(FValue);
         FormatText;
         if Length(Text)>MaxLength then {Check Length}
         begin
            Text:=OldText;
            MessageBeep(0);
         end;
      except
         //SendMessage(Self.Handle, WM_UNDO ,0,0);
         Text := OldText;
         MessageBeep(0);
      end;
      Key:=#0;
      exit;
   end;

   {Check for valid Characters}
   if Key in ['0'..'9', '-',FormatSettings.DecimalSeparator, #8,#13] then
   begin
      if Key in [FormatSettings.DecimalSeparator] then if FDataType=dtFloat then
                                                             else Key:=#0;
      inherited KeyPress(Key);
      if Key in [#13] then
      begin
         if Assigned(FOnBeforeSetValue) then FOnBeforeSetValue(self);
         FValue:=StrToFloat(Text);
         if Assigned(FOnAfterSetValue) then FOnAfterSetValue(self);
         exit;
      end;
   end else  Key:=#0;

   {Clear selection in control If selection include DecimalPoint clear
   all numbers from start selection}
   if Sellength>0 then
   begin
      if (SelStart+1<=pos(FormatSettings.DecimalSeparator,Text)) and (SelStart+SelLength >= pos(FormatSettings.DecimalSeparator,Text)) then
      begin
         SelLength:=Length(Text)- SelStart;
         //SendMessage(Self.Handle, WM_CLEAR, 0, 0);
         ClearSelection;
      end else //SendMessage(Self.Handle, WM_CLEAR, 0, 0);
         ClearSelection;
   end;

   {Take away all non numeric characters and leave the cursor}
   Sel:=SelStart;
   TrashText:='';
   OldLength:=Length(Text);
   for i:=1 to length(Text) do if ( Text[i] in ['0'..'9',FormatSettings.DecimalSeparator,'-']) then TrashText:=TrashText+Text[i] else if i>sel then inc(x);
   Text:=TrashText;
   SelStart:=Sel-(OldLength-Length(Text))+x;

   {Check for Back Space on Decimal, if decimal exist restrict removal if it will
   exceed characters before Decimal}
   if (key in [#8,#13]) and (pos(FormatSettings.DecimalSeparator,Text)>0) and (SelStart=pos(FormatSettings.DecimalSeparator,Text)) and (Length(Text)> pos(FormatSettings.DecimalSeparator,Text))
      and (Length(Text)> MaxLength-FDecimals) then
   begin
      MessageBeep(0);
      Key:=#0;
      Exit;
   end;

   {Check for decimal is allowed and if decimal exist}
   if (key=FormatSettings.DecimalSeparator) and (Fdecimals=0) or (pos(FormatSettings.DecimalSeparator,Text) >0)
      and (Key=FormatSettings.DecimalSeparator) or (SelStart<Length(Text)-FDecimals) and (Key=FormatSettings.DecimalSeparator)
      and (Length(Text)>FDecimals)then
   begin
      MessageBeep(0);
      Key:=#0;
      Exit;
   end;

   {Check for negative sign, only allowed as first character}
   if (key='-') and (SelStart<>0)then
   begin
      MessageBeep(0);
      Key:=#0;
      Exit;
   end;

   {Check max characters before decimal }
   if  (not (Key in [#8,#13])) and (pos(FormatSettings.DecimalSeparator,Text) >0) and (pos(FormatSettings.DecimalSeparator,Text) >= MaxLength-FDecimals)
        and (selstart<=pos(FormatSettings.DecimalSeparator,Text)) or (Key in ['0'..'9'])and (FDecimals <>0) and (selstart<=MaxLength-FDecimals)
        and (Length(Text)>=MaxLength-FDecimals-1) and (pos(FormatSettings.DecimalSeparator,Text)=0) then
   begin
      {Add Decimal automatically if max characters before Decimal}
      if (Key in ['0'..'9']) and (FDecimals <>0) and (selstart=MaxLength-FDecimals-1)
          and (pos(FormatSettings.DecimalSeparator,Text)=0) then
      begin
         key:=FormatSettings.DecimalSeparator;
         exit
      end;
      MessageBeep(0);
      Key:=#0;
      Exit;
   end;

   {Check max characters after decimal}
   if  (not (Key in [#8,#13])) and (pos(FormatSettings.DecimalSeparator,Text) >0)
        and (Length(Text)-pos(FormatSettings.DecimalSeparator,Text)>=FDecimals) and (selstart >= pos(FormatSettings.DecimalSeparator,Text)) then
   begin
      MessageBeep(0);
      Key:=#0;
      Exit;
   end;
 finally
      FIsEditing:=false;
 end;
end;{TFreeNumInput.KeyDown}

procedure TFreeNumInput.CMEnter(var Message: TCMEnter);
begin
{make sure the cursor is at the beginning if AutoSelect=False}
 if   (AutoSelect=False) then
      SelStart:=0;
 FFocused:=True; {info for WM_PAINT }
 FInitialValue:=FValue;
 inherited;
end;

procedure TFreeNumInput.CMExit(var Message: TCMExit);
var
  X: extended;
  TrashText : String;
  i:integer;
begin
  try
    {Using this to remove Currency symbol and Thousand seperator}
    TrashText:='';
    for i:=1 to length(Text) do
    if ( Text[i] in ['0'..'9',FormatSettings.DecimalSeparator,'-']) then TrashText:=TrashText+Text[i];
    Text:=TrashText;
    if Text='' then Text:='0';
    X:=StrToFloat(Text);
    if Valid(X) then
    begin
      IsValid:=true;
      if FValue<>X then
      begin
         if Assigned(FOnBeforeSetValue) then FOnBeforeSetValue(self);
         FValue:=X;
         FInitialValue:=FValue;
         if Assigned(FOnAfterSetValue) then FOnAfterSetValue(self);
      end;
      FormatText;
      FFocused:=False;  {info for WM_PAINT }
      invalidate; { This will repaint the control WM_PAINT Message}
      inherited ;
    end else
    begin
      IsValid:=false;
      SelectAll;
      SetFocus;
    end;
  except
    on E: EConvertError do
    begin
      MessageDlg('''' + Text + ''''+Userstring(207)+'.', mtError, [mbOK], 0);
      SelectAll;
      SetFocus;
    end;
  end;
end;

procedure TFreeNumInput.CheckRange;
var LMax, LMin: Extended;

  procedure check;
  begin
     if ((FMin<=LMin) or (FMin>LMax)) then FMin:=LMin;
     if ((FMax>LMax) or (FMax<=LMin)) then FMax:=LMax;
     if ((FValue<=LMin) or (FValue>LMax)) then FValue:=0;
   end;

begin
  if FDataType=dtInteger then
  begin
   LMax:=MaxLongInt; LMin:=MinLongInt;
   check;
  end;
end;{TFreeNumInput.CheckRange}

procedure TFreeNumInput.FormatText;
var X: Extended;
begin
  MaxLength:=FDigits;
  if FDataType in [dtFloat] then
  begin
    X:=FValue;
    Text:=FloatToStrF ( X, ffFixed, FDigits, FDecimals);
  end
  else
  begin
    FValue:=Round(FValue);
    X:=FValue;
    Text:=IntToStr(Round(X));
  end;
  Font.Color:=OldColor;
end;{TFreeNumInput.FormatText}

procedure TFreeNumInput.CMFontChanged(var Message: TMessage);
begin
  inherited;
  CalcTextMargin;
end;

procedure TFreeNumInput.CalcTextMargin;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
  LCLSysMetrics, LCLMetrics: TLCLTextMetric;
begin
  DC:=GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont:=SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I:=SysMetrics.tmHeight;
  if I>Metrics.tmHeight then I:=Metrics.tmHeight;
  FTextMargin:=I div 4;
end;

procedure TFreeNumInput.WMPaint(var Message: TWMPaint);
var
  Width_, Indent_, Left_: Integer;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
begin
{ BugFix? : }
{ CMEnter doesn't execute, when the control is focused by a keystroke }
  if Self.Focused then FFocused:=true;

  if (FAlignment=taLeftJustify) or FFocused then
  begin
    inherited;
    Exit;
  end;
{ Since edit controls do not handle justification unless multi-line (and
  then only poorly) we will draw right and center justify manually unless
  the edit has the focus. }


  DC:=Message.DC;
  if DC=0 then DC:=BeginPaint(Handle, PS);
  FCanvas.Handle:=DC;
  try
    FCanvas.Font:=Font;
    with FCanvas do
    begin
      R:=ClientRect;
      if Enabled then Brush.Color:=Color
                 else Brush.Color:=clbtnface;
      if Pen.Color<>Brush.color then Pen.Color:=Brush.Color;
      S:=Text;
      Width_:=TextWidth(S);
      if BorderStyle=bsNone then Indent_:=0
                            else Indent_:=FTextMargin;
      if FAlignment=taRightJustify then
        Left_:=R.Right - Width_ - Indent_ else
        Left_:=(R.Left + R.Right - Width_) div 2;
      TextRect(R, Left_, Indent_, S);
    end;
  finally
    FCanvas.Handle:=0;
    if Message.DC=0 then EndPaint(Handle, PS);
  end;
end;

procedure Register;
begin
  RegisterComponents ('Freeship', [TFreeNumInput]);
end;

end.