unit MethodList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLTranslator;

// simple not sorted generic list
// TItemType can be procedure of object type, double pointer, like TNotifyEvent
type generic TMethodList<TItemType> = class
private
  FCount: integer;
  FCapacity: integer;
  FList: array of TItemType;
  function FGet(Index: integer): TItemType;
  function FGetMemory: integer;
  procedure FGrow;
  procedure FSet(Index: integer; Item: TItemType);
  procedure FSetCapacity(NewCapacity: integer);
public
  procedure Add(Item: TItemType);
  constructor Create;
  procedure Clear; virtual;
  destructor Destroy; override;
  procedure Delete(Index: integer);
  procedure DeleteItem(Item: TItemType);  //deletes all instances of the item
  //deletes all instances of the aList from Self
  function IndexOf(Item: TItemType): integer;  // normal TList function
  procedure Insert(Index: integer; Item: TItemType);
  property Capacity: integer
    read FCapacity write FSetCapacity;
  property Count: integer read FCount;
  property Items[Index: integer]: TItemType
    read FGet write FSet; default;
  property Memory: integer read FGetMemory;
end;

implementation

destructor TMethodList.Destroy;
begin
  Clear;
  inherited Destroy;
end;{TMethodList.Destroy}

procedure TMethodList.Add(Item: TItemType);
var
  //Cur, Prev: PtrUInt;
  i:integer;
begin
  if FCount = FCapacity then
    FGrow;

  FList[FCount] := Item;

  Inc(FCount);
end;{TMethodList.Add}

constructor TMethodList.Create;
begin
  inherited Create;
  FList := nil;
  FCount := 0;
  FCapacity := 0;
end;{TMethodList.Create}

procedure TMethodList.Clear;
begin
  FSetCapacity(0);
end;{TMethodList.Clear}

procedure TMethodList.Delete(Index: integer);
begin
  if (Index<0) or (Index >= FCount) then
     raise Exception.Create('Index out of bounds');
  Dec(FCount);
  if Index < FCount then
  begin
    Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TItemType));
  end;
end;{TMethodList.Delete}

procedure TMethodList.DeleteItem(Item: TItemType);
var i: integer;
begin
  //logger.IncreaseIndent;
  //logger.Debug(format('TMethodList.DeleteItem[%d]',[(<TItemType>(Item)).FId]));
  i := IndexOf(Item);
  while i >= 0 do
    begin
    Delete(i);
    i := IndexOf(Item);
    end;
  //logger.DecreaseIndent;
end;{TMethodList.DeleteAll}

function TMethodList.FGet(Index: integer): TItemType;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList[Index]
  else
    Result := nil;
end;{TMethodList.FGet}

function TMethodList.FGetMemory: integer;
begin
  Result := SizeOf(Pointer) +           // self        : TItemType
    SizeOf(integer) +           // FCapacity   : integer
    SizeOf(integer) +           // fcount      : integer
    SizeOf(TItemType) * FCapacity;
end;{TMethodList.FGetMemory}

procedure TMethodList.FGrow;
var
  Delta: integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div SizeOf(TItemType);
    if Delta > 1024 then
      Delta := 1024;
  end
  else
  if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  FSetCapacity(FCapacity + Delta);
end;{TMethodList.FGrow}

function TMethodList.IndexOf(Item: TItemType): integer;
var
  I: integer;
begin
  Result := -1;
  for I := 0 to FCount-1 do
    begin
    if FList[I] = Item then
      begin
      Result := I;
      break;
      end;
    end;
end;{TMethodList.IndexOf}

procedure TMethodList.Insert(Index: integer; Item: TItemType);
begin
  if (index<0) or (Index >= FCount) then
    raise Exception.Create('Index out of bounds');
  if FCount = FCapacity then
    FGrow;
  if Index < FCount then
  begin
    Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(TItemType));
  end;
  FList[Index] := Item;
  Inc(FCount);
end;{TMethodList.Insert}

procedure TMethodList.FSet(Index: integer; Item: TItemType);
begin
  if (Index<0) or (Index>=FCount) then
    raise Exception.Create('Index out of bounds');
  FList[Index] := Item;
end;{TMethodList.FSet}


procedure TMethodList.FSetCapacity(NewCapacity: integer);
begin
  if FCapacity = NewCapacity then exit;
  Setlength(FList, NewCapacity);
  FCapacity := NewCapacity;
  if FCapacity < FCount then
    FCount := Fcapacity;
end;{TMethodList.FSetCapacity}

end.

