{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }

{    Copyright © 2005, by Martijn van Engeland                                                }
{    e-mail                  : marven@users.sourceforge.net                                   }
{    FREE!ship project page  : https://sourceforge.net/projects/freeship                      }
{    FREE!ship homepage      : http://homepages.ipact.nl/~martijn                             }

{    This program is free software; you can redistribute it and/or modify it under            }
{    the terms of the GNU General Public License as published by the                          }
{    Free Software Foundation; either version 2 of the License, or (at your option)           }
{    any later version.                                                                       }

{    This program is distributed in the hope that it will be useful, but WITHOUT ANY          }
{    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          }
{    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 }

{    You should have received a copy of the GNU General Public License along with             }
{    this program; if not, write to the Free Software Foundation, Inc.,                       }
{    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    }

{    If the program is interactive, make it output a short notice like this when              }
{    it starts in an interactive mode:                                                        }

{    Gnomovision version 69, Copyright (C) year name of author Gnomovision comes              }
{    with ABSOLUTELY NO WARRANTY; for details type `show w'. This is free software,           }
{    and you are welcome to redistribute it under certain conditions; type `show c'           }
{    for details.                                                                             }
{#############################################################################################}

{$MODE ObjFPC}{$H+}

unit FasterList;

interface

uses
  //Math,
  SysUtils;

type
  {---------------------------------------------------------------------------------------------------}
  {                                       TFasterList                                                 }

  {   TFasterlist is an improved version of Delphi's standard TList object.                           }
  {   It is stripped of unneccesary code to improve speed.                                            }
  {   Additionally the contents (pointers) can be sorted so that the time needed to search an item    }
  {   has been reduced with a factor 2^n                                                               }

  {   Use the SORT            method to sort the list                                                 }
  {   Use the SORTEDINDEXOF() function to perform a binary search in the sorted list                  }
  {---------------------------------------------------------------------------------------------------}
  TAoPtrUInt = array of PtrUInt;

  generic TFasterList<TItemType> = class
  private
    FCount: integer;
    FCapacity: integer;
    FSorted: boolean;
    FUnique: boolean; // MM 2019-05-03 : added to exclude duplications for some types of items
    FUseUserData: boolean;
    FList: array of TItemType;
    FData: array of Pointer;
    // TItemTypes to any user specified object
    function FGet(Index: integer): TItemType;
    function FGetObject(Index: integer): Pointer;
    function FGetMemory: integer;
    procedure FGrow;
    procedure FSet(Index: integer; Item: TItemType);
    procedure FSetObject(Index: integer; UserObject: Pointer);
    procedure FSetCapacity(NewCapacity: integer);
    function getPtrUintLst:TAoPtrUInt;
  public
    procedure Add(Item: TItemType);
    procedure AddList(List: TFasterList);
    procedure AddObject(Item: TItemType; UserObject: Pointer);   //TODO: specialize UserObject
    procedure AddSorted(Item: TItemType);
    procedure AddSortedObject(Item: TItemType; UserObject: Pointer);
    procedure Assign(List: TFasterList);
    constructor Create;
    constructor Create(aUnique, aSorted: boolean);
    procedure Clear; virtual;
    destructor Destroy; override;
    procedure Delete(Index: integer);
    procedure DeleteItem(Item: TItemType);  //deletes all instances of the item
    procedure Exchange(Index1, Index2: integer);
    function IndexOf(Item: TItemType): integer;
    // normal TList function
    procedure Insert(Index: integer; Item: TItemType);
    function CheckUnique:boolean;
    procedure MakeUnique;
    procedure SetUnique(val:boolean);
    procedure Swap(I, J: integer); inline;
    procedure QuickSort(L, R: integer); inline;
    procedure Sort;
    procedure SetSorted(val:boolean);
    function SortedIndexOf(Item: TItemType): integer;
    property Capacity: integer
      read FCapacity write FSetCapacity;
    property Count: integer read FCount;
    property IsSorted: boolean read FSorted write SetSorted;
    property Items[Index: integer]: TItemType
      read FGet write FSet; default;
    property Memory: integer read FGetMemory;
    property Objects[Index: integer]: Pointer
      read FGetObject write FSetObject;
    property PtrUintLst: TAoPtrUInt read getPtrUintLst;
    property IsUnique: boolean read FUnique write SetUnique; // for now it must be set before population
  end;

implementation
uses FreeLogger;

{--------------------------------------------------------------------------------------------------}
{                                       TFasterList                                                }
{--------------------------------------------------------------------------------------------------}

function TFasterList.getPtrUintLst:TAoPtrUInt;
begin
  result := TAoPtrUInt(FList);
end;

destructor TFasterList.Destroy;
begin
  Clear;
  inherited Destroy;
end;{TFasterList.Destroy}

procedure TFasterList.Add(Item: TItemType);
var
  //Cur, Prev: PtrUInt;
  i:integer;
begin
  if FSorted then
  begin
    AddSorted(Item);
    exit;
  end;

  if FUnique then
  begin
    i := self.IndexOf(Item);
    if (i >= 0) then
      begin
      exit; //raise Exception.Create('Uniqueness violation');
      end;
  end;

  if FCount = FCapacity then
    FGrow;

  FList[FCount] := Item;
  if FUseUserData then
    FData[FCount] := nil;

  {// only try to maintain sorted if it was sorted
  if FSorted and (FCount > 1) then
  begin
    Prev := PtrUInt(FList[FCount - 1]);
    Cur := PtrUInt(Item);
    FSorted := Prev < Cur;
  end;}

  Inc(FCount);
end;{TFasterList.Add}

procedure TFasterList.AddObject(Item: TItemType; UserObject: Pointer);
var
  Cur, Prev: PtrUInt; i:integer;
begin
  if not FUseUserData then
  begin
    SetLength(FData, FCapacity);
    FUseUserdata := True;
  end;

  if FSorted then
  begin
    AddSortedObject(Item, UserObject);
    exit;
  end;

  if FUnique then
  begin
    i := self.IndexOf(Item);
    if (i >= 0) then
      begin
      //raise Exception.Create('Uniqueness violation');
      if UserObject <> FData[i] then
         FData[i] := UserObject;
      exit;
      end;
  end;

  if FCount = FCapacity then
    FGrow;

  FList[FCount] := Item;
  FData[FCount] := UserObject;

  {
  // only try to maintain sorted if it was sorted
  if FSorted and (FCount > 1) then
  begin
    Prev := PtrUInt(FList[FCount - 1]);
    Cur := PtrUInt(Item);
    FSorted := Prev < Cur;
  end;
  }

  Inc(FCount);
end;{TFasterList.Add}

procedure TFasterList.AddList(
  List: TFasterList);
var
  NewCap: integer;
begin
  if List.FCount = 0 then
    exit;
  NewCap := FCount + List.FCount;

  if (not FUseUserData) and (List.FUseUserData) then
     if NewCap > FCapacity then
        Setlength(FData, NewCap)
     else Setlength(FData, FCapacity);

  FUseUserData := FUseUserData or List.FUseUserData;

  if NewCap > FCapacity then
  begin
    FCapacity := NewCap;
    Setlength(FList, FCapacity);
    //if FUseUserData then
    //  Setlength(FData, FCapacity);
  end;

  Move(List.FList[0], FList[FCount], List.FCount * SizeOf(TItemType));
  if FUseUserData then
    Move(List.FData[0], FData[FCount], List.FCount * SizeOf(Pointer));
  Inc(FCount, List.FCount);

  FSorted := False;
  if FUnique then
    begin
      MakeUnique;
      FUnique := CheckUnique;
    end;
end;{TFasterList.AddList}

procedure TFasterList.AddSorted(Item: TItemType);
var
  Address: PtrUInt;
  L, H, Mid: integer;
begin
  if FUnique and (self.IndexOf(Item)>=0) then
    exit;
  if FCount = FCapacity then
    FGrow;
  if FCount = 0 then
  begin
    FList[FCount] := Item;
    if FUseUserData then
      FData[FCount] := nil;
    Inc(FCount);
  end
  else
  begin
    if not FSorted then begin Add(Item); exit; end;
    Address := PtrUInt(Item);
    // check start
    if Address < PtrUInt(FList[0]) then
    begin
      // insert at start
      Move(FList[0], FList[1], FCount * SizeOf(TItemType));
      FList[0] := Item;
      if FUseUserData then
      begin
        Move(FData[0], FData[1], FCount * SizeOf(Pointer));
        FData[0] := nil;
      end;
    end
    else if Address > PtrUInt(FList[FCount - 1]) then
    begin
      // add at end
      FList[FCount] := Item;
      if FUseUserdata then
        FData[FCount] := nil;
    end
    else
    begin
      // perform binary search to quickly find the location
      L := 0;
      H := FCount - 1;
      while H - L > 1 do
      begin
        //Mid := Floor(0.5 * (L + H));
        Mid := (L + H) div 2;
        if Address < PtrUInt(FList[Mid]) then
          H := Mid - 1
        else
          L := Mid + 1;
      end;
      if Address < PtrUInt(FList[L]) then
        Mid := L
      else
      if Address < PtrUInt(FList[H]) then
        Mid := H
      else
        Mid := H + 1;
      Move(FList[Mid], FList[Mid + 1], (FCount - Mid) * SizeOf(TItemType));
      FList[Mid] := Item;
      if FUseUserdata then
      begin
        Move(FData[Mid], FData[Mid + 1], (FCount - Mid) * SizeOf(Pointer));
        FData[Mid] := nil;
      end;
    end;
    Inc(FCount);
  end;
end;{TFasterList.AddSorted}

procedure TFasterList.AddSortedObject(Item: TItemType; UserObject: Pointer);
var
  Address: PtrUInt;
  L, H, Mid, i: integer;
begin
  if not FUseUserData then
  begin
    FUseUserData := True;
    Setlength(FData, FCapacity);
  end;

  if FUnique then
  begin
    i := self.IndexOf(Item);
    if (i >= 0) then
      begin
      //raise Exception.Create('Uniqueness violation');
      if UserObject <> FData[i] then
         FData[i] := UserObject;
      exit;
      end;
  end;

  if FCount = FCapacity then
    FGrow;

  if FCount = 0 then
  begin
    FList[FCount] := Item;
    if FUseUserData then
      FData[FCount] := UserObject;
    Inc(FCount);
  end
  else
  begin
    if not FSorted then begin AddObject(Item, UserObject); exit; end;
    Address := PtrUInt(Item);
    // check start
    if Address < PtrUInt(FList[0]) then
    begin
      // insert at start
      Move(FList[0], FList[1], FCount * SizeOf(TItemType));
      FList[0] := Item;
      if FUseUserData then
      begin
        Move(FData[0], FData[1], FCount * SizeOf(Pointer));
        FData[0] := UserObject;
      end;
    end
    else if Address > PtrUInt(FList[FCount - 1]) then
    begin
      // add at end
      FList[FCount] := Item;
      if FUseUserdata then
        FData[FCount] := UserObject;
    end
    else
    begin
      // perform binary search to quickly find the location
      L := 0;
      H := FCount - 1;
      while H - L > 1 do
      begin
        //Mid := Floor(0.5 * (L + H));
        Mid := (L + H) div 2;
        if Address < PtrUInt(FList[Mid]) then
          H := Mid - 1
        else
          L := Mid + 1;
      end;
      if Address < PtrUInt(FList[L]) then
        Mid := L
      else
      if Address < PtrUInt(FList[H]) then
        Mid := H
      else
        Mid := H + 1;
      Move(FList[Mid], FList[Mid + 1], (FCount - Mid) * SizeOf(TItemType));
      FList[Mid] := Item;
      if FUseUserdata then
      begin
        Move(FData[Mid], FData[Mid + 1], (FCount - Mid) * SizeOf(Pointer));
        FData[Mid] := UserObject;
      end;
    end;
    Inc(FCount);
  end;
end;{TFasterList.AddSortedObject}

procedure TFasterList.Assign(
   List: TFasterList);
begin
  FUseUserdata := List.FUseUserData;
  Capacity := List.Count;
  Move(List.FList[0], FList[0], List.Count * SizeOf(TItemType));
  if FUseUserdata then
    Move(List.FData[0], FData[0], List.Count * SizeOf(Pointer));
  FCount := List.Count;
  FSorted := List.FSorted;
  FUnique := List.FUnique;
end;{TFasterList.Assign}

constructor TFasterList.Create;
begin
  inherited Create;
  FList := nil;
  FData := nil;
  FCount := 0;
  FCapacity := 0;
  FSorted := False;
  FUnique := False;
end;{TFasterList.Create}

constructor TFasterList.Create(aUnique, aSorted: boolean);
begin
  self.Create;
  FUnique := aUnique;
  FSorted := aSorted;
end;{TFasterList.Create}


procedure TFasterList.Clear;
var l,s:integer;
begin
  if FCapacity > 0
    then l:=0;
  l:=Length(FList);
  s:=SizeOf(TItemType);
  if FCapacity > 0
    then l:=0;

  //s:=SizeOf(Pointer);
  {FillByte(FList, FCapacity * SizeOf(TItemType), 0);
  if FUseUserData then
     FillByte(FData, FCapacity * SizeOf(Pointer), 0);
  }

  FCount := 0;
  FSetCapacity(0);
  FSorted := False;
  //FUnique := False; //leave it as created
  FUseUserData := False;
end;{TFasterList.Clear}

procedure TFasterList.Delete(Index: integer);
begin
  if (Index<0) or (Index >= FCount) then
     raise Exception.Create('Index out of bounds');
  Dec(FCount);
  if Index < FCount then
  begin
    Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TItemType));
    if FUseUserData then
      Move(FData[Index + 1], FData[Index], (FCount - Index) * SizeOf(Pointer));
  end;
end;{TFasterList.Delete}

procedure TFasterList.DeleteItem(Item: TItemType);
var i: integer;
begin
  //logger.IncreaseIndent;
  //logger.Debug(format('TFasterList.DeleteItem[%d]',[(<TItemType>(Item)).FId]));
  i := IndexOf(Item);
  while i >= 0 do
    begin
    Delete(i);
    i := IndexOf(Item);
    end;
  //logger.DecreaseIndent;
end;{TFasterList.DeleteAll}


procedure TFasterList.Exchange(Index1, Index2: integer);
var
  vItem: TItemType; vData:Pointer;
begin
  vItem := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := vItem;
  if FUseUserData then
  begin
    vData := FData[Index1];
    FData[Index1] := FData[Index2];
    FData[Index2] := vData;
  end;
  if FSorted then
    FSorted := False;
end;{TFasterList.Exchange}

function TFasterList.FGet(Index: integer): TItemType;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList[Index]
  else
    Result := nil;
end;{TFasterList.FGet}

function TFasterList.FGetObject(Index: integer): TItemType;
begin
  if (Index >= 0) and (Index < FCount) and (FUseUserData) then
    Result := FData[Index]
  else
    Result := nil;
end;{TFasterList.FGetObject}

function TFasterList.FGetMemory: integer;
begin
  Result := SizeOf(Pointer) +           // self        : TItemType
    SizeOf(integer) +           // FCapacity   : integer
    SizeOf(integer) +           // fcount      : integer
    SizeOf(boolean) +           // FSorted     : Boolean
    SizeOf(boolean) +           // FUseUserdata: boolean
    SizeOf(TItemType) * FCapacity;
  if FUseUserData then
    Inc(Result, FCapacity * SizeOf(Pointer));
end;{TFasterList.FGetMemory}

procedure TFasterList.FGrow;
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
end;{TFasterList.FGrow}

function TFasterList.IndexOf(Item: TItemType): integer;
var
  I: integer;
begin
  if FSorted then
   begin
     result:=SortedIndexOf(Item);
     exit;
   end;

  Result := -1;
  for I := 0 to FCount-1 do
    if FList[I] = Item then
    begin
      Result := I;
      break;
    end;
end;{TFasterList.IndexOf}

procedure TFasterList.Insert(Index: integer; Item: TItemType);
begin
  if (index<0) or (Index >= FCount) then
    raise Exception.Create('Index out of bounds');
  if FUnique and (self.IndexOf(Item)>=0) then
    exit; //raise Exception.Create('Uniqueness violation');
  if FCount = FCapacity then
    FGrow;
  if Index < FCount then
  begin
    Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(TItemType));
    if FUseUserData then
      Move(FData[Index], FData[Index + 1], (FCount - Index) * SizeOf(Pointer));
  end;
  FList[Index] := Item;
  if FUseUserData then
    FData[index] := nil;
  if FSorted then
    FSorted := False;  // TODO make smart
  Inc(FCount);
end;{TFasterList.Insert}

procedure TFasterList.MakeUnique;
var I, J: integer;
begin
  i:=FCount-1;
  while i>=0 do
    begin
    for j:=0 to i-1 do
      if FList[i] = FList[j] then
        Delete(i);
    dec(i);
    end;
end;

procedure TFasterList.SetUnique(val:boolean);
begin
  if val then
     begin if not FUnique then MakeUnique; end
  else FUnique := false;
end;

function TFasterList.CheckUnique:boolean;
var I, J: integer;
begin
  result:=true;
  for i:=0 to FCount-1 do
    for j:=i+1 to FCount-1 do
      if FList[i] = FList[j] then
        begin
        result:=false;
        exit;
        end;
end;

procedure TFasterList.Swap(I, J: integer);
var
  Tmp: TItemType; TmpDt:Pointer;
begin
  Tmp := FList[I];
  FList[I] := FList[J];
  FList[J] := Tmp;
  if FUseUserdata then
  begin
    TmpDt := FData[I];
    FData[I] := FData[J];
    FData[J] := TmpDt;
  end;
end;

procedure TFasterList.QuickSort(L, R: integer);
var
  I, J: integer;
  Val: PtrUInt;
begin
  I := L;
  J := R;
  Val := PtrUInt(FList[(L + R) div 2]);
  repeat
    while PtrUInt(FList[I]) < Val do
      Inc(I);
    while Val < PtrUInt(FList[J]) do
      Dec(J);
    if I <= J then
    begin
      Swap(I, J);
      Inc(I);
      Dec(J);
    end;
  until I > J;
  if L < J then
    QuickSort(L, J);
  if I < R then
    QuickSort(I, R);
end;{QuickSort}

procedure TFasterList.Sort;
begin
  if (FCount = 0) then
    FSorted := True;
  if (FCount > 1) and (not FSorted) then
  begin
    QuickSort(0, FCount - 1);
    FSorted := True;
  end;
end;{TFasterList.Sort}

procedure TFasterList.SetSorted(val:boolean);
begin
  if val then
    begin
      if not FSorted
        then Sort;
    end
  else
    FSorted := false;
end;

function TFasterList.SortedIndexOf(Item: TItemType): integer;
var
  MemAddr: PtrUInt;
  MidVal: PtrUInt;
  L, H, Mid : integer;
begin
  if not FSorted then
     begin
       result:=IndexOf(Item);
       exit;
     end;

  Result := -1;
  MemAddr := PtrUInt(Item);
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    //Mid := Floor(0.5 * (L + H));
    Mid := (L + H) div 2;
    MidVal := PtrUInt(FList[Mid]);
    if MemAddr = MidVal then
    begin
      Result := Mid;
      exit;
    end
    else
    begin
      if MemAddr < MidVal then
      begin
        H := Mid - 1;
      end
      else
      begin
        L := Mid + 1;
      end;
    end;
  end;
end;{TFasterList.SortedIndexOf}

procedure TFasterList.FSet(Index: integer; Item: TItemType);
begin
  if (Index<0) or (Index>=FCount) then
    raise Exception.Create('Index out of bounds');
  if FUnique and (IndexOf(Item)>=0) then
    raise Exception.Create('Uniqueness violation');
  FList[Index] := Item;
  if FUseUserData then
    FData[index] := nil;
end;{TFasterList.FSet}

procedure TFasterList.FSetObject(Index: integer; UserObject: Pointer);
begin
  if (Index<0) or (Index>=FCount) then
    raise Exception.Create('Index out of bounds');
  if not FUseUserdata then
  begin
    Setlength(FData, FCapacity);
    FUseUserdata := True;
  end;
  FData[Index] := UserObject;
end;{TFasterList.FSetObject}

procedure TFasterList.FSetCapacity(NewCapacity: integer);
begin
  if FCapacity = NewCapacity then exit;
  Setlength(FList, NewCapacity);
  if FUseUserData then
    Setlength(FData, NewCapacity);
  FCapacity := NewCapacity;
  if FCapacity < FCount then
    FCount := Fcapacity;
end;{TFasterList.FSetCapacity}

end.
