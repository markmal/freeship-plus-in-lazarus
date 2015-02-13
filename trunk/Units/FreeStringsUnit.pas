unit FreeStringsUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

Type TUserstring   = record
                        ID     : Integer;
                        Value  : string;
                     end;

const UserStrings:array[0..8] of TUserstring =(
   (ID:0275; Value:'according to John Winters'),
   (ID:0131; Value:'Add cylinder'),
   (ID:0130; Value:'Add flowline'),
   (ID:0227; Value:'Add multiple buttocks'),
   (ID:0233; Value:'Add multiple diagonals'),
   (ID:0224; Value:'Add multiple stations'),
   (ID:0230; Value:'Add multiple waterlines'),
   (ID:0226; Value:'Add one buttock'),
   (ID:0232; Value:'Add one diagonal')
);

implementation

end.
