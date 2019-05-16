{---------------------------------------------------------------------------------------------------}
{                                           TFreeSubdivisionBase                                    }
{ TFreeSubdivisionBase is the base class for all subdivision points, edges and faces                }
{---------------------------------------------------------------------------------------------------}

var FIdSequence:integer=0;

constructor TFreeSubdivisionBase.Create(Owner: TFreeSubdivisionSurface);
begin
  inherited Create;
  FOwner := Owner;
  inc(FIdSequence);
  FId:=FIdSequence;
  InUnreference := false;
  IsUnreferenceEnabled := true;
end;{TFreeSubdivisionBase.Create}

procedure TFreeSubdivisionBase.PrintDebug;
begin
  Logger.Debug( self.ClassName+'.FOwner:'+ IntToHex(QWord(FOwner),16));
end;

