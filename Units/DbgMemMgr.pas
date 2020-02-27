unit DbgMemMgr;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

Const
  LibName = 'libc';

Function Malloc (Size : ptrint) : Pointer;
  cdecl; external LibName name 'malloc';
Procedure Free (P : pointer);
  cdecl; external LibName name 'free';
function ReAlloc (P : Pointer; Size : ptrint) : pointer;
  cdecl; external LibName name 'realloc';
Function CAlloc (unitSize,UnitCount : ptrint) : pointer;
  cdecl; external LibName name 'calloc';

implementation

type
  pptruint = ^ptruint;

Function CGetMem  (Size : ptruint) : Pointer;
begin
  CGetMem:=Malloc(Size+sizeof(ptrint));
  if (CGetMem <> nil) then
    begin
      pptrint(CGetMem)^ := size;
      inc(CGetMem,sizeof(ptrint));
    end;
end;

Function CFreeMem (P : pointer) : ptruint;
begin
  if (p <> nil) then
    dec(p,sizeof(ptrint));
  Free(P);
  CFreeMem:=0;
end;

Function CFreeMemSize(p:pointer;Size:ptruint):ptruint;
begin
  if size<=0 then
    begin
      if size<0 then
        runerror(204);
      exit;
    end;
  if (p <> nil) then
    begin
      if (size <> pptruint(p-sizeof(ptruint))^) then
        runerror(204);
    end;
  CFreeMemSize:=CFreeMem(P);
end;

Function CAllocMem(Size : ptruint) : Pointer;
begin
  CAllocMem:=calloc(Size+sizeof(ptruint),1);
  if (CAllocMem <> nil) then
    begin
      pptruint(CAllocMem)^ := size;
      inc(CAllocMem,sizeof(ptruint));
    end;
end;

Function CReAllocMem (var p:pointer;Size:ptruint):Pointer;
begin
  if size=0 then
    begin
      if p<>nil then
        begin
          dec(p,sizeof(ptruint));
          free(p);
          p:=nil;
        end;
    end
  else
    begin
      inc(size,sizeof(ptruint));
      if p=nil then
        p:=malloc(Size)
      else
        begin
          dec(p,sizeof(ptruint));
          p:=realloc(p,size);
        end;
      if (p <> nil) then
        begin
          pptruint(p)^ := size-sizeof(ptruint);
          inc(p,sizeof(ptruint));
        end;
    end;
  CReAllocMem:=p;
end;

Function CMemSize (p:pointer): ptruint;
begin
  CMemSize:=pptruint(p-sizeof(ptruint))^;
end;

function CGetHeapStatus:THeapStatus;
var res: THeapStatus;
begin
  fillchar(res,sizeof(res),0);
  CGetHeapStatus:=res;
end;

function CGetFPCHeapStatus:TFPCHeapStatus;
begin
  fillchar(CGetFPCHeapStatus,sizeof(CGetFPCHeapStatus),0);
end;

Const
 CMemoryManager : TMemoryManager =
    (
      NeedLock : false;
      GetMem : @CGetmem;
      FreeMem : @CFreeMem;
      FreememSize : @CFreememSize;
      AllocMem : @CAllocMem;
      ReallocMem : @CReAllocMem;
      MemSize : @CMemSize;
      InitThread : Nil;
      DoneThread : Nil;
      RelocateHeap : Nil;
      GetHeapStatus : @CGetHeapStatus;
      GetFPCHeapStatus: @CGetFPCHeapStatus;
    );

Var
  OldMemoryManager : TMemoryManager;

Initialization
  GetMemoryManager (OldMemoryManager);
  SetMemoryManager (CmemoryManager);

Finalization
  SetMemoryManager (OldMemoryManager);
end.


