// 
//  Interface unit for the fortlib.dll library.
//  
//  Generated 2001-12-18 16:18:02
//    by Fortran interface wizard 1.0.1
// 

unit fortlib;

interface

uses
    Classes, SysUtils;

type

    // 
    // The following datatypes corresponds to Fortran datatypes.
    // 


    PDoubleArray = ^TDoubleArray;
    TDoubleArray = array [0..0] of double;

    PIntegerArray = ^TIntegerArray;
    TIntegerArray = array [0..0] of integer;

    PDoubleMatrix = ^TDoubleMatrix;
    TDoubleMatrix = array [0..0,0..0] of double;

    PIntegerMatrix = ^TIntegerMatrix;
    TIntegerMatrix = array [0..0,0..0] of integer;

// 
// Procedures publicly available.
// 

procedure adder(a,b : integer;var c : integer); 
stdcall;
procedure adderR(a,b : double;var c : double); 
stdcall;
procedure FSresist(Vs : double); 
stdcall;
procedure FSpropel(Vs : double); 
stdcall;
procedure FSexfact(Vs,C : double;var Ke : double); 
stdcall;

implementation

// 
// Interface unit implementation.
//

//
// NOTE:
// Put the dll-library in the search path or change the location
// of the library below
//

procedure adder(a,b : integer;var c : integer); 
stdcall;
external 'fortlib.dll' name '_adder@12';

procedure adderR(a,b : double;var c : double); 
stdcall;
external 'fortlib.dll' name '_adderr@20';

procedure FSresist(Vs : double); 
stdcall;
external 'fortlib.dll' name '_fsresist@8';

procedure FSpropel(Vs : double); 
stdcall;
external 'fortlib.dll' name '_fspropel@8';

procedure FSexfact(Vs,C : double;var Ke : double); 
stdcall;
external 'fortlib.dll' name '_fsexfact@20';

end.
