unit FreeViewPortOpenGL;

{with helpful stuff from unit UOpenGLCanvas by Qianyuan Wang}

{$mode delphi}
{ $mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  FreeTypes,
  FreeGeometry,
  GL, OpenGLContext;

type
  TFreeGLCanvas = class (TCanvas)

  end;

  TFreeViewportOpenGL = class (TFreeViewport)
     OpenGLControl: TOpenGLControl;
     procedure OpenGLControl1Paint(Sender: TObject);
     procedure OpenGLControl1Resize(Sender: TObject);
  private
     AreaInitialized: boolean;
     FPenWidth : integer;
  protected
     procedure Paint;                                                        override;
     procedure Resize;                                                       override;
     procedure InitGL;
  public
     constructor Create(AOwner:TComponent);                                  override;
     destructor Destroy;                                                     override;
     procedure DrawLineToZBuffer(Point1,Point2:T3DCoordinate;R,G,B:Integer); override;
     procedure InitializeViewport(Min,Max:T3DCoordinate);                    override;
     procedure Print(Units:TFreeUnitType;AskPrintScale:Boolean;Jobname:string); override;
     procedure SaveAsBitmap(Filename:string;const ShowDialog:boolean=true); override;
     procedure SetPenWidth(Width:integer); override;
     Procedure ShadedColor(Dp:single;R,G,B:byte;var ROut,GOut,BOut:byte);     override;
     procedure ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;R,G,B:byte;Alpha:byte);override;
     procedure ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;C1,C2,C3:Extended);    override;
     procedure ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;R1,G1,B1,R2,G2,B2,R3,G3,B3:byte);override;
  end;

implementation

constructor TFreeViewportOpenGL.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  OpenGLControl := TOpenGLControl.Create(Self);
  with OpenGLControl do
    begin
      Name:='OpenGLControl1';
      Parent := Self;
      SetBounds(0,0, Self.Width, Self.Height);
      OnPaint:=Self.OpenGLControl1Paint;
      OnResize:=Self.OpenGLControl1Resize;
      Visible:=true;
      Invalidate;
    end;
  AreaInitialized:=false;
end;

procedure TFreeViewportOpenGL.Paint;
begin
  inherited Paint;
  //if Assigned(OpenGLControl) then
  //OpenGLControl.Paint;
end;


procedure TFreeViewportOpenGL.Resize;
begin
  inherited Resize;
  if Assigned(OpenGLControl) then
    OpenGLControl.SetBounds(0,0,Width,Height);
end;


destructor TFreeViewportOpenGL.Destroy;
begin
  if Assigned(OpenGLControl) then
    OpenGLControl.Destroy;
  inherited Destroy;
end;

procedure TFreeViewportOpenGL.DrawLineToZBuffer(Point1,Point2:T3DCoordinate;R,G,B:Integer);
begin
  raise Exception.create('Not implemented');
end;

procedure TFreeViewportOpenGL.InitializeViewport(Min,Max:T3DCoordinate);
begin
  inherited InitializeViewport(Min,Max);
  AreaInitialized:=false;
end;

procedure TFreeViewportOpenGL.Print(Units:TFreeUnitType;AskPrintScale:Boolean;Jobname:string);
begin
  raise Exception.create('Not implemented');
end;

procedure TFreeViewportOpenGL.SaveAsBitmap(Filename:string;const ShowDialog:boolean=true);
begin
  raise Exception.create('Not implemented');
end;

procedure TFreeViewportOpenGL.SetPenWidth(Width:integer);
begin
  FPenWidth := Width;
  glLineWidth(FPenWidth);
  glPointSize(FPenWidth);
end;

procedure TFreeViewportOpenGL.ShadedColor(Dp:single;R,G,B:byte;var ROut,GOut,BOut:byte);
begin
   raise Exception.create('Not implemented');
end;

procedure TFreeViewportOpenGL.ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;R,G,B:byte;Alpha:byte);
begin
  glBegin(GL_LINE_LOOP);
  glColor4i(R,G,B,Alpha);
  glVertex2f(P_1.X, P_1.Y);
  glVertex2f(P_2.X, P_2.Y);
  glVertex2f(P_3.X, P_3.Y);
  glEnd;
end;

procedure TFreeViewportOpenGL.ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;C1,C2,C3:Extended);
begin
  glBegin(GL_LINE_LOOP);
  glVertex2f(P_1.X, P_1.Y);
  glVertex2f(P_2.X, P_2.Y);
  glVertex2f(P_3.X, P_3.Y);
  glEnd;
end;

procedure TFreeViewportOpenGL.ShadeTriangle(P_1,P_2,P_3:T3DCoordinate;R1,G1,B1,R2,G2,B2,R3,G3,B3:byte);
begin
  glBegin(GL_LINE_LOOP);
  glColor3i(R1,G1,B1);
  glVertex2f(P_1.X, P_1.Y);
  glColor3i(R2,G2,B2);
  glVertex2f(P_2.X, P_2.Y);
  glColor3i(R3,G3,B3);
  glVertex2f(P_3.X, P_3.Y);
  glEnd;
end;

procedure TFreeViewportOpenGL.InitGL;
begin

end;



// This one draws GL
procedure TFreeViewportOpenGL.OpenGLControl1Paint(Sender: TObject);
 var CTF:GLclampf=1.0/255.0;
begin
  if OpenGLControl.MakeCurrent then
  begin
    if not AreaInitialized then begin
      InitGL;
      glMatrixMode (GL_PROJECTION);    { prepare for and then }
      glLoadIdentity ();               { define the projection }
      glMatrixMode (GL_MODELVIEW);  { back to modelview matrix }
      glViewport (0, 0, OpenGLControl.Width, OpenGLControl.Height);
                                    { define the viewport }
      AreaInitialized:=true;
    end;

    glClearColor( CTF*Red(Self.Color), CTF*Green(Self.Color), CTF*Blue(Self.Color), 0.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glLoadIdentity;             { clear the matrix }

    //drawGeometry()  FreeShip.DrawToViewport(Self);

    glFlush;
    glFinish;
    // Swap backbuffer to front
    OpenGLControl.SwapBuffers;
  end;
end;

procedure TFreeViewportOpenGL.OpenGLControl1Resize(Sender: TObject);
begin
  if (AreaInitialized)
   and OpenGLControl.MakeCurrent then
    glViewport (0, 0, OpenGLControl.Width, OpenGLControl.Height);
end;



end.

