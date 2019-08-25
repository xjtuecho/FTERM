unit AnsiEmulVT;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, emulvt;

type
  TMouseOp = (moSelection, moDraw, moMoveSelection);

  TAnsiEmulVT = class(TEmulVT)
  private
    { Private declarations }
    FAfterSelection: TNotifyEvent;
    SaveLines: array[0..50] of TLine;
    FMoveStart: TPoint;
    function AnsiCopyOneLine(i, c1, c2: integer): string;
    function AttToEscChar(att: word; var bblink, bhigh: boolean;
      var fore, back: byte): string;
    function MoveRect(var Rect1: TRect; dx, dy: integer): boolean;
    function AdjustRect(var Rect1: TRect): boolean;
  protected
    { Protected declarations }
    FMouseDown: boolean;
    FMouseTop: integer;
    FMouseLeft: integer;
    FFocusDrawn: boolean;
    FFocusRect: TRect;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
  public
    { Public declarations }
    MouseMoveOp: TMouseOp;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CutSelected(r: TRect);
    procedure RectToRowCol(r: TRect; var r1, r2, c1, c2: integer);
    procedure SaveScreenLines;
    procedure DrawFocusRectangle(Wnd: HWnd; Rect: TRect);
    procedure UnSelect;
    procedure CopyToClipBoard(bAnsi: boolean);
    procedure CutToClipBoard(bAnsi: boolean);
    procedure Delete;
    procedure BrightSelection;
    procedure DeBrightSelection;
    procedure SetAttrSelection(Att: word);
    procedure SetTextSelection(txt: string);
    procedure SetText(x, y: integer; att: word; s1: string);
    procedure DrawTableBorder(att: word);
    procedure DrawTableLine(att: word);
    procedure DrawCnTableBorder(att: word);
    procedure DrawCnTableLine(att: word);
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
  published
    { Published declarations }
    property AfterSelection: TNotifyEvent read FAfterSelection write FAfterSelection;
  end;

procedure Register;

implementation

uses Clipbrd;

procedure Register;
begin
  RegisterComponents('Samples', [TAnsiEmulVT]);
end;

constructor TAnsiEmulVT.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited Create(AOwner);

  SingleCharPaint := True;
  for i := 0 to 40 do 
  begin
    SaveLines[i] := TLine.Create;
  end;
end;

destructor TAnsiEmulVT.Destroy;
var
  i: integer;
begin
  for i := 0 to Rows - 1 do 
  begin
    SaveLines[i].Free;
  end;
  inherited Destroy;
end;

{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TAnsiEmulVT.DrawFocusRectangle(Wnd: HWnd; Rect: TRect);
var
  DC: HDC;
  r1: TRect;
  l1: integer;
begin
  DC := GetDC(Wnd);

  if not FFocusDrawn then 
  begin
    DrawSelectRect(DC, rect);
  end
  else if SelectMode = smBlock then 
  begin
    {DrawSelectRect(DC, FFocusRect);
    DrawSelectRect(DC, rect);
    }
    if ((rect.Bottom >= rect.Top) and (FFocusRect.Bottom <= FFocusRect.Top)) or
      ((rect.Bottom <= rect.Top) and (FFocusRect.Bottom >= FFocusRect.Top)) then 
    begin
      DrawSelectRect(DC, FFocusRect);
      DrawSelectRect(DC, rect);
    end
    else if rect.Bottom <= rect.Top then 
    begin
      if FFocusRect.Bottom > rect.Bottom then 
      begin
        r1 := rect;
        r1.Top := FFocusRect.Bottom;
        DrawSelectRect(DC, r1);
        r1 := rect;
        r1.Bottom := FFocusRect.Bottom;
        r1.Left := FFocusRect.Right;
        DrawSelectRect(DC, r1);
      end
      else 
      begin
        r1 := FFocusRect;
        r1.Top := rect.Bottom;
        DrawSelectRect(DC, r1);
        r1 := FFocusRect;
        r1.Bottom := rect.Bottom;
        r1.Left := rect.Right;
        DrawSelectRect(DC, r1);
      end;
    end
    else 
    begin
      if FFocusRect.Bottom <= rect.Bottom then 
      begin
        r1 := rect;
        r1.Left := FFocusRect.Right;
        DrawSelectRect(DC, r1);
        r1 := rect;
        r1.Right := FFocusRect.Right;
        r1.Top := FFocusRect.Bottom;
        DrawSelectRect(DC, r1);
      end
      else 
      begin
        r1 := FFocusRect;
        r1.Left := rect.Right;
        DrawSelectRect(DC, r1);
        r1 := FFocusRect;
        r1.Right := rect.Right;
        r1.Top := rect.Bottom;
        DrawSelectRect(DC, r1);
      end;
    end;
  end
  else if (rect.Top >= rect.Bottom) then 
  begin
    if (FFocusRect.Top < FFocusRect.Bottom) then 
    begin
      DrawSelectRect(DC, FFocusRect);
      DrawSelectRect(DC, rect);
    end
    {else if (FFocusRect.Bottom < TopMargin) or (rect.Bottom < TopMargin) then begin
      DrawSelectRect(DC, FFocusRect);
      DrawSelectRect(DC, rect);
    end}
    else 
    begin
      if FFocusRect.Bottom > rect.Bottom then 
      begin
        l1 := PixelToRow(rect.Bottom);
        if rect.Bottom < TopMargin then l1 := -1;
        r1.Top := TopMargin + FLinePos[l1 + 1];
        r1.Left := rect.Right;
        l1 := PixelToRow(FFocusRect.Bottom);
        if FFocusRect.Bottom < TopMargin then l1 := -1;
        r1.Bottom := TopMargin + FLinePos[l1 + 2];
        r1.Right := FFocusRect.Right;
        DrawSelectRect(DC, r1);
      end
      else 
      begin
        l1 := PixelToRow(FFocusRect.Bottom);
        if FFocusRect.Bottom < TopMargin then l1 := -1;
        r1.Top := TopMargin + FLinePos[l1 + 1];
        r1.Left := FFocusRect.Right;
        l1 := PixelToRow(rect.Bottom);
        if rect.Bottom < TopMargin then l1 := -1;
        r1.Bottom := TopMargin + FLinePos[l1 + 2];
        r1.Right := rect.Right;
        DrawSelectRect(DC, r1);
      end;
    end;
  end
  else if (rect.Top < rect.Bottom) and (FFocusRect.Top >= FFocusRect.Bottom) then 
  begin
    DrawSelectRect(DC, FFocusRect);
    DrawSelectRect(DC, rect);
  end
  else if (rect.Top = FFocusRect.Top) and (rect.Left = FFocusRect.Left) and
    (rect.Top < rect.Bottom) then 
  begin
    if rect.Bottom = FFocusRect.Bottom then 
    begin
      l1 := PixelToRow(FFocusRect.Bottom - LineHeight);
      r1.Left := rect.Right; 
      r1.Right := FFocusRect.Right;
      r1.Top := rect.Bottom; 
      r1.Bottom := TopMargin + FLinePos[l1];
      InvertRect(DC, r1);
    end
    else if rect.Bottom > FFocusRect.Bottom then 
    begin
      l1 := PixelToRow(FFocusRect.Bottom - LineHeight);
      r1 := rect;
      r1.Left := FFocusRect.Right; 
      r1.Top := TopMargin + FLinePos[l1];
      DrawSelectRect(DC, r1);
    end
    else 
    begin
      l1 := PixelToRow(rect.Bottom - LineHeight);
      r1 := FFocusRect;
      r1.Left := rect.Right; 
      r1.Top := TopMargin + FLinePos[l1];
      DrawSelectRect(DC, r1);
    end;
  end;
  ReleaseDC(Wnd, DC);
end;

procedure TAnsiEmulVT.SaveScreenLines;
var
  i: integer;
begin
  for i := 0 to Rows do 
  begin
    Move(Screen.Lines[i].Txt, SaveLines[i].Txt, SizeOf(SaveLines[i].Txt));
    Move(Screen.Lines[i].Att, SaveLines[i].Att, SizeOf(SaveLines[i].Att));
  end;
end;

procedure TAnsiEmulVT.CutSelected(r: TRect);
var
  i, j: integer;
  r1, r2: integer;
  c1, c2: integer;
begin
  AdjustRect(r);
  RectToRowCol(r, r1, r2, c1, c2);
  for i := r1 to r2 do 
  begin
    for j := c1 to c2 do 
    begin
      SaveLines[i].Txt[j] := ' ';
      SaveLines[i].Att[j] := 7;
    end;
  end;
end;

function TAnsiEmulVT.AdjustRect(var Rect1: TRect): boolean;
var
  r: TRect;
  c1, c2, i: integer;
  bExchange: boolean;
begin
  r := Rect1;
  bExchange := False;

  if (r.Top > r.Bottom) then
  begin
    c1 := r.Left;
    c2 := r.Top;
    i := DrawLineHeight(1);
    r.Left := r.Right;
    r.Top := r.Bottom + i;
    r.Right := c1;
    r.Bottom := c2 + i;
    bExchange := True;
  end;

  if (r.Left > r.Right) then 
  begin
    c1 := r.Left;
    r.Left := r.Right;
    r.Right := c1;
    bExchange := True;
  end;

  Result := bExchange;
end;

procedure TAnsiEmulVT.RectToRowCol(r: TRect; var r1, r2, c1, c2: integer);
begin
  r1 := PixelToRow(r.Top);
  r2 := PixelToRow(r.Bottom - LineHeight + 1);
  if (r1 = r2) and (r.Left > r.Right) then
  begin
    c2 := PixelToCol(r.Left - 2);
    c1 := PixelToCol(r.Right);
  end
  else
  begin
    c1 := PixelToCol(r.Left);
    c2 := PixelToCol(r.Right - 2);
  end;
end;

function TAnsiEmulVT.MoveRect(var Rect1: TRect; dx, dy: integer): boolean;
var
  c1, c2, c3, c4: integer;
  r1, r2, r3, r4: integer;
  i, j: integer;
  r: TRect;
  saveline2: array [0..50] of TLine;
begin
  Result := False;
  if Rect1.Top < 0 then Exit;

  r := Rect1;

  AdjustRect(r);

  r1 := PixelToRow(r.Top);
  r2 := PixelToRow(r.Bottom - LineHeight + 1);
  if (r1 = r2) and (r.Left > r.Right) then
  begin
    c2 := PixelToCol(r.Left - 2);
    c1 := PixelToCol(r.Right);
  end
  else
  begin
    c1 := PixelToCol(r.Left);
    c2 := PixelToCol(r.Right - 2);
  end;

  Dec(r.Left, dx); 
  Dec(r.Right, dx);
  Dec(r.Top, dy); 
  Dec(r.Bottom, dy);

  if (r.Left < 0) or (r.Top < 0) then Exit;

  r3 := PixelToRow(r.Top);
  r4 := r3 + r2 - r1;
  if (r3 = r4) and (r.Left > r.Right) then
  begin
    c4 := PixelToCol(r.Left - 2);
    c3 := PixelToCol(r.Right);
  end
  else
  begin
    c3 := PixelToCol(r.Left);
    c4 := PixelToCol(r.Right - 2);
  end;

  for i := r1 to r2 do 
  begin
    saveline2[i] := TLine.Create;
    Move(Screen.Lines[i].Txt, saveline2[i].Txt, SizeOf(saveline2[i].Txt));
    Move(Screen.Lines[i].Att, saveline2[i].Att, SizeOf(saveline2[i].Att));
  end;
  for i := r3 to r4 do
  begin
    for j := c3 to c4 do
    begin
      Screen.Lines[i].Txt[j] := saveline2[i + r1 - r3].Txt[j + c1 - c3];
      // Screen.Lines[i+r1-r3].Txt[j+c1-c3];
      Screen.Lines[i].Att[j] := saveline2[i + r1 - r3].Att[j + c1 - c3];
      //Screen.Lines[i+r1-r3].Att[j+c1-c3];
    end;
  end;
  for i := r1 to r2 do 
  begin
    saveline2[i].Free;
  end;

  for i := r1 to r2 do
  begin
    for j := c1 to c2 do
    begin
      if ((i < r3) or (i > r4) or (j < c3) or (j > c4)) then 
      begin
        Screen.Lines[i].Txt[j] := savelines[i].Txt[j];
        Screen.Lines[i].Att[j] := savelines[i].Att[j];
      end;
    end;
  end;

  if (r1 <> r3) or (c3 <> c1) then 
  begin
    Rect1 := r;
{    Dec(Rect1.Left, dx);
    Dec(Rect1.Top, dy);
    Dec(Rect1.Right, dx);
    Dec(Rect1.Bottom, dy);
}
  end;
  Result := True;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAnsiEmulVT.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbRight then Exit;
  FMouseDown := True;

  if (SelectRect.Top > 0) and (MouseMoveOp = moMoveSelection) then 
  begin
    //SaveScreenLines;
    FMoveStart.X := SnapPixelToCol(X);
    FMoveStart.Y := SnapPixelToRow(Y);
    Exit;
  end;

  if FFocusDrawn then
  begin
    //if not swapdraw then DrawFocusRectangle(Handle, FFocusRect);
    FFocusDrawn := False;
  end;
  if (SelectRect.Top <> -1) and (Button = mbLeft) then
  begin
    FFocusRect.Top := -1;
    SelectRect := FFocusRect;
    //if swapdraw then UpdateScreen;
    Repaint;
  end;
end;

function EqualRect(r1, r2: TRect): boolean;
begin
  if (r1.Left = r2.Left) and (r1.Top = r2.Top) and (r1.Right = r2.Right) and
    (r1.Bottom = r2.Bottom) then Result := True
  else 
    Result := False;
end;

{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TAnsiEmulVT.MouseMove(Shift: TShiftState; X, Y: integer);
var
  Rect: TRect;
  Point: TPoint;
  i, j, ch1, ch2: integer;
  pstr: string;
  r: TRect;
begin
  inherited MouseMove(Shift, X, Y);

  if not FMouseDown then
    Exit;

  if not FMouseCaptured then
  begin
    SetCapture(Handle);
    FMouseCaptured := True;
    FMouseTop := SnapPixelToRow(Y);
    FMouseLeft := SnapPixelToCol(X);

    Point.X := 0;
    Point.Y := 0;
    Rect.TopLeft := ClientToScreen(Point);
    Point.X := Width;
    Point.Y := Height;
    Rect.BottomRight := ClientToScreen(Point);
    ClipCursor(@Rect);
  end
  else if (MouseMoveOp = moMoveSelection) then 
  begin
    FFocusRect := SelectRect;
    AdjustRect(FFocusRect);
    i := SnapPixelToRow(FFocusRect.Top - FMoveStart.Y + Y);
    j := SnapPixelToCol(FFocusRect.Left - FMoveStart.X + X);
    if j < LeftMargin then j := LeftMargin + FMoveStart.X - FFocusRect.Left;
    if i < TopMargin then i := TopMargin + FMoveStart.Y - FFocusRect.Top;
    if j + FFocusRect.Right - FFocusRect.Left > LeftMargin + FCharPos[Cols] then
      j := LeftMargin + FCharPos[Cols] + FFocusRect.Left - FFocusRect.Right;
    if (i + FFocusRect.Bottom - FFocusRect.Top) > TopMargin + FLinePos[Rows] then
      i := TopMargin + FLinePos[Rows] + FFocusRect.Top - FFocusRect.Bottom;
    if (j = FFocusRect.Left) and (i = FFocusRect.Top) then Exit;
    if MoveRect(FFocusRect, FFocusRect.Left - j, FFocusRect.Top - i) then 
    begin
      FMouseLeft := SnapPixelToCol(X);; 
      FMouseTop := SnapPixelToRow(Y);
      FMoveStart.X := FMouseLeft;
      FMoveStart.Y := FMouseTop;
      SelectRect := FFocusRect;
      r := GetClientRect;
      //InvalidateRect(Handle, @r, False);
      Refresh;
    end;
    Exit;
  end
  else if (FMouseTop <> Y) or (FMouseLeft <> X) then
  begin
    Rect.Top := FMouseTop;
    Rect.Left := FMouseLeft;
    i := PixelToRow(Y);
    if i >= Rows then Exit;
    if Y > Rect.Top then
      Rect.Bottom := SnapPixelToRow(Y) + FLinePos[i + 1] - FLinePos[i]
    else
    begin
      {if Y < TopMargin then begin
        Rect.Bottom := TopMargin;
      end
      else}
      if i > 1 then
        Rect.Bottom := SnapPixelToRow(Y) - (FLinePos[i] - FLinePos[i - 1])
      else
        Rect.Bottom := SnapPixelToRow(Y) - FLinePos[1];
    end;
    //i := PixelToCol(X);
    Rect.Right := SnapPixelToCol(X);

    if EqualRect(FFocusRect, Rect) then Exit;

    //    if FFocusDrawn and (not swapdraw) then DrawFocusRectangle(Handle, FFocusRect);

    if (MouseMoveOp = moSelection) then
      DrawFocusRectangle(Handle, Rect);

    FFocusRect := Rect;
    if (MouseMoveOp = moDraw) then 
    begin
      FFocusRect.Top := -1;
    end;
    SelectRect := FFocusRect;       {fuse +}
    FFocusDrawn := True;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TAnsiEmulVT.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  urlstr: string;
  i: integer;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Button = mbRight then Exit;

  FMouseDown := False;
  if FMouseCaptured then
  begin
    ReleaseCapture;
    FMouseCaptured := False;
    ClipCursor(nil);
  end;

  if FFocusDrawn and (MouseMoveOp = moMoveSelection) then 
  begin
    Exit;
  end;

  if FFocusDrawn then
  begin
    //if not swapdraw then InvalidateRect(Handle, @SelectRect, false); //DrawFocusRectangle(Handle, FFocusRect);
    FFocusDrawn := False;
    i := PixelToCol(FFocusRect.Left);
    if Abs(FFocusRect.Right - FFocusRect.Left) < DrawCharWidth(i) then
      FFocusRect.Top := -1;
    i := PixelToRow(FFocusRect.Top);
    if Abs(FFocusRect.Bottom - FFocusRect.Top) < DrawLineHeight(i) then
      FFocusRect.Top := -1;
    if (MouseMoveOp = moDraw) then 
    begin
      FFocusRect.Top := -1;
    end;
    SelectRect := FFocusRect;
    //if swapdraw then UpdateScreen;
    InvalidateRect(Handle, @SelectRect, False);
    //Repaint;
    if Assigned(FAfterSelection) then FAfterSelection(self);
    Exit;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TAnsiEmulVT.UnSelect;
begin
  if FMouseCaptured then
  begin
    ReleaseCapture;
    FMouseCaptured := False;
    ClipCursor(nil);
  end;
  if SelectRect.Top <> -1 then
  begin
    FFocusRect.Top := -1;
    SelectRect := FFocusRect;
    UpdateScreen;
    Repaint;
  end;
end;

procedure TAnsiEmulVT.LoadFromFile(FileName: string);
var
  F1: TextFile;
  s1: string;
  i: integer;
begin
  Screen.ClearScreen;
  AssignFile(F1, Filename);
  Reset(F1);
  while not EOF(F1) do
  begin
    ReadLn(F1, s1);
    WriteStr(s1);
    if Screen.FRow < Rows then 
    begin
      Inc(Screen.FRow);
      Screen.FCol := 0;
    end
    else 
      break;
  end;
  CloseFile(F1);
  UpdateScreen;
end;

procedure TAnsiEmulVT.SaveToFile(FileName: string);
var
  F1: TextFile;
  s1: string;
  i: integer;
begin
  AssignFile(F1, Filename);
  ReWrite(F1);
  for i := 0 to Rows - 1 do 
  begin
    s1 := AnsiCopyOneLine(i, 0, Cols - 1);
    WriteLn(F1, s1);
  end;
  CloseFile(F1);
end;


function TAnsiEmulVT.AttToEscChar(att: word; var bblink, bhigh: boolean;
  var fore, back: byte): string;
var
  b1, b2: byte;
  ss, ss2: string;
  bclear: boolean;
begin
  bclear := False;
  b1 := (att shr 4) and $0f;
  b2 := att and $0f;
  ss := #$1B + '[';
  if (att and X_UNDERLINE) <> 0 then ss2 := #$1B + '[4m'
  else 
    ss2 := '';
  if (b1 > 7) and (b2 > 7) then
  begin
    if bhigh then 
    begin
      if not bblink then ss := #$1B + '[5;';
    end
    else 
    begin
      if not bblink then ss := #$1B + '[5;1;'
      else 
        ss := #$1B + '[1;'
    end;
    bblink := True;
    bhigh := True;
    b1 := b1 and $07;
    b2 := b2 and $07;
  end
  else if (b1 > 7) then 
  begin
    if not bblink then ss := #$1B + '[5;';
    b1 := b1 and $07;
    bblink := True;
  end
  else if (b2 > 7) then 
  begin
    if bhigh then 
    begin
      if bblink then 
      begin
        ss := #$1B + '[0;1;';
        bclear := True;
      end
      else 
        ss := #$1B + '[';
    end
    else 
    begin
      if bblink then 
      begin
        ss := #$1B + '[0;1;';
        bclear := True;
      end
      else 
        ss := #$1B + '[1;';
    end;
    bhigh := True;
    bblink := False;
    b2 := b2 and $07;
  end
  else 
  begin
    if bhigh then 
    begin
      if bblink then ss := #$1B + '[0;'
      else 
        ss := #$1B + '[0;';
      bclear := True;
    end
    else 
    begin
      if bblink then 
      begin
        ss := #$1B + '[0;';
        bclear := True;
      end
      else 
        ss := #$1B + '[';
    end;
    bblink := False;
    bhigh := False;
  end;
  if (b1 <> 0) and (b2 <> 0) then 
  begin
    if (b1 <> back) or (bclear) then  ss := ss + IntToStr(40 + b1) + ';';
    back := b1;
    ss := ss + IntToStr(30 + b2) + 'm';
    fore := b2;
  end
  else if (b2 = 0) then 
  begin
    if bhigh then 
    begin
      ss := ss + IntToStr(40 + b1) + ';';
      ss := ss + IntToStr(30 + b2) + 'm';
    end
    else 
    begin
      ss := ss + IntToStr(40 + b1) + ';';
      ss := ss + IntToStr(30 + b2) + 'm';
    end;
    back := b1;
    fore := 0;
    //ss := ss + IntToStr(30 + b2) + 'm';
  end
  else if (b1 = 0) then 
  begin
    if back <> 0 then  ss := ss + '40';
    back := 0;
    if (Length(ss) > 0) and (ss[Length(ss)] = ';') then
      ss := Copy(ss, 1, Length(ss) - 1);
    if (b2 = fore) and (not bclear) then ss := ss + 'm'
    else 
    begin
      if ss[Length(ss)] = '[' then ss := ss + IntToStr(30 + b2) + 'm'
      else  
        ss := ss + ';' + IntToStr(30 + b2) + 'm';
    end;
    fore := b2;
    //ss := ss + IntToStr(40 + b1) + 'm';
  end;

  if (ss = #$1B + '[40;37m') and (not bblink) and (not bhigh) then ss := #$1B + '[m'
  else if (ss = #$1B + '[37m') and (back = 0) and (not bblink) and (not bhigh) then
    ss := #$1B + '[m';

  if ss = #$1B + '[m' then
  begin
    back := 0; 
    fore := 7;
    bhigh := False;
    bblink := False;
  end;
  if ss2 <> '' then ss := ss2 + ss;
  Result := ss;
end;


function TAnsiEmulVT.AnsiCopyOneLine(i, c1, c2: integer): string;
var
  j, js: integer;
  atts1, s: string;
  natt: word;
  bblink, bhigh: boolean;
  fore, back: byte;
begin
  j := c1;
  s := '';
  while (j <= c2) and (Screen.Lines[i].Att[j] = $07) do 
  begin
    s := s + Screen.Lines[i].Txt[j];
    Inc(j);
  end;

  if j >= c2 then
  begin
    {for js := c1 to c2 do
    begin
      s := s + TnEmulVT1.Screen.Lines[i].Txt[js];
    end;}
    s := TrimRight(s);
    Result := s;
    Exit;
  end;

  bblink := False;
  bhigh := False;
  back := 0;
  fore := 7;
  repeat
    js := j;
    nAtt := Screen.Lines[i].Att[j];
    atts1 := AttToEscChar(nAtt, bblink, bhigh, fore, back);
    s := s + atts1;
    while (nAtt = Screen.Lines[i].Att[js]) and (js <= c2) do 
    begin
      s := s + Screen.Lines[i].Txt[js];
      Inc(js);
    end;
    j := js;
  until j >= c2;

  if (SelectMode = smLine) and (back = 0) then s := TrimRight(s);

  s := s + #$1B + '[m';
  
  Result := s;
end;

procedure TAnsiEmulVT.CopyToClipBoard(bAnsi: boolean);
var
  r: TRect;
  i, j: integer;
  P: string;
  s: string;
  c1, c2, c3, r1, r2: integer;
begin
  if SelectRect.Top > 0 then
    r := SelectRect;

  if (r.Top > r.Bottom) then
  begin
    c1 := r.Left;
    c2 := r.Top;
    i := DrawLineHeight(1);
    r.Left := r.Right;
    r.Top := r.Bottom + i;
    r.Right := c1;
    r.Bottom := c2 + i;
  end;

  r1 := PixelToRow(r.Top);
  r2 := PixelToRow(r.Bottom - LineHeight + 1);
  if (r1 = r2) and (r.Left > r.Right) then
  begin
    c2 := PixelToCol(r.Left - 2);
    c1 := PixelToCol(r.Right);
  end
  else
  begin
    c1 := PixelToCol(r.Left);
    c2 := PixelToCol(r.Right - 2);
  end;
  P := '';
  if SelectMode = smLine then
  begin
    for i := r1 to r2 do
    begin
      s := '';
      if i > r1 then c1 := 0;
      if i = r2 then c3 := c2
      else
        c3 := Cols;
      if not bAnsi then
      begin
        for j := c1 to c3 do
        begin
          s := s + Screen.Lines[i + TopLine].Txt[j];
        end;
        s := TrimRight(s);
        //s := s + TnEmulVT1.Screen.Lines[i].Txt[j];
      end
      else
      begin
        s := AnsiCopyOneline(i, c1, c3);
      end;
      if r1 <> r2 then P := P + s + #13 + #10
      else
        P := P + s;
    end;
  end
  else if SelectMode = smBlock then
  begin
    for i := r1 to r2 do
    begin
      s := '';
      if not bAnsi then
      begin
        for j := c1 to c2 do
        begin
          s := s + Screen.Lines[i + TopLine].Txt[j];
        end;
        s := TrimRight(s);
        //s := s + TnEmulVT1.Screen.Lines[i].Txt[j];
      end
      else
      begin
        s := AnsiCopyOneline(i, c1, c2);
      end;
      if r1 <> r2 then P := P + s + #13 + #10
      else
        P := P + s;
    end;
  end;
  Clipboard.SetTextBuf(PChar(P));
end;

procedure TAnsiEmulVT.CutToClipBoard(bAnsi: boolean);
var
  r: TRect;
  i, j: integer;
  P: string;
  s: string;
  c1, c2, c3, r1, r2: integer;
begin
  if SelectRect.Top > 0 then
    r := SelectRect;

  if (r.Top > r.Bottom) then 
  begin
    c1 := r.Left;
    c2 := r.Top;
    i := DrawLineHeight(1);
    r.Left := r.Right;
    r.Top := r.Bottom + i;
    r.Right := c1;
    r.Bottom := c2 + i;
  end;

  r1 := PixelToRow(r.Top);
  r2 := PixelToRow(r.Bottom - LineHeight + 1);
  if (r1 = r2) and (r.Left > r.Right) then
  begin
    c2 := PixelToCol(r.Left - 2);
    c1 := PixelToCol(r.Right);
  end
  else
  begin
    c1 := PixelToCol(r.Left);
    c2 := PixelToCol(r.Right - 2);
  end;
  P := '';
  if SelectMode = smLine then
  begin
    for i := r1 to r2 do
    begin
      s := '';
      if i > r1 then c1 := 0;
      if i = r2 then c3 := c2
      else
        c3 := Cols;
      if not bAnsi then
      begin
        for j := c1 to c3 do
        begin
          s := s + Screen.Lines[i + TopLine].Txt[j];
          Screen.Lines[i + TopLine].Txt[j] := ' ';
          Screen.Lines[i + TopLine].Att[j] := 7;
        end;
        s := TrimRight(s);
        //s := s + TnEmulVT1.Screen.Lines[i].Txt[j];
      end
      else
      begin
        s := AnsiCopyOneline(i, c1, c3);
        for j := c1 to c3 do 
        begin
          Screen.Lines[i + TopLine].Txt[j] := ' ';
          Screen.Lines[i + TopLine].Att[j] := 7;
        end;
      end;
      if r1 <> r2 then P := P + s + #13 + #10
      else
        P := P + s;
    end;
  end
  else if SelectMode = smBlock then
  begin
    for i := r1 to r2 do
    begin
      s := '';
      if not bAnsi then
      begin
        for j := c1 to c2 do
        begin
          s := s + Screen.Lines[i + TopLine].Txt[j];
          Screen.Lines[i + TopLine].Txt[j] := ' ';
          Screen.Lines[i + TopLine].Att[j] := 7;
        end;
        s := TrimRight(s);
        //s := s + TnEmulVT1.Screen.Lines[i].Txt[j];
      end
      else
      begin
        s := AnsiCopyOneline(i, c1, c2);
        for j := c1 to c2 do 
        begin
          Screen.Lines[i + TopLine].Txt[j] := ' ';
          Screen.Lines[i + TopLine].Att[j] := 7;
        end;
      end;
      if r1 <> r2 then P := P + s + #13 + #10
      else
        P := P + s;
    end;
  end;
  Clipboard.SetTextBuf(PChar(P));
end;

procedure TAnsiEmulVT.Delete;
var
  r: TRect;
  i, j: integer;
  P: string;
  //s: string;
  c1, c2, c3, r1, r2: integer;
begin
  if SelectRect.Top > 0 then
    r := SelectRect;

  if (r.Top > r.Bottom) then 
  begin
    c1 := r.Left;
    c2 := r.Top;
    i := DrawLineHeight(1);
    r.Left := r.Right;
    r.Top := r.Bottom + i;
    r.Right := c1;
    r.Bottom := c2 + i;
  end;

  r1 := PixelToRow(r.Top);
  r2 := PixelToRow(r.Bottom - LineHeight + 1);
  if (r1 = r2) and (r.Left > r.Right) then
  begin
    c2 := PixelToCol(r.Left - 2);
    c1 := PixelToCol(r.Right);
  end
  else
  begin
    c1 := PixelToCol(r.Left);
    c2 := PixelToCol(r.Right - 2);
  end;
  P := '';
  if SelectMode = smLine then
  begin
    for i := r1 to r2 do
    begin
      if i > r1 then c1 := 0;
      if i = r2 then c3 := c2
      else
        c3 := Cols;
      for j := c1 to c3 do
      begin
        Screen.Lines[i + TopLine].Txt[j] := ' ';
        Screen.Lines[i + TopLine].Att[j] := 7;
      end;
    end;
  end
  else if SelectMode = smBlock then
  begin
    for i := r1 to r2 do
    begin
      for j := c1 to c2 do
      begin
        Screen.Lines[i + TopLine].Txt[j] := ' ';
        Screen.Lines[i + TopLine].Att[j] := 7;
      end;
    end;
  end;
end;

procedure TAnsiEmulVT.BrightSelection;
var
  r: TRect;
  i, j: integer;
  P: string;
  //s: string;
  c1, c2, c3, r1, r2: integer;
begin
  if SelectRect.Top > 0 then
    r := SelectRect;

  if (r.Top > r.Bottom) then
  begin
    c1 := r.Left;
    c2 := r.Top;
    i := DrawLineHeight(1);
    r.Left := r.Right;
    r.Top := r.Bottom + i;
    r.Right := c1;
    r.Bottom := c2 + i;
  end;

  r1 := PixelToRow(r.Top);
  r2 := PixelToRow(r.Bottom - LineHeight + 1);
  if (r1 = r2) and (r.Left > r.Right) then
  begin
    c2 := PixelToCol(r.Left - 2);
    c1 := PixelToCol(r.Right);
  end
  else
  begin
    c1 := PixelToCol(r.Left);
    c2 := PixelToCol(r.Right - 2);
  end;
  P := '';
  if SelectMode = smLine then
  begin
    for i := r1 to r2 do
    begin
      if i > r1 then c1 := 0;
      if i = r2 then c3 := c2
      else
        c3 := Cols;
      for j := c1 to c3 do
      begin
        Screen.Lines[i + TopLine].Att[j] :=
          Screen.Lines[i + TopLine].Att[j] or $08;
      end;
    end;
  end
  else if SelectMode = smBlock then
  begin
    for i := r1 to r2 do
    begin
      for j := c1 to c2 do
      begin
        Screen.Lines[i + TopLine].Att[j] :=
          Screen.Lines[i + TopLine].Att[j] or $08;
      end;
    end;
  end;
end;

procedure TAnsiEmulVT.DeBrightSelection;
var
  r: TRect;
  i, j: integer;
  P: string;
  //s: string;
  c1, c2, c3, r1, r2: integer;
begin
  if SelectRect.Top > 0 then
    r := SelectRect;

  if (r.Top > r.Bottom) then
  begin
    c1 := r.Left;
    c2 := r.Top;
    i := DrawLineHeight(1);
    r.Left := r.Right;
    r.Top := r.Bottom + i;
    r.Right := c1;
    r.Bottom := c2 + i;
  end;

  r1 := PixelToRow(r.Top);
  r2 := PixelToRow(r.Bottom - LineHeight + 1);
  if (r1 = r2) and (r.Left > r.Right) then
  begin
    c2 := PixelToCol(r.Left - 2);
    c1 := PixelToCol(r.Right);
  end
  else
  begin
    c1 := PixelToCol(r.Left);
    c2 := PixelToCol(r.Right - 2);
  end;
  P := '';
  if SelectMode = smLine then
  begin
    for i := r1 to r2 do
    begin
      if i > r1 then c1 := 0;
      if i = r2 then c3 := c2
      else
        c3 := Cols;
      for j := c1 to c3 do
      begin
        Screen.Lines[i + TopLine].Att[j] :=
          Screen.Lines[i + TopLine].Att[j] and $FFF7;
      end;
    end;
  end
  else if SelectMode = smBlock then
  begin
    for i := r1 to r2 do
    begin
      for j := c1 to c2 do
      begin
        Screen.Lines[i + TopLine].Att[j] :=
          Screen.Lines[i + TopLine].Att[j] and $FFF7;
      end;
    end;
  end;
end;

procedure TAnsiEmulVT.SetTextSelection(txt: string);
var
  r: TRect;
  i, j, j1, len: integer;
  c1, c2, c3, r1, r2: integer;
begin
  len := Length(txt);
  if len < 0 then Exit;

  r := SelectRect;
  AdjustRect(r);
  RectToRowCol(r, r1, r2, c1, c2);
  for i := r1 to r2 do
  begin
    j := c1;
    j1 := 1;
    while j < c2 do
    begin
      Screen.Lines[i + TopLine].Txt[j] := txt[j1];
      Inc(j1);
      Inc(j);
      if j1 > len then j1 := 1;
    end;
  end;
end;

procedure TAnsiEmulVT.SetAttrSelection(Att: word);
var
  r: TRect;
  i, j: integer;
  //P: string;
  //s: string;
  c1, c2, c3, r1, r2: integer;
begin
  if SelectRect.Top > 0 then
    r := SelectRect;

  if (r.Top > r.Bottom) then
  begin
    c1 := r.Left;
    c2 := r.Top;
    i := DrawLineHeight(1);
    r.Left := r.Right;
    r.Top := r.Bottom + i;
    r.Right := c1;
    r.Bottom := c2 + i;
  end;

  r1 := PixelToRow(r.Top);
  r2 := PixelToRow(r.Bottom - LineHeight + 1);
  if (r1 = r2) and (r.Left > r.Right) then
  begin
    c2 := PixelToCol(r.Left - 2);
    c1 := PixelToCol(r.Right);
  end
  else
  begin
    c1 := PixelToCol(r.Left);
    c2 := PixelToCol(r.Right - 2);
  end;
  //P := '';
  if SelectMode = smLine then
  begin
    for i := r1 to r2 do
    begin
      if i > r1 then c1 := 0;
      if i = r2 then c3 := c2
      else
        c3 := Cols;
      for j := c1 to c3 do
      begin
        Screen.Lines[i + TopLine].Att[j] := Att;
      end;
    end;
  end
  else if SelectMode = smBlock then
  begin
    for i := r1 to r2 do
    begin
      for j := c1 to c2 do
      begin
        Screen.Lines[i + TopLine].Att[j] := Att;
      end;
    end;
  end;
end;

procedure TAnsiEmulVT.SetText(x, y: integer; att: word; s1: string);
var
  i: integer;
begin
  for i := 1 to Length(s1) do 
  begin
    Screen.Lines[y].Att[x + i - 1] := Att;
    Screen.Lines[y].Txt[x + i - 1] := s1[i];
  end;
end;

procedure TAnsiEmulVT.DrawTableBorder(att: word);
var
  r: TRect;
  i: integer;
  c1, c2, c3, r1, r2: integer;
begin
  r := SelectRect;
  if r.Top < 0 then Exit;
  AdjustRect(r);
  RectToRowCol(r, r1, r2, c1, c2);
  if c2 - c1 < 3 then Exit;
  if r2 - r1 < 2 then Exit;

  Screen.Lines[r1].Txt[c1] := '+';
  Screen.Lines[r1].Txt[c2] := '+';
  Screen.Lines[r2].Txt[c1] := '+';
  Screen.Lines[r2].Txt[c2] := '+';
  Screen.Lines[r1].Att[c1] := att;
  Screen.Lines[r1].Att[c2] := att;
  Screen.Lines[r2].Att[c1] := att;
  Screen.Lines[r2].Att[c2] := att;
  for i := c1 + 1 to c2 - 1 do 
  begin
    Screen.Lines[r1].Txt[i] := '-';
    Screen.Lines[r2].Txt[i] := '-';
    Screen.Lines[r1].Att[i] := Att;
    Screen.Lines[r2].Att[i] := Att;
  end;
  for i := r1 + 1 to r2 - 1 do 
  begin
    Screen.Lines[i].Txt[c1] := '|';
    Screen.Lines[i].Txt[c2] := '|';
    Screen.Lines[i].Att[c1] := Att;
    Screen.Lines[i].Att[c2] := Att;
  end;
end;

procedure TAnsiEmulVT.DrawTableLine(att: word);
var
  r: TRect;
  i: integer;
  c1, c2, c3, r1, r2: integer;
begin
  r := SelectRect;
  if r.Top < 0 then Exit;
  AdjustRect(r);
  RectToRowCol(r, r1, r2, c1, c2);

  if (r2 - r1) > (c2 - c1) then 
  begin
    if r2 - r1 < 2 then Exit;
    for i := r1 + 1 to r2 - 1 do 
    begin
      Screen.Lines[i].Txt[c1] := '|';
      Screen.Lines[i].Att[c1] := Att;
    end
  end
  else 
  begin
    if c2 - c1 < 3 then Exit;
    for i := c1 + 1 to c2 - 1 do 
    begin
      Screen.Lines[r1].Txt[i] := '-';
      Screen.Lines[r1].Att[i] := Att;
    end;
  end;
end;

procedure TAnsiEmulVT.DrawCnTableBorder(att: word);
var
  r: TRect;
  i: integer;
  c1, c2, c3, r1, r2: integer;
begin
  r := SelectRect;
  if r.Top < 0 then Exit;
  AdjustRect(r);
  RectToRowCol(r, r1, r2, c1, c2);
  if c2 - c1 < 3 then Exit;
  if r2 - r1 < 2 then Exit;
  if ((c2 - c1) mod 2) = 0 then c2 := c2 + 1;

  Move('©³', Screen.Lines[r1].Txt[c1], 2);
  Move('©·', Screen.Lines[r1].Txt[c2 - 1], 2);
  Move('©»', Screen.Lines[r2].Txt[c1], 2);
  Move('©¿', Screen.Lines[r2].Txt[c2 - 1], 2);
  Screen.Lines[r1].Att[c1] := att;
  Screen.Lines[r1].Att[c2] := att;
  Screen.Lines[r2].Att[c1] := att;
  Screen.Lines[r2].Att[c2] := att;
  Screen.Lines[r1].Att[c1 + 1] := att;
  Screen.Lines[r1].Att[c2 - 1] := att;
  Screen.Lines[r2].Att[c1 + 1] := att;
  Screen.Lines[r2].Att[c2 - 1] := att;
  i := c1 + 2;
  while i < c2 - 2 do 
  begin
    Move('©¥', Screen.Lines[r1].Txt[i], 2);
    Move('©¥', Screen.Lines[r2].Txt[i], 2);
    Screen.Lines[r1].Att[i] := Att;
    Screen.Lines[r2].Att[i] := Att;
    Screen.Lines[r1].Att[i + 1] := Att;
    Screen.Lines[r2].Att[i + 1] := Att;
    Inc(i, 2);
  end;
  for i := r1 + 1 to r2 - 1 do 
  begin
    Move('©§', Screen.Lines[i].Txt[c1], 2);
    Move('©§', Screen.Lines[i].Txt[c2 - 1], 2);
    Screen.Lines[i].Att[c1] := Att;
    Screen.Lines[i].Att[c2] := Att;
    Screen.Lines[i].Att[c1 + 1] := Att;
    Screen.Lines[i].Att[c2 - 1] := Att;
  end;
end;

procedure TAnsiEmulVT.DrawCnTableLine(att: word);
var
  r: TRect;
  i, j: integer;
  c1, c2, c3, r1, r2: integer;
  ss1: array [0..5] of char;
begin
  r := SelectRect;
  if r.Top < 0 then Exit;
  AdjustRect(r);
  RectToRowCol(r, r1, r2, c1, c2);

  if (r2 - r1) > (c2 - c1) then 
  begin
    if r2 - r1 < 2 then Exit;
    for i := r1 to r2 do 
    begin
      if (Screen.Lines[i].Txt[c1] = ' ') and
        (Screen.Lines[i].Txt[c1 + 1] = ' ') then 
      begin
        Move('©¦', Screen.Lines[i].Txt[c1], 2);
        Screen.Lines[i].Att[c1] := Att;
        Screen.Lines[i].Att[c1 + 1] := Att;
      end
      else 
      begin
        Move(Screen.Lines[i].Txt[c1], ss1, 2);
        if StrPos(ss1, '©¤') <> nil then 
        begin
          Move('©à', Screen.Lines[i].Txt[c1], 2);
          Screen.Lines[r1].Att[c1] := Att;
          Screen.Lines[r1].Att[c1 + 1] := Att;
        end
        else if (i = r1) and (StrPos(ss1, '©¥') <> nil) then 
        begin
          Move('©Ó', Screen.Lines[i].Txt[c1], 2);
          Screen.Lines[r1].Att[c1] := Att;
          Screen.Lines[r1].Att[c1 + 1] := Att;
        end
        else if (i = r2) and (StrPos(ss1, '©¥') <> nil) then 
        begin
          Move('©Û', Screen.Lines[i].Txt[c1], 2);
          Screen.Lines[r1].Att[c1] := Att;
          Screen.Lines[r1].Att[c1 + 1] := Att;
        end
      end;
    end
  end
  else 
  begin
    if c2 - c1 < 3 then Exit;
{    i := c1;
    while (Screen.Lines[r1].Txt[i] <> ' ') do begin
      Inc(i);
    end;
    j := c2;
    while (Screen.Lines[r1].Txt[j] <> ' ') do begin
      Dec(j);
    end;
}
    i := c1;
    j := c2;
    while i < j do 
    begin
      if (Screen.Lines[r1].Txt[i] = ' ') and
        (Screen.Lines[r1].Txt[i + 1] = ' ') then 
      begin
        Move('©¤', Screen.Lines[r1].Txt[i], 2);
        Screen.Lines[r1].Att[i] := Att;
        Screen.Lines[r1].Att[i + 1] := Att;
        Inc(i, 2);
      end
      else 
      begin
        Move(Screen.Lines[r1].Txt[i], ss1, 2);
        if StrPos(ss1, '©¦') <> nil then 
        begin
          Move('©à', Screen.Lines[r1].Txt[i], 2);
          Screen.Lines[r1].Att[i] := Att;
          Screen.Lines[r1].Att[i + 1] := Att;
          Inc(i, 2);
        end
        else if (i = c1) and (StrPos(ss1, '©§') <> nil) then 
        begin
          Move('©Ä', Screen.Lines[r1].Txt[i], 2);
          Screen.Lines[r1].Att[i] := Att;
          Screen.Lines[r1].Att[i + 1] := Att;
          Inc(i, 2);
        end
        else if (i - c2 < 2) and (StrPos(ss1, '©§') <> nil) then 
        begin
          Move('©Ì', Screen.Lines[r1].Txt[i], 2);
          Screen.Lines[r1].Att[i] := Att;
          Screen.Lines[r1].Att[i + 1] := Att;
          Inc(i, 2);
        end
        else 
        begin
          Inc(i);
        end;
      end;
    end;
  end;
end;

end.
