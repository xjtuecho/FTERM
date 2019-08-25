unit TPZFiles;
(* File manipulation routines for Turbo Pascal Zmodem *)
(* (c)1988 by J.R.Louvau                              *)

interface

uses Windows, SysUtils;

function Z_OpenFile(var f: file; pathname: string): boolean;
  (* Return true if able to open an existing file *)
function Z_MakeFile(var f: file; pathname: string): boolean;
  (* Return true if able to create a file *)
procedure Z_CloseFile(var f: file);
  (* Closes a file and ignores errors *)
function Z_SeekFile(var f: file; fpos: longint): boolean;
  (* Find a byte position within a file *)
function Z_WriteFile(var f: file; var buff; bytes: integer): boolean;
  (* Read a specified number of bytes from a file *)
function Z_ReadFile(var f: file; var buff; btoread: integer;
  var bread: integer): boolean;
  (* Search for a named file *)
function Z_FindFile(pathname: string; var Name: string;
  var size, time: longint): boolean;
  (* Set time and date of a file *)
procedure Z_SetFTime(var f: file; time: longint);

implementation

function Z_OpenFile(var f: file; pathname: string): boolean;
begin {$I-}
  AssignFile(f, pathname);
  Reset(f, 1);
  Z_OpenFile := (IOresult = 0)
end; {$I+}

function Z_MakeFile(var f: file; pathname: string): boolean;
begin {$I-}
  AssignFile(f, pathname);
  ReWrite(f, 1);
  Z_MakeFile := (IOresult = 0)
end; {$I+}

procedure Z_CloseFile(var f: file);
begin {$I-}
  CloseFile(f);
  if (IOresult <> 0) then
    { ignore this error }
end; {$I+}

function Z_SeekFile(var f: file; fpos: longint): boolean;
begin {$I-}
  Seek(f, fpos);
  Z_SeekFile := (IOresult = 0)
end; {$I+}

function Z_WriteFile(var f: file; var buff; bytes: integer): boolean;
var
  b1: integer;
begin {$I-}
  BlockWrite(f, buff, bytes, b1);
  Z_WriteFile := True; //(b1 = bytes)
end; {$I+}

function Z_ReadFile(var f: file; var buff; btoread: integer;
  var bread: integer): boolean;
begin {$I-}
  BlockRead(f, buff, btoread, bread);
  Z_ReadFile := True;// (bread > 0)
end; {$I+}

function Z_FindFile(pathname: string; var Name: string;
  var size, time: longint): boolean;
var
  sr: TSearchRec;
begin {$I-}
  if FindFirst(pathname, faArchive, sr) = 0 then 
  begin
    Name := sr.Name;
    size := sr.Size;
    time := sr.Time;
    Z_FindFile := True
  end
  else 
  begin
    Z_FindFile := False;
  end;
end; {$I+}

procedure Z_SetFTime(var f: file; time: longint);
begin {$I-}
  //SetFTime(f,time);
  if (IOresult <> 0) then
    {null}
end; {$I+}

end.
