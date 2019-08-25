{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}

unit sshmac;

interface

uses sshhash, sshsha;

type
  TSSH2MAC = class
  protected
    function GetMacSize: integer; virtual;
    function GetName: string; virtual;
    procedure GenMAC(Source: Pointer; Len: integer; Dest: Pointer; Sequence: integer);
      virtual;
  public
    procedure SetKey(Data: Pointer); virtual;
    procedure MakeMAC(Source: Pointer; Len: integer; Sequence: integer);
    function VerifyMAC(Source: Pointer; Len: integer; Sequence: integer): boolean;
    property MacSize: integer read GetMacSize;
    property Name: string read GetName;
  end;
  TSSH2MACSHA1 = class(TSSH2MAC)

  protected
    s1, s2: TSSHSHA1;
    procedure SHA1Key(var s1, s2: TSSHSHA1; Key: Pointer; Len: integer);
    function GetMacSize: integer; override;
    function GetName: string; override;
    procedure GenMAC(Source: Pointer; Len: integer; Dest: Pointer; Sequence: integer);
      override;
  public
    constructor Create;
    procedure SetKey(Data: Pointer); override;
  end;
  TSSH2MACSHA1Buggy = class(TSSH2MACSHA1)
  public
    procedure SetKey(Data: Pointer); override;
  end;

implementation

uses Math, Sysutils, WinSock;
{ TSSH2MAC }





procedure TSSH2MAC.GenMAC(Source: Pointer; Len: integer; Dest: Pointer;
  Sequence: integer);
begin
end;

function TSSH2MAC.GetMacSize: integer;
begin
  Result := 0;
end;

function TSSH2MAC.GetName: string;
begin
  Result := '';
end;

procedure TSSH2MAC.MakeMAC(Source: Pointer; Len, Sequence: integer);
begin
  GenMAC(Source, Len, PChar(Source) + Len, Sequence);
end;

procedure TSSH2MAC.SetKey(Data: Pointer);
begin
end;


function TSSH2MAC.VerifyMAC(Source: Pointer; Len,
  Sequence: integer): boolean;
var
  correct: array[0..19] of char;
begin
  GenMac(Source, Len, @correct, Sequence);
  Result := CompareMem(@correct, PChar(Source) + Len, 20);
end;

{ TSSH2MACSHA1 }

constructor TSSH2MACSHA1.Create;
begin
  s1 := TSSHSHA1.Create;
  s2 := TSSHSHA1.Create;
end;



procedure TSSH2MACSHA1.GenMAC(Source: Pointer; Len: integer;
  Dest: Pointer; Sequence: integer);
var
  s: TSSHSHA1;
  buf: array[0..19] of byte;
  i: integer;
begin
  i := ntohl(Sequence);
  s := TSSHSHA1.Create;
  s.Assign(s1);
  s.Update(@i, 4);
  s.Update(Source, Len);
  s.Final(@buf);
  s.Assign(s2);
  s.Update(@buf, 20);
  s.Final(Dest);
  s.Free;
end;

function TSSH2MACSHA1.GetMacSize: integer;
begin
  Result := 20;
end;

function TSSH2MACSHA1.GetName: string;
begin
  Result := 'hmac-sha1';
end;


procedure TSSH2MACSHA1.SetKey(Data: Pointer);
begin
  SHA1Key(s1, S2, Data, 20);
end;

procedure TSSH2MACSHA1.SHA1Key(var s1, s2: TSSHSHA1; Key: Pointer;
  Len: integer);
var
  foo: array[1..64] of byte;
  i: integer;
  keyptr: PChar;
begin
  Fillchar(foo, 64, $36);
  KeyPtr := Key;
  for i := 1 to min(64, len) do
    foo[i] := foo[i] xor Ord((KeyPtr + i - 1)^);
  S1.Free;
  S1 := TSSHSHA1.Create;
  s1.Update(@foo, 64);
  Fillchar(foo, 64, $5C);
  for i := 1 to min(64, len) do
    foo[i] := foo[i] xor Ord((KeyPtr + i - 1)^);
  S2.Free;
  S2 := TSSHSHA1.Create;  
  s2.Update(@foo, 64);
  Fillchar(foo, 64, 0);
end;

{ TSSH2MACSHA1Buggy }

procedure TSSH2MACSHA1Buggy.SetKey(Data: Pointer);
begin
  SHA1Key(s1, S2, Data, 16);
end;

end.
