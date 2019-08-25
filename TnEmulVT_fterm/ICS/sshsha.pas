unit sshsha;

interface

uses sshhash;

type
  SHA_CTX = packed record
    Data: array [0..23] of integer;
  end;
  PSHA_CTX = ^SHA_CTX;
  TSSHSHA1 = class(TSSHHash)
  protected
    sha1: SHA_CTX;
    function GetDigLen: integer; override;
  public
    constructor Create; override;
    procedure Assign(Source: TSSHSHA1); // for hmac use
    procedure Update(Data: Pointer; Len: integer); override;
    procedure Final(Digest: Pointer); override;
  end;
procedure SHA1_Init(c: PSHA_CTX); cdecl; external 'libeay32.dll';

procedure SHA1_Update(c: PSHA_CTX; Data: Pointer; Len: integer); cdecl;
  external 'libeay32.dll';

procedure SHA1_Final(Digest: Pointer; c: PSHA_CTX); cdecl; external 'libeay32.dll';

implementation

{ TSSHSHA1 }

procedure TSSHSHA1.Assign(Source: TSSHSHA1);
begin
  sha1 := Source.sha1;
end;

constructor TSSHSHA1.Create;
begin
  inherited;
  SHA1_Init(@sha1);
end;

procedure TSSHSHA1.Final(Digest: Pointer);
begin
  SHA1_Final(Digest, @sha1);
end;

function TSSHSHA1.GetDigLen: integer;
begin
  Result := 20;
end;

procedure TSSHSHA1.Update(Data: Pointer; Len: integer);
begin
  SHA1_Update(@sha1, Data, Len);
end;

end.
