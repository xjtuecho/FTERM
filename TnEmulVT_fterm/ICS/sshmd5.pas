{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}

unit sshmd5;

interface

uses SSHHash;

type
  MD5CTX = record
    Data: array[1..23] of integer;
  end;
  PMD5CTX = ^MD5CTX;
  TSSHMD5 = class(TSSHHash)
  protected
    md5: MD5CTX;
    function GetDigLen: integer; override;
  public
    constructor Create; override;
    procedure Update(Data: Pointer; Len: integer); override;
    procedure Final(Digest: Pointer); override;
    property DigestLength: integer read GetDigLen;
  end;
procedure MD5_Init(c: PMD5CTX); cdecl; external 'libeay32.dll';

procedure MD5_Update(c: PMD5CTX; Data: Pointer; Len: integer); cdecl;
  external 'libeay32.dll';

procedure MD5_Final(Data: Pointer; c: PMD5CTX); cdecl; external 'libeay32.dll';

implementation

{ TSSHMD5 }

constructor TSSHMD5.Create;
begin
  MD5_Init(@md5);
end;

procedure TSSHMD5.Final(Digest: Pointer);
begin
  MD5_Final(Digest, @md5);
end;

function TSSHMD5.GetDigLen: integer;
begin
  Result := 16;
end;

procedure TSSHMD5.Update(Data: Pointer; Len: integer);
begin
  MD5_Update(@md5, Data, Len);
end;

end.
