{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}

unit sshhash;

interface

type
  TSSHHash = class
  protected
    function GetDigLen: integer; virtual;
  public
    constructor Create; virtual;
    //   procedure Assign(Source:TSSHHash);virtual;
    procedure Update(Data: Pointer; Len: integer); virtual;
    procedure Final(Digest: Pointer); virtual;
    property DigestLength: integer read GetDigLen;
  end;

implementation

{ TSSHHash }
{
procedure TSSHHash.Assign(Source: TSSHHash);
begin

end;
}
constructor TSSHHash.Create;
begin
end;

procedure TSSHHash.Final(Digest: Pointer);
begin
end;

function TSSHHash.GetDigLen: integer;
begin
  Result := 0;
end;

procedure TSSHHash.Update(Data: Pointer; Len: integer);
begin
end;

end.
