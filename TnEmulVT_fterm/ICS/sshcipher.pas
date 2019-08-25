{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}

unit sshcipher;

interface

type

  TSSHCipher = class
  protected
    function Getcode: integer; virtual;
    function GetBlkSize: integer; virtual;
    function GetName: string; virtual;
    function GetKeyBits: integer; virtual;
  public
    procedure SetIV(Data: Pointer); virtual;
    procedure SetKey(Data: Pointer); virtual;
    procedure Encrypt(Source, Dest: Pointer; Len: integer); virtual;
    procedure Decrypt(Source, Dest: Pointer; Len: integer); virtual;
    property BlockSize: integer read GetBlkSize;
    property Name: string read GetName;
    property Code: integer read Getcode;
    property KeyBits: integer read GetKeyBits;
  end;

implementation

{ TSSHCipher }

procedure TSSHCipher.Decrypt(Source, Dest: Pointer; Len: integer);
begin
end;

procedure TSSHCipher.Encrypt(Source, Dest: Pointer; Len: integer);
begin
end;

function TSSHCipher.GetBlkSize: integer;
begin
  Result := 0;
end;

function TSSHCipher.Getcode: integer;
begin
  Result := 0;
end;

function TSSHCipher.GetKeyBits: integer;
begin
end;

function TSSHCipher.GetName: string;
begin
  Result := '';
end;

procedure TSSHCipher.SetIV(Data: Pointer);
begin
end;

procedure TSSHCipher.SetKey(Data: Pointer);
begin
end;

end.
