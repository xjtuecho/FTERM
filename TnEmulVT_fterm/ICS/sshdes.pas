{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}

unit sshdes;

interface

uses sshcipher;

type
  DES_KEY = record
    cblock: array[0..191] of char;
  end;
  TSSHDES = class(TSSHCipher)
  protected
    function Getcode: integer; override;
    function GetBlkSize: integer; override;
    function GetName: string; override;
    function GetKeyBits: integer; override;
  private
    iv: array [0..7] of char;
    key: DES_KEY;
  public
    constructor Create;
    procedure SetIV(Data: Pointer); override;
    procedure SetKey(Data: Pointer); override;
    procedure Encrypt(Source, Dest: Pointer; Len: integer); override;
    procedure Decrypt(Source, Dest: Pointer; Len: integer); override;
  end;

  TSSHDES3 = class(TSSHCipher)
  protected
    function Getcode: integer; override;
    function GetBlkSize: integer; override;
    function GetName: string; override;
    function GetKeyBits: integer; override;
  private
    iv: array [0..7] of char;
    key1: DES_KEY;
    key2: DES_KEY;
    key3: DES_KEY;
  public
    constructor Create;
    procedure SetIV(Data: Pointer); override;
    procedure SetKey(Data: Pointer); override;
    procedure Encrypt(Source, Dest: Pointer; Len: integer); override;
    procedure Decrypt(Source, Dest: Pointer; Len: integer); override;
  end;
 
  TSSH1DES3 = class(TSSHCipher)
  protected
    function Getcode: integer; override;
    function GetBlkSize: integer; override;
    function GetName: string; override;
    function GetKeyBits: integer; override;
  private
    iv1: array [0..7] of char;
    iv2: array [0..7] of char;
    iv3: array [0..7] of char;
    key1: DES_KEY;
    key2: DES_KEY;
    key3: DES_KEY;
  public
    constructor Create;
    procedure SetIV(Data: Pointer); override;
    procedure SetKey(Data: Pointer); override;
    procedure Encrypt(Source, Dest: Pointer; Len: integer); override;
    procedure Decrypt(Source, Dest: Pointer; Len: integer); override;
  end;

 implementation

uses sshconst;

const
  DES_ENCRYPT = 1;
  DES_DECRYPT = 0;

procedure des_set_key(Data, Key: Pointer); cdecl; external 'libeay32.dll';

procedure des_ncbc_encrypt(Source, Dest: Pointer; Len: integer;
  Key, IV: Pointer; Enc: integer); cdecl; external 'libeay32.dll';

procedure des_ede3_cbc_encrypt(Source, Dest: Pointer; Len: integer;
  Key1, Key2, Key3, IV: Pointer; Enc: integer); cdecl; external 'libeay32.dll';
{ TSSHDES }

constructor TSSHDES.Create;
begin
  FillChar(key, sizeof(key), 0);
  Fillchar(iv, sizeof(iv), 0);
end;

procedure TSSHDES.Decrypt(Source, Dest: Pointer; Len: integer);
begin
  des_ncbc_encrypt(Source, Dest, Len, @Key, @iv, DES_DECRYPT);
end;

procedure TSSHDES.Encrypt(Source, Dest: Pointer; Len: integer);
begin
  des_ncbc_encrypt(Source, Dest, Len, @Key, @iv, DES_ENCRYPT);
end;

function TSSHDES.GetBlkSize: integer;
begin
  Result := 8;
end;

function TSSHDES.Getcode: integer;
begin
  Result := SSH_CIPHER_DES;
end;

function TSSHDES.GetKeyBits: integer;
begin
  Result := 56;
end;

function TSSHDES.GetName: string;
begin
  Result := 'des-cbc';
end;

procedure TSSHDES.SetIV(Data: Pointer);
begin
  if Data <> nil then
    Move(Data^, iv, 8)
  else 
    Fillchar(IV, Sizeof(IV), 0);
end;

procedure TSSHDES.SetKey(Data: Pointer);
begin
  des_set_key(Data, @key);
end;

{ TSSHDES3 }

constructor TSSHDES3.Create;
begin
  Fillchar(iv, sizeof(iv), 0);
  Fillchar(key1, sizeof(key1), 0);
  Fillchar(key2, sizeof(key1), 0);
  Fillchar(key3, sizeof(key1), 0);
end;

procedure TSSHDES3.Decrypt(Source, Dest: Pointer; Len: integer);
begin
  des_ede3_cbc_encrypt(Source, Dest, Len, @Key1, @Key2, @Key3, @IV, DES_DECRYPT);
end;

procedure TSSHDES3.Encrypt(Source, Dest: Pointer; Len: integer);
begin
  des_ede3_cbc_encrypt(Source, Dest, Len, @Key1, @Key2, @Key3, @IV, DES_ENCRYPT);
end;

function TSSHDES3.GetBlkSize: integer;
begin
  Result := 8;
end;

function TSSHDES3.Getcode: integer;
begin
  Result := SSH_CIPHER_3DES;
end;

function TSSHDES3.GetKeyBits: integer;
begin
  Result := 168;
end;

function TSSHDES3.GetName: string;
begin
  Result := '3des-cbc';
end;

procedure TSSHDES3.SetIV(Data: Pointer);
begin
  if Data <> nil then
    Move(Data^, IV, sizeof(IV))
  else 
    Fillchar(IV, Sizeof(IV), 0);
end;

procedure TSSHDES3.SetKey(Data: Pointer);
var
  P: PChar;
begin
  P := Data;
  des_set_key(P, @Key1);
  Inc(P, 8);
  des_set_key(P, @Key2);
  Inc(P, 8);
  des_set_key(P, @Key3);
end;

{ TSSH1DES3 }

constructor TSSH1DES3.Create;
begin
  Fillchar(iv1, sizeof(iv1), 0);
  Fillchar(iv2, sizeof(iv2), 0);
  Fillchar(iv3, sizeof(iv3), 0);
  Fillchar(key1, sizeof(key1), 0);
  Fillchar(key2, sizeof(key1), 0);
  Fillchar(key3, sizeof(key1), 0);
end;

procedure TSSH1DES3.Decrypt(Source, Dest: Pointer; Len: integer);
begin
  des_ncbc_encrypt(Source, Dest, len, @key3, @iv3, DES_DECRYPT);
  des_ncbc_encrypt(dest, dest, len, @key2, @iv2, DES_ENCRYPT);
  des_ncbc_encrypt(dest, dest, len, @key1, @iv1, DES_DECRYPT);
end;

procedure TSSH1DES3.Encrypt(Source, Dest: Pointer; Len: integer);
begin
  des_ncbc_encrypt(Source, Dest, len, @key1, @iv1, DES_ENCRYPT);
  des_ncbc_encrypt(dest, dest, len, @key2, @iv2, DES_DECRYPT);
  des_ncbc_encrypt(dest, dest, len, @key3, @iv3, DES_ENCRYPT);
end;

function TSSH1DES3.GetBlkSize: integer;
begin
  Result := 8;
end;

function TSSH1DES3.Getcode: integer;
begin
  Result := SSH_CIPHER_3DES;
end;

function TSSH1DES3.GetKeyBits: integer;
begin
  Result := 16;
end;

function TSSH1DES3.GetName: string;
begin
  Result := 'des3-cbc';
end;

procedure TSSH1DES3.SetIV(Data: Pointer);
begin
  Fillchar(IV1, Sizeof(IV1), 0);
  Fillchar(IV2, Sizeof(IV1), 0);
  Fillchar(IV3, Sizeof(IV1), 0);
end;

procedure TSSH1DES3.SetKey(Data: Pointer);
var
  P: PChar;
begin
  P := Data;
  des_set_key(P, @Key1);
  Inc(P, 8);
  des_set_key(P, @Key2);
  Inc(P, 8);
  des_set_key(P, @Key3);
end;

end.
