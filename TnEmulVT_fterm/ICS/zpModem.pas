unit zpModem;

interface

uses
  Windows, Messages, SysUtils, Classes, ExtCtrls, TPZFiles, zstatdlg;


type
  THDRBuf = array[0..3] of byte;
  TDATABuf = array[0..4096] of byte;
  PDataBuf = ^TDatabuf;
const
  ZBUFSIZE = 4096;

type
  TZmodemState = (
    {Transmit states}
    tzInitial,       {Allocates buffers, sends zrqinit}
    tzHandshake,     {Wait for hdr (zrinit), rsend zrqinit on timout}
    tzGetFile,       {Call NextFile, build ZFile packet}
    tzSendFile,      {Send ZFile packet}
    tzCheckFile,     {Wait for hdr (zrpos), set next state to tzData}
    tzStartData,     {Send ZData and next data subpacket}
    tzEscapeData,    {Check for header, escape next block}
    tzSendData,      {Wait for free space in buffer, send escaped block}
    tzWaitAck,       {Wait for Ack on ZCRCW packets}
    tzSendEof,       {Send eof}
    tzDrainEof,      {Wait for output buffer to drain}
    tzCheckEof,      {Wait for hdr (zrinit)}
    tzSendFinish,    {Send zfin}
    tzCheckFinish,   {Wait for hdr (zfin)}
    tzError,         {Cleanup after errors}
    tzCleanup,       {Release buffers and other cleanup}
    tzDone,          {Signal end of protocol}

    {Receive states}
    rzRqstFile,      {Send zrinit}
    rzDelay,         {Delay handshake for Telix}
    rzWaitFile,      {Waits for hdr (zrqinit, zrfile, zsinit, etc)}
    rzCollectFile,   {Collect file info into work block}
    rzSendInit,      {Extract send init info}
    rzSendBlockPrep, {Discard last two chars of previous hex packet}
    rzSendBlock,     {Collect sendinit block}
    rzSync,          {Send ZrPos with current file position}
    rzStartFile,     {Extract file info, prepare writing, etc., put zrpos}
    rzStartData,     {Wait for hdr (zrdata)}
    rzCollectData,   {Collect data subpacket}
    rzGotData,       {Got dsp, put it}
    rzWaitEof,       {Wait for hdr (zreof)}
    rzEndOfFile,     {Close file, log it, etc}
    rzSendFinish,    {Send ZFin, goto rzWaitOO}
    rzCollectFinish, {Check for OO, goto rzFinish}
    rzError,         {Handle errors while file was open}
    rzWaitCancel,    {Wait for the cancel to leave the outbuffer}
    rzCleanup,       {Clean up buffers, etc.}
    rzDone);          {Signal end of protocol}

  TProtocolStatus = (psInit, psHeader, psData, psCancelRequested);
  TZDLEStatus = (zsData, zsCRC, zsGotCRCE, zsGotCrcG, zsGotCrcQ, zsGotCrcW);
  THeaderState = (hsInit, hsZPAD, hsZDLE, hsFMT, hsDONE);
  TBlockState = (bsInit, bsData, bsCRC, bsDONE);

  TzpModem = class(TObject)
  private
    { Private declarations }
    HexByte1, HexVale: integer;
    bHexPending: boolean;
    ZMessage: string;
    { Receive related }
    rxhdr, txhdr: THDRBuf;
    nbibi, zCanCount: integer;
    zEscapePending: boolean;
    zControlCharSkip: boolean;
    LastPullByte : Byte;
    IACEscape : Boolean;
    zHeaderFomat: integer;
    aHeaderData: array[0..18] of byte;
    rzHeaderType: byte;
    iHeaderCnt: integer;
    aHeaderFomat: byte;
    aBlockStatus: TBlockState;
    iBlockData: integer;
    nCRCBytes: integer;
    aBlockData: TDATABuf;
    dwDataCRC: DWORD;
    aZDLEStatus: TZDLEStatus;
    aBlockZDLE: TZDLEStatus;
    { process save file }
    rxbytes: longint;
    fsize: integer;
    ftime: integer;
    filestart: longint;
    outfile: file;     {this is the file}
    bFileOpen: boolean;
    fname: string;
    lastbyte: byte;
    starttime: TDateTime;
    OOCount: integer;
    bCharEscaped: boolean;
    fdownloadpath: string;
    {}
    lastsent: byte;
    txpos, rxpos: longint;
    rxbuflen: integer;
    usecrc32: boolean;
    txbuf: TDATABuf;
    infile: file;
    blklen, blkred, maxblklen, newcnt: integer;
    bUseCRC32: boolean;
    zpCanceled: boolean;
    zEscapeAll: boolean;
    { Timer Trigger }
    aTimer: TTimer;
    procedure TransmitEvent(Sender: TObject);
    procedure ClearTransmitTimer;

    function zParseHdr(b: byte): THeaderState;
    function zParseData(b: byte): TBlockState;
    function zParseData16(b: byte): TBlockState;
    function zParseData32(b: byte): TBlockState;
    function zpGotCancel: boolean;
    function PullStripChar(var C: byte): boolean;
    function PullEscapeChar(var C: byte): boolean;
    function PullHex(var C: byte): boolean;
    function GetHexFromHeaderData(ofs: integer): byte;
    function CheckUpZBIN32Header: boolean;
    function CheckUpZBINHeader: boolean;
    function CheckUpHexHeader: boolean;
    function Z_PullLongFromHeader(var hdr: THDRBuf): longint;
    procedure PrepareHeader;
    procedure PrepareData;
    function ClearRestByte: boolean;
    function zParseOO(b: byte): boolean;
    { process save file }
    function RZ_GetHeader: integer;
    function RZ_SaveToDisk(var rxbytes: longint): integer;
    { send }
    procedure Z_SendByte(b: byte);
    procedure Z_SendBuf(buf: Pointer; len: integer);
    procedure Z_PutHex(b: byte);
    procedure Z_PutLongIntoHeader(l: longint);
    procedure Z_SendHexHeader(htype: byte; var hdr: THDRBuf);
    procedure RZ_AckBibi;
    procedure Z_SendCan;
    { Transmit routine }
    function SZ_PrepareFile: boolean;
    procedure SZ_Z_SendByte(b: byte);
    function Z_FileCRC32(var f: file): longint;
    procedure SZ_SendBinaryHead32(htype: byte; var hdr: THDRBuf);
    procedure SZ_SendBinaryHeader(htype: byte; var hdr: THDRBuf);
    procedure SZ_SendData(var buf: TDATABuf; blength: integer; frameend: byte);
    procedure SZ_SendDa32(var buf: TDATABuf; blength: integer; frameend: byte);
    procedure SZ_EndSend;
    procedure SZ_UpdateStatus;
    { Timer Trigger }
    procedure SetupTransmitTimer;
  protected
    { Protected declarations }
    bReceiving: boolean;
    aProtocolStatus: TProtocolStatus;
    aHeaderStatus: THeaderState;
    curByte, curLen: integer;
    zstatfrm: Tzstatfrm;
  public
    TnCnx1: TComponent;
    zZModemState: TZmodemState;
    statform: Tzstatfrm;
    uploadfilename: string;
    constructor Create(ATnCnx: TComponent);
    destructor Destory;
    procedure IniStatus;
    procedure UpdateStatus;
    function zParseReceive(b: byte): TZmodemState;
    function zParseTransmit(b: byte): TZmodemState;
    procedure InitParser;
    procedure PrepareReceive;
    procedure PrepareTransmit;
    function ProcessZModemRecevive(Data: PChar; Len: integer): boolean;
    function ProcessZModemTransmit(Data: PChar; Len: integer): boolean;
  published
    property DownloadPath: string read FDownloadPath write FDownloadPath;
  end;

  {-$DEFINE LOGSENT}
  {-$DEFINE LOGRECV}

implementation

uses  Forms, Dialogs, DateUtils, TnCnx, TPZunix, TPZcrc;

const
  ZPAD = 42;  { '*' }
  ZDLE = 24;  { ^X  }
  ZDLEE = 88;
  ZBIN = 65;  { 'A' }
  ZHEX = 66;  { 'B' }
  ZBIN32 = 67;{ 'C' }
  ZRQINIT = 0;
  ZRINIT = 1;
  ZSINIT = 2;
  ZACK = 3;
  ZFILE = 4;
  ZSKIP = 5;
  ZNAK = 6;
  ZABORT = 7;
  ZFIN = 8;
  ZRPOS = 9;
  ZDATA = 10;
  ZEOF = 11;
  ZFERR = 12;
  ZCRC = 13;
  ZCHALLENGE = 14;
  ZCOMPL = 15;
  ZCAN = 16;
  ZFREECNT = 17;
  ZCOMMAND = 18;
  ZSTDERR = 19;
  ZCRCE = 104; { 'h' }
  ZCRCG = 105; { 'i' }
  ZCRCQ = 106; { 'j' }
  ZCRCW = 107; { 'k' }
  ZRUB0 = 108; { 'l' }
  ZRUB1 = 109; { 'm' }
  ZOK = 0;
  ZERROR = -1;
  ZTIMEOUT = -2;
  RCDO = -3;
  FUBAR = -4;
  GOTOR = 256;
  GOTCRCE = 360; { 'h' OR 256 }
  GOTCRCG = 361; { 'i' "   "  }
  GOTCRCQ = 362; { 'j' "   "  }
  GOTCRCW = 363; { 'k' "   "  }
  GOTCAN = 272;  { CAN OR  "  }

  { xmodem paramaters }
const
  ENQ = 5;
  CAN = 24;
  XOFF = 19;
  XON = 17;
  SOH = 1;
  STX = 2;
  EOT = 4;
  ACK = 6;
  NAK = 21;
  CPMEOF = 26;

  { byte positions }
const
  ZF0 = 3;
  ZF1 = 2;
  ZF2 = 1;
  ZF3 = 0;
  ZP0 = 0;
  ZP1 = 1;
  ZP2 = 2;
  ZP3 = 3;

  { bit masks for ZRINIT }
const
  CANFDX = 1;    { can handle full-duplex          (yes for PC's)}
  CANOVIO = 2;   { can overlay disk and serial I/O (ditto)       }
  CANBRK = 4;    { can send a break - True but superfluous       }
  CANCRY = 8;    { can encrypt/decrypt - not defined yet         }
  CANLZW = 16;   { can LZ compress - not defined yet             }
  CANFC32 = 32;  { can use 32 bit crc frame checks - true        }
  ESCALL = 64;   { escapes all control chars. NOT implemented    }
  ESC8 = 128;    { escapes the 8th bit. NOT implemented          }

  { paramaters for ZFILE }
const
  { ZF0 }
  ZCBIN = 1;
  ZCNL = 2;
  ZCRESUM = 3;
  { ZF1 }
  ZMNEW = 1;   {I haven't implemented these as of yet - most are}
  ZMCRC = 2;   {superfluous on a BBS - Would be nice from a comm}
  ZMAPND = 3;  {programs' point of view however                 }
  ZMCLOB = 4;
  ZMSPARS = 5;
  ZMDIFF = 6;
  ZMPROT = 7;
  { ZF2 }
  ZTLZW = 1;   {encryption, compression and funny file handling }
  ZTCRYPT = 2; {flags - My docs (03/88) from OMEN say these have}
  ZTRLE = 3;   {not been defined yet                            }
  { ZF3 }
  ZCACK1 = 1;  {God only knows...                               }


  {$IFDEF LOGRECV}
var
  rlogfile: file of byte;

procedure ROpenLog;
begin
  AssignFile(rlogfile, 'rzmodem.log');
  Rewrite(rlogfile);
end;

procedure RLogBuf(buf: PChar; Len: integer);
var
  i: integer;
begin
  //Write(rlogfile, b);
  //BlockWrite(rlogfile, buf, len);
  for i := 0 to Len - 1 do 
  begin
    Write(rlogfile, byte((buf + i)^));
  end;
end;

procedure RCloseLog;
begin
  CloseFile(rlogfile);
end;
{$ENDIF}

{$IFDEF LOGSENT}
var
  slogfile: file of byte;

procedure SOpenLog;
begin
  AssignFile(slogfile, 'szmodem.log');
  Rewrite(slogfile);
end;

procedure SLogByte(b: byte);
begin
  Write(slogfile, b);
end;
{$ENDIF}
  
constructor TzpModem.Create(ATnCnx: TComponent);
begin
  inherited Create;
  TnCnx1 := ATnCnx;
  statform := Tzstatfrm.Create(Application);
  InitParser;
end;

destructor TzpModem.Destory;
begin
  ClearTransmitTimer;
  if Assigned(statform) then statform.Close;
  inherited;
end;

procedure TzpModem.InitParser;
begin
  {$IFDEF LOGRECV}
  ROpenLog;
  {$ENDIF}
  aProtocolStatus := psHeader;
  nbibi := 4;
  zCanCount := 0;
  aProtocolStatus := psInit;
  zEscapePending := False;
  zControlCharSkip := False;
  bHexPending := False;

  rxbytes := 0;
  fsize := 99999;
  fname := 'noname';
  
  aHeaderStatus := hsInit;
  aBlockStatus := bsInit;
  statform.bCancel := False;
  zpCanceled := False;
  zEscapeAll := False;

  { IAC initial }
  IACEscape := not (TnCnx1 as TTnCnx).IsSSH;
end;

function TzpModem.zpGotCancel: boolean;
begin
  Inc(zCanCount);
  if zCanCount >= 5 then
  begin
    aProtocolStatus := psCancelRequested;
    //aForceStatus := True;
    if bReceiving then
      zZmodemState := rzDone
    else
      zZmodemState := tzDone;
    ClearRestByte;
    zpGotCancel := True;
  end 
  else
    zpGotCancel := False;
end;


function TzpModem.PullHex(var C: byte): boolean;
var
  n: integer;
begin
  if (C and $7f) in [17, 19] then
  begin
    {unescaped control char, ignore it}
    zControlCharSkip := True;
    Result := False;
    Exit;
  end;

  n := C;

  n := n - $30;                     {build the high nybble}
  if (n > 9) then
    n := n - 39;
  if bHexPending then 
  begin
    HexVale := (HexByte1 shl 4) or n;
    bHexPending := False;
    Result := True;
  end
  else 
  begin
    HexByte1 := n;
    bHexPending := True;
    Result := False;
  end;
end;

function TzpModem.PullStripChar(var C: byte): boolean;
begin
  if IACEscape and (LastPullByte=$FF) and (C=$FF) then begin
    LastPullByte := 0;
    Result := False;
    Exit;
  end
  else if IACEscape and (LastPullByte=13) and (C=0) then begin
    LastPullByte := 0;
    Result := False;
    Exit;
  end;

  LastPullByte := C;
  
  if (C and $7f) in [17, 19] then
  begin
    {unescaped control char, ignore it}
    zControlCharSkip := True;
    Result := False;
    Exit;
  end;

  Result := False;
  {If not data link escape or cancel then just return the character}
  if (C <> ZDLE) then 
  begin
    Result := True;
    zCanCount := 0;
    Exit;
  end
  else if zpGotCancel then 
  begin
    {Got 5 cancels, ZDle's, in a row}
    Result := True;
    Exit;
  end
  else 
  begin
    Result := True;
  end;
end;

function TzpModem.PullEscapeChar(var C: byte): boolean;
  { return True when 1 Byte escaped char is output      }
  {false: 17,19 ignore, ZDLE->zEscapePending, ZDLE+CAN  }
  {true: 5 CAN, ZDLE Char, others...                    }
label
  Escape;
begin
  {Go get escaped char if we already have the escape}
  if zEscapePending then
    goto Escape;

  if IACEscape and (LastPullByte=$FF) and (C=$FF) then begin
    LastPullByte := 0;
    Result := False;
    Exit;
  end
  else if IACEscape and (LastPullByte=13) and (C=0) then begin
    LastPullByte := 0;
    Result := False;
    Exit;
  end;

  LastPullByte := C;

  if (C and $7f) in [17, 19] then
  begin
    {unescaped control char, ignore it}
    zControlCharSkip := True;
    Result := False;
    Exit;
  end;

  bCharEscaped := False;
  Result := False;
  {If not data link escape or cancel then just return the character}
  if (C <> ZDLE) then
  begin
    Result := True;
    aZDLEStatus := zsData;
    zCanCount := 0;
    Exit;
  end
  else if zpGotCancel then 
  begin
    {Got 5 cancels, ZDle's, in a row}
    Result := True;
    Exit;
  end
  else 
  begin
    Result := False;
    zEscapePending := True;
    Exit;
  end;

  Escape:
  zEscapePending := False;

  bCharEscaped := True;
  aZDLEStatus := zsData;
  {If cancelling make sure we get at least 5 of them}
  if (C = CAN) then 
  begin
    Result := False;
    zpGotCancel;
    Exit;
  end
  else 
  begin
    {Must be an escaped character}
    Result := True;
    zCanCount := 0;
    case C of
      ZCrcE: {Last DataSubpacket of file}
        aZDLEStatus := zsGotCrcE;
      ZCrcG: {Normal DataSubpacket, no response necessary}
        aZDLEStatus := zsGotCrcG;
      ZCrcQ: {ZAck or ZrPos requested}
        aZDLEStatus := zsGotCrcQ;
      ZCrcW: {DataSubpacket contains file information}
        aZDLEStatus := zsGotCrcW;
      ZRub0:         {Ascii delete}
        C := $7F;
      ZRub1:         {Hibit Ascii delete}
        C := $FF;
      else            {Normal escaped character}
        C := byte(Ord(C) xor $40)
    end;
  end;
end;

function TzpModem.CheckUpZBIN32Header: boolean;
var
  crc: DWORD;
  c: byte;
  i: integer;
begin
  rzHeaderType := aHeaderData[0];
  crc := UpdC32(rzHeaderType, $FFFFFFFF);

  for i := 1 to 4 do
  begin
    c := aHeaderData[i];
    rxhdr[i - 1] := c;
    crc := UpdC32(c, crc)
  end;
  for i := 5 to 8 do
  begin
    c := aHeaderData[i];
    crc := UpdC32(c, crc)
  end;

  Result := False;
  if (crc <> $DEBB20E3) then   {this is the polynomial value}
  begin
    Result := False;
  end;
end;

function TzpModem.CheckUpZBINHeader: boolean;
var
  crc: DWORD;
  c: byte;
  i: integer;
begin
  rzHeaderType := aHeaderData[0];
  crc := UpdCrc(rzHeaderType, 0);

  for i := 1 to 4 do
  begin
    c := aHeaderData[i];
    rxhdr[i - 1] := c;
    crc := UpdCrc(c, crc);
  end;
  for i := 5 to 8 do
  begin
    c := aHeaderData[i];
    crc := UpdCrc(c, crc);
  end;

  Result := False;
  if (crc <> 0) then   {this is the polynomial value}
  begin
    Result := False;
  end;
end;

function TzpModem.GetHexFromHeaderData(ofs: integer): byte;
var
  n, c: integer;
begin
  n := aHeaderData[ofs];
  n := n - $30;                     {build the high nybble}
  if (n > 9) then
    n := n - 39;
  c := aHeaderData[ofs + 1];
  c := c - $30;                     {now the low nybble}
  if (c > 9) then
    c := c - 39;
  Result := byte((n shl 4) or c);
end;

function TzpModem.CheckUpHexHeader: boolean;
var
  crc: word;
  c: byte;
  i: integer;
begin
  rzHeaderType := GetHexFromHeaderData(0);
  crc := UpdCrc(rzHeaderType, 0);

  for i := 1 to 4 do
  begin
    c := GetHexFromHeaderData(i * 2);
    rxhdr[i - 1] := c;
    crc := UpdCrc(c, crc);
  end;
  for i := 5 to 6 do
  begin
    c := GetHexFromHeaderData(i * 2);
    crc := UpdCrc(c, crc);
  end;

  Result := False;
  if (crc <> 0) then   {this is the polynomial value}
  begin
    Result := False;
  end;
end;

function TzpModem.Z_PullLongFromHeader(var hdr: THDRBuf): longint;
type
  longarray = array [0..3] of byte;
var
  l: longint;
  longptr: longarray absolute l;
begin
  longptr[0] := hdr[ZP0];
  longptr[1] := hdr[ZP1];
  longptr[2] := hdr[ZP2];
  longptr[3] := hdr[ZP3];

  Z_PullLongFromHeader := l
end;

function TzpModem.zParseHdr(b: byte): THeaderState;
begin
  case aHeaderStatus of
    hsInit: 
      begin
        if PullStripChar(b) then 
        begin
          if b = ZPAD then 
          begin
            aHeaderStatus := hsZPAD
          end;
        end;
      end;
    hsZPAD: 
      begin
        if PullStripChar(b) then 
        begin
          if b = ZPAD then Exit;
          if b = ZDLE then 
          begin
            aHeaderStatus := hsZDLE
          end;
        end;
      end;
    hsZDLE: 
      begin
        if PullStripChar(b) then 
        begin
          aHeaderStatus := hsFMT;
          case b of
            ZBIN32,
            ZBIN,
            ZHEX: 
              begin
                aHeaderFomat := b;
                iHeaderCnt := 0;
              end;
          end;
        end;
      end;
    hsFMT: 
      begin
        if PullEscapeChar(b) then 
        begin
          if aHeaderFomat = ZBIN32 then 
          begin
            aHeaderData[iHeaderCnt] := b;
            Inc(iHeaderCnt);
            if iHeaderCnt > 8 then aHeaderStatus := hsDONE;
            {type, P1, P2, P3, P4}
          end
          else if aHeaderFomat = ZBIN then 
          begin
            aHeaderData[iHeaderCnt] := b;
            Inc(iHeaderCnt);
            if iHeaderCnt > 6 then aHeaderStatus := hsDONE;
            {type, P1, P2, P3, P4}
          end
          else if aHeaderFomat = ZHEX then
          begin
            aHeaderData[iHeaderCnt] := b;
            Inc(iHeaderCnt);
            if iHeaderCnt > 14 then aHeaderStatus := hsDONE;
            {TYPE*2, P1*2, P2*2, P3*2, P4*2, CRC1*2, CRC2*2}
          end;
          if aHeaderStatus = hsDONE then 
          begin
            case aHeaderFomat of
              ZBIN32: CheckUpZBIN32Header;
              ZBIN: CheckUpZBINHeader;
              ZHEX: CheckUpHexHeader
            end;
          end;
        end;
      end;
    hsDONE: 
      begin
        rxpos := Z_PullLongFromHeader(rxhdr);
      end;
  end;
  Result := aHeaderStatus;
end;

function TzpModem.zParseData16(b: byte): TBlockState;
begin
  if iBlockData = 0 then
  begin
    dwDataCRC := 0;
    aBlockStatus := bsData;
  end;
  if PullEscapeChar(b) then 
  begin
    if aBlockStatus = bsCRC then 
    begin
      if aZDLEStatus <> zsData then
      begin
        nCRCBytes := 2;
      end;
      dwDataCRC := UpdCRC(b, dwDataCRC);
      Dec(nCRCBytes);
      if nCRCBytes = 0 then 
      begin
        if (dwDataCRC <> 0) then
        begin
          aBlockStatus := bsCRC;
          // CRC error;
        end;
        aBlockStatus := bsDone;
      end;
    end
    else 
    begin
      case aZDLEStatus of
        zsGotCRCE,
        zsGotCrcG,
        zsGotCrcQ,
        zsGotCrcW: 
          begin
            dwDataCRC := UpdCRC(b, dwDataCRC);
            aBlockStatus := bsCRC;
            aBlockZDLE := aZDLEStatus;
            nCRCBytes := 2;
          end;
        zsData:
          begin
            aBlockData[iBlockData] := b;
            Inc(iBlockData);
            dwDataCRC := UpdCRC(b, dwDataCRC);
          end;
      end;
    end;
    lastbyte := b;
    //if (b = 13) and (bCharEscaped) then lastbyte := 0;
  end;
  Result := aBlockStatus;
end;

function TzpModem.zParseData32(b: byte): TBlockState;
begin
  if iBlockData = 0 then 
  begin
    dwDataCRC := $FFFFFFFF;
    aBlockStatus := bsData;
  end;
  if PullEscapeChar(b) then 
  begin
    if aBlockStatus = bsCRC then 
    begin
      if aZDLEStatus <> zsData then 
      begin
        nCRCBytes := 4;
      end;
      dwDataCRC := UpdC32(b, dwDataCRC);
      Dec(nCRCBytes);
      if nCRCBytes = 0 then 
      begin
        if (dwDataCRC <> $DEBB20E3) then 
        begin
          aBlockStatus := bsCRC;
          // CRC error;
        end;
        aBlockStatus := bsDone;
      end;
    end
    else 
    begin
      case aZDLEStatus of
        zsGotCRCE,
        zsGotCrcG,
        zsGotCrcQ,
        zsGotCrcW: 
          begin
            dwDataCRC := UpdC32(b, dwDataCRC);
            aBlockStatus := bsCRC;
            aBlockZDLE := aZDLEStatus;
            nCRCBytes := 4;
          end;
        zsData: 
          begin
            aBlockData[iBlockData] := b;
            Inc(iBlockData);
            dwDataCRC := UpdC32(b, dwDataCRC);
          end;
      end;
    end;
    lastbyte := b;
    //if (b = 13) and (bCharEscaped) then lastbyte := 0;
  end;
  Result := aBlockStatus;
end;

function TzpModem.zParseData(b: byte): TBlockState;
begin
  if bUseCRC32 then
    Result := zParseData32(b)
  else
    Result := zParseData16(b);
end;

procedure TZpModem.Z_SendBuf(buf: Pointer; len: integer);
begin
  if (TnCnx1 as TTnCnx).IsConnected then 
  begin
    (TnCnx1 as TTnCnx).Send(buf, len);
  end
  else 
  begin
    if bReceiving then zZModemState := rzError
    else 
    begin
      zZModemState := tzError;
      ClearTransmitTimer;
    end;
  end;
end;

procedure TZpModem.Z_SendByte(b: byte);
  (* Output one byte *)
var
  buf: array[0..3] of char;
  i : integer;
begin
  //   Async_Send(Chr(b))
  {$IFDEF LOGSENT}
  SLogByte(b);
  {$ENDIF}

  i := 0;
  if (b = $FF) then
  begin
    buf[0] := #$FF;
    buf[1] := #$FF;
    i := 2;
  end
  else if (b = 13) then
  begin
    buf[0] := #13;
    buf[1] := #0;
    i := 2;
  end
  else begin
    buf[0] := char(b);
    i := 1;
  end;

  if (TnCnx1 as TTnCnx).IsConnected then
  begin
    if (TnCnx1 as TTnCnx).IsSSH then i := 1; { SSH do not have IAC escape }
    (TnCnx1 as TTnCnx).Send(@buf[0], i);
  end
  else 
  begin
    if bReceiving then zZModemState := rzError
    else 
    begin
      zZModemState := tzError;
      ClearTransmitTimer;
    end;
  end;
end;

procedure TzpModem.Z_PutLongIntoHeader(l: longint);
  (* Reverse of above *)
begin
  txhdr[ZP0] := byte(l);
  txhdr[ZP1] := byte(l shr 8);
  txhdr[ZP2] := byte(l shr 16);
  txhdr[ZP3] := byte(l shr 24)
end;

procedure TZpModem.Z_PutHex(b: byte);
  (* Output a byte as two hex digits (in ASCII) *)
  (* Uses lower case to avoid confusion with    *)
  (* escaped control characters.                *)
const
  hex: array[0..15] of char = '0123456789abcdef';
begin
  Z_SendByte(Ord(hex[b shr 4]));  { high nybble }
  Z_SendByte(Ord(hex[b and $0F])) { low nybble  }
end;

procedure TzpModem.Z_SendHexHeader(htype: byte; var hdr: THDRBuf);
  (* Sends a zmodem hex type header *)
var
  crc: word;
  n, i: integer;
begin
  Z_SendByte(ZPAD);                  { '*' }
  Z_SendByte(ZPAD);                  { '*' }
  Z_SendByte(ZDLE);                  { 24  }
  Z_SendByte(ZHEX);                  { 'B' }
  Z_PutHex(htype);
  crc := UpdCrc(htype, 0);
  for n := 0 to 3 do
  begin
    Z_PutHex(hdr[n]);
    crc := UpdCrc(hdr[n], crc)
  end;
  crc := UpdCrc(0, crc);
  crc := UpdCrc(0, crc);
  Z_PutHex(Lo(crc shr 8));
  Z_PutHex(Lo(crc));
  Z_SendByte(13);                    { make it readable to the other end }
  Z_SendByte(10);                    { just in case                      }
  if (htype <> ZFIN) and (htype <> ZACK) then
    Z_SendByte(17);                 { Prophylactic XON to assure flow   }
end;

procedure TZpModem.RZ_AckBibi;
begin
  Z_PutLongIntoHeader(rxbytes);
  Z_SendHexHeader(ZFIN, txhdr);
  Dec(nbibi);
  if nbibi <= 0 then
    zZmodemState := rzError;
end;

procedure TZpModem.Z_SendCan;
  (* Send a zmodem CANcel sequence to the other guy *)
  (* 8 CANs and 8 backspaces                        *)
var
  n: byte;
begin
  ClearRestByte;
  for n := 1 to 8 do
  begin
    Z_SendByte(CAN);
    //Sleep(100)     { the pause seems to make reception of the sequence }
  end;              { more reliable                                     }
  for n := 1 to 10 do
    Z_SendByte(8);
end;

function TZpModem.RZ_GetHeader: integer;
var
  e, p, n, i: integer;
  multiplier: longint;
  s: string;
  ttime, tsize: longint;
  tname: string;
  secbuf: PDataBuf;
begin
  //   isbinary := TRUE;    {Force the issue!}
  secbuf := @aBlockData;

  fsize := longint(0);
  p := 0;
  s := '';
  while (p < 255) and (secbuf[p] <> 0) do
  begin
    //s := s + UpCase(Chr(secbuf[p]));
    s := s + Chr(secbuf[p]);
    Inc(p)
  end;
  Inc(p);
  (* get rid of drive & path specifiers *)
  while (Pos(':', s) > 0) do
    Delete(s, 1, Pos(':', s));
  while (Pos('\', s) > 0) do
    Delete(s, 1, Pos('\', s));
  fname := s;

  (**** done with name ****)

  fsize := longint(0);
  while (p < ZBUFSIZE) and (secbuf[p] <> $20) and (secbuf[p] <> 0) do
  begin
    fsize := (fsize * 10) + Ord(secbuf[p]) - $30;
    Inc(p)
  end;
  Inc(p);

  (**** done with size ****)

  s := '';
  while (p < ZBUFSIZE) and (secbuf[p] in [$30..$37]) do
  begin
    s := s + Chr(secbuf[p]);
    Inc(p)
  end;
  Inc(p);
  //   ftime := Z_FromUnixDate(s);

  (**** done with time ****)

  if not DirectoryExists(fdownloadpath) then 
  begin
    CreateDir(fdownloadpath);
  end;
  if Pos('\', fdownloadpath) <> Length(fdownloadpath) then 
  begin
    fdownloadpath := fdownloadpath + '\';
  end;
{   IF (Z_FindFile(fname,tname,tsize,ttime)) THEN
   BEGIN
      IF (zconv = ZCRESUM) AND (fsize > tsize) THEN
      BEGIN
         filestart := tsize;
         IF (NOT Z_OpenFile(outfile,zrxpath + fname)) THEN
         BEGIN
            Z_message('Error opening '+fname);
            RZ_GetHeader := ZERROR;
            Exit
         END;
         IF (NOT Z_SeekFile(outfile,tsize)) THEN
         BEGIN
            Z_Message('Error positioning file');
            RZ_GetHeader := ZERROR;
            Exit
         END;
         Z_Message('Recovering')
      END
      ELSE
      BEGIN
         Z_Message('File is already complete');
         RZ_GetHeader := ZSKIP;
         Exit
      END

   END
   ELSE
}   
  begin
    filestart := 0;
    if (not Z_MakeFile(outfile, fdownloadpath + fname)) then
    begin
      RZ_GetHeader := ZERROR;
      Exit
    end
  end;

  //Z_ShowName(fname);
  //Z_ShowSize(fsize);
  //Z_ShowTransferTime(fsize,zbaud);
  RZ_GetHeader := ZOK
end;

function TZpModem.RZ_SaveToDisk(var rxbytes: longint): integer;
var
  secbuf: PDataBuf;
begin
  secbuf := @aBlockData;

  if (not Z_WriteFile(outfile, secbuf^, iBlockData)) then
  begin
    RZ_SaveToDisk := ZERROR
  end
  else
    RZ_SaveToDisk := ZOK;

  rxbytes := rxbytes + iBlockData;
end;

procedure TzpModem.PrepareReceive;
begin
  bReceiving := True;
  statform.Caption := 'ZModem Download';
  zZModemState := rzRqstFile;
  Z_PutLongIntoHeader(longint(0));
  txhdr[ZF0] := CANFDX or CANOVIO or CANFC32 or CANBRK;
  {Full dplx, overlay I/O and CRC32}

  { buflen = 4096 
  txhdr[ZF1] := $10;
  rxhdr[ZF2] := $00;
  }

  bUseCRC32 := True;
  Z_SendHexHeader(ZRINIT, txhdr);
  //SZ_SendBinaryHead32(ZRINIT, txhdr);
  zZmodemState := rzWaitFile;
  PrepareHeader;
end;

procedure TzpModem.PrepareHeader;
begin
  aHeaderStatus := hsInit;
end;

procedure TzpModem.PrepareData;
begin
  aBlockStatus := bsInit;
  iBlockData := 0;
  dwDataCRC := $FFFFFFFF;
end;

function TzpModem.zParseOO(b: byte): boolean;
begin
  Result := False;
  if PullEscapeChar(b) then 
  begin
    if b = 79 then 
    begin
      Inc(OOCount);
      if OOCount >= 2 then 
      begin
        Result := True;
      end;
    end;
  end;
end;

{ Data driven, buffer data pull in one by one }
function TzpModem.zParseReceive(b: byte): TZmodemState;
begin
  case zZModemState of
    rzRqstFile: 
      begin
        PrepareReceive;
        PrepareHeader;
        zZmodemState := rzWaitFile;
        ZMessage := 'Request File...';
        Sleep(100);
      end;
    rzWaitFile:
      begin
        if ZParseHdr(b) = hsDONE then
        begin
          case rzHeaderType of
            ZrQInit: {Go send ZrInit again}
              PrepareReceive;
            //PrepareHeader;
            ZFreeCnt,
            ZSINIT: 
              begin
                Z_PutLongIntoHeader(longint(0));
                Z_SendHexHeader(ZACK, txhdr);
                PrepareHeader;
              end;
            ZFILE:
              begin
                zZmodemState := rzCollectFile;
                IniStatus;
                PrepareData;
                ZMessage := 'Wait File...';
              end;
            ZCOMPL, ZFIN:      {Finished}
              begin
                RZ_AckBibi;
                zZmodemState := rzCleanup;
              end;
          end;
        end;
      end;
    rzCollectFile:
      begin
        ZMessage := 'Collect File...';
        if zParseData(b) = bsDONE then
        begin
          zZmodemState := rzStartData;

          bFileOpen := False;
          if RZ_GetHeader = ZOK then
            bFileOpen := True;
          rxbytes := filestart;
          //rxpos := filestart;
          rxbytes := 0;
          ClearRestByte;
          Z_PutLongIntoHeader(rxbytes);
          Z_SendHexHeader(ZRPOS, txhdr);
          PrepareHeader;
        end;
      end;
    rzStartData:
      begin
        ZMessage := 'Start Data...';
        if zParseHdr(b) = hsDONE then
        begin
          zZmodemState := rzCollectData;
          PrepareData;
        end;
      end;
    rzCollectData: 
      begin
        ZMessage := 'Collect Data...';
        if zParseData(b) = bsDONE then
        begin
          case aBlockZDLE of
            zsGOTCRCW:
              begin
                RZ_SaveToDisk(rxbytes);
                Z_PutLongIntoHeader(rxbytes);
                Z_SendHexHeader(ZACK, txhdr);
                zZmodemState := rzStartData;
                PrepareHeader;
              end;
            zsGOTCRCQ: 
              begin
                RZ_SaveToDisk(rxbytes);
                Z_PutLongIntoHeader(rxbytes);
                Z_SendHexHeader(ZACK, txhdr);
                PrepareData;
              end;
            zsGOTCRCG:
              begin
                RZ_SaveToDisk(rxbytes);
                PrepareData;
              end;
            zsGOTCRCE: 
              begin
                RZ_SaveToDisk(rxbytes);
                zZmodemState := rzWaitEof;
                PrepareHeader;
              end;
          end;
          {
          if statform.bCancel then
          begin
            //ClearRestByte;
            Z_SendCan;
            RZ_AckBibi;
            //zZmodemState := rzWaitCancel;
            //PrepareHeader;
          end;
          }
        end;
      end;
    rzWaitEof: 
      begin
        ZMessage := 'Wait EOF...';
        if zParseHdr(b) = hsDone then
        begin
          Z_CloseFile(outfile);
          bFileOpen := False;
          if not statform.bCancel then
          begin
            PrepareReceive;
            PrepareHeader;
            zZmodemState := rzWaitFile;
          end
          else
          begin
            RZ_AckBibi;
            zZmodemState := rzCleanup;
          end;
        end;
      end;
    rzCollectFinish: 
      begin
        ZMessage := 'Collect Finish...';
        RZ_AckBibi;
        zZmodemState := rzCleanup;
      end;
    rzCleanup: 
      begin
        ZMessage := 'Clean Up...';
        zZmodemState := rzDone;
        {$IFDEF LOGRECV}
        RCloseLog;
        {$ENDIF}
      end;
    rzError: 
      begin
        ZMessage := 'Error';
        if bFileOpen then
          Z_CloseFile(outfile);
        //RZ_AckBibi;
        zZmodemState := rzWaitCancel;
      end;
    rzWaitCancel:
      begin
        ZMessage := 'Wait Cancel';
        //RZ_AckBibi;
        zZmodemState := rzCleanup;
      end;
  end;
  Result := zZModemState;
end;

function TzpModem.ClearRestByte: boolean;
begin
  if curByte < curLen then
  begin
    curByte := curLen;
    Result := True;
  end
  else 
  begin
    Result := False;
  end;
end;

procedure TzpModem.IniStatus;
begin
  statform.psFileName.Caption := '';
  statform.lblrxbyte.Caption := '  Bytes transferred (0%)';
  statform.psStatusMsg.Caption := 'OK';
  statform.pbRxByte.Position := 0;
  statform.bCancel := False;
  starttime := Now;
end;

procedure TzpModem.UpdateStatus;
var
  t1: integer;
  ms: double;
begin
  if statform.bCancel then 
  begin
    //Z_SendCan;
    //zZmodemState := rzError;
    //Exit;
    statform.psStatusMsg.Caption := 'Canceling...';
  end;
  statform.psFileName.Caption := fname;
  if fsize <> 0 then 
  begin
    t1 := Round(rxbytes / fsize * 100);
    statform.lblrxbyte.Caption := IntToStr(rxbytes) + ' of ' +
      IntToStr(fsize) + ' Bytes transferred (' + IntToStr(t1) + '%) ';
    ms := MilliSecondSpan(Now, starttime);
    if ms > 0 then
      statform.psThroughput.Caption := IntToStr(Round(rxbytes * 1000.0 / ms)) +
        ' Bytes/second';
    statform.pbRxByte.Position := t1;
  end;
  statform.psStatusMsg.Caption := ZMessage;
end;

function TzpModem.ProcessZModemRecevive(Data: PChar; Len: integer): boolean;
begin
  curByte := 0;
  curLen := Len;

  {$IFDEF LOGRECV}
  RLogBuf(Data, Len);
  {$ENDIF}

  while curByte < Len do
  begin
    zParseReceive(byte((Data + curByte)^));
    Inc(curByte);
  end;
  UpdateStatus;
end;


{ Transmit routines }

procedure TzpModem.SZ_Z_SendByte(b: byte);
begin
  if zEscapeAll and ((b and $60) = 0) then
  begin
    Z_SendByte(ZDLE);
    lastsent := (b xor 64)
  end
  else if ((b and $7F) in [16, 17, 19, 24]) or (((b and $7F) = 13) and
    ((lastsent and $7F) = 64)) then
  begin
    Z_SendByte(ZDLE);
    lastsent := (b xor 64)
  end
  else
    lastsent := b;

  Z_SendByte(lastsent);
end;

function TzpModem.Z_FileCRC32(var f: file): longint;
var
  fbuf: TDataBuf;
  crc: DWORD;
  bread, n: integer;
begin {$I-}
  crc := $FFFFFFFF;
  Seek(f, 0);
  if (IOresult <> 0) then      {null};
  repeat
    BlockRead(f, fbuf, ZBUFSIZE, bread);
    for n := 0 to (bread - 1) do
      crc := UpdC32(fbuf[n], crc)
    until (bread < ZBUFSIZE) or (IOresult <> 0);
  Seek(f, 0);
  if (IOresult <> 0) then      {null};
  Z_FileCRC32 := crc
end; {$I+}

procedure TzpModem.SZ_SendBinaryHead32(htype: byte; var hdr: THDRBuf);
var
  crc: DWORD;
  n: integer;
begin
  Z_SendByte(ZPAD);
  Z_SendByte(ZDLE);
  Z_SendByte(ZBIN32);
  SZ_Z_SendByte(htype);
  crc := UpdC32(htype, $FFFFFFFF);
  for n := 0 to 3 do
  begin
    SZ_Z_SendByte(hdr[n]);
    crc := UpdC32(hdr[n], crc)
  end;
  crc := (not crc);
  for n := 0 to 3 do
  begin
    SZ_Z_SendByte(byte(crc));
    crc := (crc shr 8)
  end;
  if (htype <> ZDATA) then
    Sleep(50)
end;

procedure TzpModem.SZ_SendBinaryHeader(htype: byte; var hdr: THDRBuf);
var
  crc: word;
  n: integer;
begin
  if (usecrc32) then
  begin
    SZ_SendBinaryHead32(htype, hdr);
    Exit
  end;
  Z_SendByte(ZPAD);
  Z_SendByte(ZDLE);
  Z_SendByte(ZBIN);
  SZ_Z_SendByte(htype);
  crc := UpdCrc(htype, 0);
  for n := 0 to 3 do
  begin
    SZ_Z_SendByte(hdr[n]);
    crc := UpdCrc(hdr[n], crc)
  end;
  crc := UpdCrc(0, crc);
  crc := UpdCrc(0, crc);
  SZ_Z_SendByte(Lo(crc shr 8));
  SZ_Z_SendByte(Lo(crc));
  if (htype <> ZDATA) then
    Sleep(50)
end;

procedure TzpModem.SZ_SendDa32(var buf: TDATABuf; blength: integer; frameend: byte);
var
  crc: DWORD;
  t: integer;
begin
  crc := $FFFFFFFF;
  for t := 0 to (blength - 1) do
  begin
    SZ_Z_SendByte(buf[t]);
    crc := UpdC32(buf[t], crc)
  end;

  crc := UpdC32(frameend, crc);
  crc := (not crc);
  Z_SendByte(ZDLE);
  Z_SendByte(frameend);
  for t := 0 to 3 do
  begin
    SZ_Z_SendByte(byte(crc));
    crc := (crc shr 8)
  end;
end;

procedure TzpModem.SZ_SendData(var buf: TDATABuf; blength: integer; frameend: byte);
var
  crc: word;
  t: integer;
begin
  if (usecrc32) then
  begin
    SZ_SendDa32(buf, blength, frameend);
    Exit
  end;
  crc := 0;
  for t := 0 to (blength - 1) do
  begin
    SZ_Z_SendByte(buf[t]);
    crc := UpdCrc(buf[t], crc)
  end;
  crc := UpdCrc(frameend, crc);
  Z_SendByte(ZDLE);
  Z_SendByte(frameend);
  crc := UpdCrc(0, crc);
  crc := UpdCrc(0, crc);
  SZ_Z_SendByte(Lo(crc shr 8));
  SZ_Z_SendByte(Lo(crc));
  if (frameend = ZCRCW) then
  begin
    Z_SendByte(17);
    //Sleep(500)
  end
end;

procedure TzpModem.SZ_EndSend;
var
  done: boolean;
begin
  done := False;
  //   REPEAT
  Z_PutLongIntoHeader(txpos);
  SZ_SendBinaryHeader(ZFIN, txhdr);
{      CASE Z_GetHeader(rxhdr) OF
         ZFIN: BEGIN
                  Z_SendByte(Ord('O'));
                  Z_SendByte(Ord('O'));
                  Delay(500);
                  Z_ClearOutbound;
                  Exit
               END;
         ZCAN,
         RCDO,
         ZFERR,
         ZTIMEOUT: Exit
      END
}
  //   UNTIL (done)
end;

procedure TzpModem.PrepareTransmit;
begin
  {$IFDEF LOGSENT}
  SOpenLog;
  {$ENDIF}

  bReceiving := False;
  statform.Caption := 'ZModem Upload';

  IniStatus;
  zpCanceled := False;

  Z_SendByte(Ord('r'));
  Z_SendByte(Ord('z'));
  Z_SendByte(13);
  Z_SendByte(0);
  Z_PutLongIntoHeader(longint(0));
  Z_SendHexHeader(ZRQINIT, txhdr);
  zZmodemState := tzInitial;
  PrepareHeader;
end;

function TzpModem.SZ_PrepareFile: boolean;
var
  s: string;
  dt: TDateTime;
  apath: string;
  f: TOpenDialog;
begin
  f := TOpenDialog.Create(Application);
  if f.Execute then 
  begin
    uploadfilename := f.FileName;
  end
  else 
  begin
    Z_SendCan;
    Z_PutLongIntoHeader(txpos);
    SZ_SendBinaryHeader(ZFIN, txhdr);

    zpCanceled := True;
    zZmodemState := tzError;
    f.Free;
    Exit;
  end;
  f.Free;
  apath := ExtractFilePath(uploadfilename);
  fname := ExtractFileName(uploadfilename);
  if (not Z_FindFile(apath + fname, s, fsize, ftime)) then
  begin
    dt := FileDateToDateTime(ftime);
  end;
  if (not Z_OpenFile(infile, apath + fname)) then
  begin
  end;
  rxpos := 0;
  Str(fsize, s);
  s := (fname + #0 + s + ' ');
  ftime := FileGetDate(TFileRec(infile).Handle);
  s := s + FileDateStr(ftime); //Z_ToUnixDate(dt);
  blkred := Length(s) + 1;
  FillChar(txbuf, ZBUFSIZE, 0);
  Move(s[1], txbuf[0], Length(s));
end;

procedure TzpModem.SZ_UpdateStatus;
var
  t1: integer;
  ms: double;
begin
  if statform.bCancel and (not zpCanceled) then
  begin
    Z_SendCan;
    Z_PutLongIntoHeader(txpos);
    SZ_SendBinaryHeader(ZFIN, txhdr);

    zpCanceled := True;
    zZmodemState := tzSendFinish;
    statform.psStatusMsg.Caption := 'Canceling...';

    ClearTransmitTimer;

    Exit;
  end;
  statform.psFileName.Caption := fname;
  if fsize <> 0 then
  begin
    t1 := Round(txpos / fsize * 100);
    statform.lblrxbyte.Caption := IntToStr(txpos) + ' of ' +
      IntToStr(fsize) + ' Bytes transferred (' + IntToStr(t1) + '%) ';
    ms := MilliSecondSpan(Now, starttime);
    if ms > 0 then
      statform.psThroughput.Caption := IntToStr(Round(txpos * 1000.0 / ms)) +
        ' Bytes/second';
    statform.pbRxByte.Position := t1;
  end;
  statform.psStatusMsg.Caption := ZMessage;
end;

procedure TzpModem.SetupTransmitTimer;
begin
  if not Assigned(aTimer) then
    aTimer := TTimer.Create(Application);
  aTimer.Interval := 10;
  aTimer.Enabled := True;
  aTimer.OnTimer := TransmitEvent;
end;

procedure TzpModem.ClearTransmitTimer;
begin
  if Assigned(aTimer) then 
  begin
    aTimer.Enabled := False;
    aTimer.Free;
    aTimer := nil;
  end;
end;

procedure TzpModem.TransmitEvent(Sender: TObject);
var
  e: integer;
begin
  ZMessage := 'Sending Data....';
  Z_ReadFile(infile, txbuf, blklen, blkred);
  if (blkred < blklen) then
  begin
    e := ZCRCE;
    zZmodemState := tzSendEof;
  end
  else if (rxbuflen <> 0) and ((newcnt - blkred) <= 0) then
  begin
    newcnt := (newcnt - blkred);
    e := ZCRCW;
    zZmodemState := tzWaitAck;
  end
  else
    e := ZCRCG;

  SZ_SendData(txbuf, blkred, e);
  txpos := txpos + blkred;


  if e = ZCRCG then
  begin
    SZ_UpdateStatus;
  end;

  if e = ZCRCE then
  begin
    Sleep(10);
    //ClearRestByte; { ALERT!!!! }
    Z_PutLongIntoHeader(txpos);
    SZ_SendBinaryHeader(ZEOF, txhdr);
    zZmodemState := tzCheckEof;
    PrepareHeader;

    ZMessage := 'Check EOF....';
    ClearTransmitTimer;
  end;
end;

function TzpModem.zParseTransmit(b: byte): TZmodemState;
begin
  case zZModemState of
    tzInitial:
      begin
        ZMessage := 'Initialize....';
        if ZParseHdr(b) = hsDONE then
        begin
          case rzHeaderType of
            ZrInit:
              begin
                ClearRestByte;
                Sleep(100);
                Z_PutLongIntoHeader(longint(0));
                Z_SendHexHeader(ZRQINIT, txhdr);
                zZmodemState := tzHandshake;
              end;
            ZACK: 
              begin
                ClearRestByte;
                Z_PutLongIntoHeader(longint(0));
                Z_SendHexHeader(ZRQINIT, txhdr);
                zZmodemState := tzHandshake;
                PrepareHeader;
              end;
          end;
          PrepareHeader;
        end;
      end;
    tzHandshake:
      begin
        ZMessage := 'HandShake....';
        if ZParseHdr(b) = hsDONE then
        begin
          case rzHeaderType of
            ZrInit:     {Got ZrInit, extract info}
              begin
                rxbuflen := (word(rxhdr[ZP1]) shl 8) or rxhdr[ZP0];
                usecrc32 := ((rxhdr[ZF0] and CANFC32) <> 0);
                zEscapeAll := (rxhdr[ZF0] and ESCALL) = ESCALL;

                maxblklen := ZBUFSIZE;
                if (rxbuflen > 0) and (rxbuflen < maxblklen) then
                  maxblklen := rxbuflen;
                blklen := maxblklen;

                //ClearRestByte;
                FillChar(txhdr, 4, 0);
                txhdr[ZF0] := ZCBIN; //ZCRESUM; {recover}
                SZ_SendBinaryHeader(ZFILE, txhdr);

                SZ_PrepareFile;

                SZ_SendData(txbuf, blkred, ZCRCW);
                zZmodemState := tzCheckFile;
                PrepareHeader;
              end;
            ZChallenge: {Receiver is challenging, respond with same number}
              begin
                Z_PutLongIntoHeader(rxpos);
                Z_SendHexHeader(ZACK, txhdr);
                PrepareHeader;
              end;
            ZCOMMAND:
              begin
                Z_PutLongIntoHeader(longint(0));
                Z_SendHexHeader(ZRQINIT, txhdr);
                PrepareHeader;
              end;
            else
              begin
                Z_SendHexHeader(ZNak, txhdr);
                PrepareHeader;
              end;
          end;
        end;
      end;
    tzCheckFile:
      begin
        ZMessage := 'Check File....';
        if ZParseHdr(b) = hsDONE then
        begin
          case rzHeaderType of
            ZRINIT:
              begin
                // zZmodemState := tzHandshake;
                PrepareHeader;
              end;{Got an extra ZrInit, ignore it}
            ZCAN,
            ZNAK,
            ZFIN,
            ZABORT:
              begin
                zZmodemState := tzERROR;
              end;
            ZCRC:
              begin
                Z_PutLongIntoHeader(Z_FileCRC32(infile));
                Z_SendHexHeader(ZCRC, txhdr);
                PrepareHeader;
              end;
            ZSKIP:
              begin
                Z_PutLongIntoHeader(txpos);
                SZ_SendBinaryHeader(ZFIN, txhdr);
                zZmodemState := tzSendFinish;
              end;
            ZRPOS:
              begin
                if (not Z_SeekFile(infile, rxpos)) then
                begin
                  Z_SendHexHeader(ZFERR, txhdr);
                  PrepareHeader;
                end;
                //strtpos := rxpos;
                ClearRestByte;
                newcnt := rxbuflen;
                txpos := rxpos;
                Z_PutLongIntoHeader(txpos);
                SZ_SendBinaryHeader(ZDATA, txhdr);
                zZmodemState := tzSendData;

                SetupTransmitTimer;
                //goto LabelSendData;
              end
          end {case}
        end;
      end;
    tzSendData:
      begin
        if ZParseHdr(b) = hsDONE then 
        begin
          case rzHeaderType of
            ZRPOS: 
              begin
                Z_SendCan;
                ClearTransmitTimer;
                zZmodemState := tzError;
              end;
          end;
        end;
        //ClearRestByte;
        //zZmodemState := tzWaitAck;
        PrepareHeader;
      end;
    tzWaitAck: 
      begin
        ZMessage := 'Window Ack....';
        if ZParseHdr(b) = hsDONE then
        begin
          case rzHeaderType of
            ZCAN:;
            ZACK: zZmodemState := tzSendData;
            ZRPOS:;
            else
              Z_PutLongIntoHeader(txpos);
              Z_SendHexHeader(ZACK, txhdr);
              PrepareHeader;
          end;
        end;
      end;
    tzCheckEof: 
      begin
        ZMessage := 'Check EOF....';
        if ZParseHdr(b) = hsDONE then
        begin
          case rzHeaderType of
            ZCAN:;
            ZRPOS:
              begin
              {txpos := rxpos;
              Z_PutLongIntoHeader(txpos);
              SZ_SendBinaryHeader(ZDATA, txhdr);
              Z_SeekFile(infile, rxpos);

              zZmodemState := tzSendData;
              goto LabelSendData;
              }
                Z_SendCan;
                zZmodemState := tzDone;
              end;
            ZRINIT:
              begin
                ClearRestByte; { ALERT!!!! }
                Z_CloseFile(infile);
                Z_PutLongIntoHeader(txpos);
                SZ_SendBinaryHeader(ZFIN, txhdr);
                zZmodemState := tzSendFinish;
              end;
            else
              zZmodemState := tzDone;
          end;
          PrepareHeader;
        end;
      end;
    tzSendFinish:
      begin
        if ZParseHdr(b) = hsDONE then
        begin
          case rzHeaderType of
            ZFIN: 
              begin
                Z_SendByte(Ord('O'));
                Z_SendByte(Ord('O'));
                zZmodemState := tzDone;
              end;
            else
              Z_SendCan;
              zZmodemState := tzDone;
              PrepareHeader;
          end;
        end;
      end;
    tzError:
      begin
        ClearTransmitTimer;
        ClearRestByte; { ALERT!!!! }
        //Z_PutLongIntoHeader(txpos);
        //SZ_SendBinaryHeader(ZFIN, txhdr);
        zZmodemState := tzDone;
        PrepareHeader;
      end;
    tzDone: 
      begin
      end;
  end;
end;

function TzpModem.ProcessZModemTransmit(Data: PChar; Len: integer): boolean;
begin  
  curByte := 0;
  curLen := Len;
  while curByte < Len do
  begin    
    zParseTransmit(byte((Data + curByte)^));
    Inc(curByte);
  end;
  SZ_UpdateStatus;
end;

end.
