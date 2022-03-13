unit RPC;

interface

uses
  Classes, IdHTTP, System.json,

  FireDAC.Comp.Client;

const
  GenesisHashBlock =
    '000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f';

  LargestTransBlock =
    '000000000000048eafc216a4b55f5cf2400786925e01d611bcf7964465de13e9';

type

  TTransaction = record
    hex, txid, hash: string;
    time, blocktime: TDateTime;
  end;

  TInfoRecord = record
    version: string;
    protocolversion: string;
    blocks: string;
    timeoffset: string;
    connections: string;
    proxy: string;
    difficulty: string;
    testnet: string;
    paytxfee: string;
    relayfee: string;
  end;

  TNetWorkInfoRecord = record
    version: string;
    subversion: string;
    protocolversion: string;
    localservices: string;
    localrelay: string;
    timeoffset: TDateTime;
    connections: string;
    relayfee: string;
    warnings: string;
  end;

  TBlock = class(TObject)
  public
    ajson: string;
    hash: string;
    confirmations: integer;
    strippedsize: integer;
    size: integer;
    weight: integer;
    height: integer;
    version: integer;
    versionHex: string;
    merkleroot: string;

    transactions: tstringlist;

    time, mediantime: TDateTime;
    nonce: int64;
    bits: string;
    difficulty: extended;
    chainwork: string;
    previousblockhash, nextblockhash: string;
  end;

  TBlockThread = class;
  TBlockChainDB = class;
  TNewBlockEvent = procedure(const aBlock: TBlock) of object;
  TBlockCountEvent = procedure(const aBlockCoun: cardinal) of object;

  TBCN = class(TComponent)
  strict private
    fOnReady: TNotifyEvent;
    fOnNewBlock: TNewBlockEvent;
    fOnBlockCount: TBlockCountEvent;

    fBlockThread: TBlockThread;

    aHTTP: TIdHTTP;
    JsonToSend: TStringStream;
    fJSON: TJsonobject;

    function post(const command: string): string;
  private
    procedure SetRPCPassword(const Value: string);
    procedure SetRPCUser(const Value: string);
    function GetRPCPassword: string;
    function GetRPCUser: string;

  public
    constructor Create(Owner: TComponent);
    destructor Destroy; override;

    procedure Start;
    function GetResultFromJSON(const ajson: string): string;
    function GetBlockJSON(const aBlockHash: string): string;
    function GetBlockHash(const aBlockNumber: integer): string;
    function GetBlock(const aBlockHash: string): TBlock;
    function GetInfo: TInfoRecord;
    function GetDifficulty: string;
    function GetNetworkInfo: TNetWorkInfoRecord;
    function GetBlockCount: int64;

    function GetRawTransaction(const atx: string): string;
    function GetTransaction(const atx: string): TTransaction;

    property rpcuser : string read GetRPCUser write SetRPCUser;
    property rpcpassword : string read GetRPCPassword write SetRPCPassword;

    property OnReady: TNotifyEvent read fOnReady write fOnReady;
    property OnNewBlock: TNewBlockEvent read fOnNewBlock write fOnNewBlock;
    property OnBlockCount: TBlockCountEvent read fOnBlockCount
      write fOnBlockCount;
  end;

  TBlockThread = class(TThread)
  private
    { Private declarations }
    fBCN: TBCN;

    fBCNDB: TBlockChainDB;

    LastBlockStored: int64;

    procedure CallEvents;
    procedure CallBlockCountEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const aBCN: TBCN; CreateSuspended: boolean);
    destructor Destroy; override;
  end;

  TBlockChainDB = class
  strict private
    FDConnection: TFDConnection;
    FDCommand: TFDCommand;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StoreBlock(const aBlock: TBlock);
  end;

implementation

uses
  System.SysUtils, forms, System.dateutils,

  FireDAC.Stan.Factory;

var
  aGlobalTBCN: TBCN;

function GetGlobalBNC: TBCN;
begin
  if aGlobalTBCN = nil then
  begin
    aGlobalTBCN := TBCN.Create(nil);
  end;

  result := aGlobalTBCN;
end;

constructor TBCN.Create(Owner: TComponent);
begin
  inherited;

  aHTTP := TIdHTTP.Create(self);
  aHTTP.Request.BasicAuthentication := true;

 // fBlockThread := TBlockThread.Create(self, true);
end;

destructor TBCN.Destroy;
begin

 // fBlockThread.Terminate;

 // while not fBlockThread.Terminated do;

 // fBlockThread.free;

  aHTTP.free;
  inherited;
end;

function TBCN.GetBlockHash(const aBlockNumber: integer): string;
begin
  result := GetResultFromJSON
    (post(format
    ('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getblockhash", "params": [%d] }',
    [aBlockNumber])));
end;

function TBCN.GetBlock(const aBlockHash: string): TBlock;

begin
end;

function TBCN.GetBlockJSON(const aBlockHash: string): string;
begin
  result := post
    (format('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getblock", "params": ["%s"] }',
    [aBlockHash]));
end;

function TBCN.GetBlockCount: int64;
begin
  result := strtoint
    (GetResultFromJSON
    (post('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getblockcount", "params": [] }'))
    );
end;

function TBCN.GetDifficulty: string;
begin
  result := post
    ('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getdifficulty", "params": [] }')
end;

function TBCN.GetInfo: TInfoRecord;
var
  ajson: string;
  aa: tjsonvalue;
begin
  ajson := post
    ('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getinfo", "params": [] }');

  fJSON := TJsonobject.Create;
  if fJSON.Parse(BytesOf(ajson), 0) > 0 then
  begin
    aa := fJSON.GetValue('result');
    result.version := aa.GetValue<string>('version');
    result.protocolversion := aa.GetValue<string>('protocolversion');
    result.blocks := aa.GetValue<string>('blocks');
    result.timeoffset := aa.GetValue<string>('timeoffset');
    result.connections := aa.GetValue<string>('connections');
    result.proxy := aa.GetValue<string>('proxy');
    result.difficulty := aa.GetValue<string>('difficulty');
    result.testnet := aa.GetValue<string>('testnet');
    result.paytxfee := aa.GetValue<string>('paytxfee');
    result.relayfee := aa.GetValue<string>('relayfee');
  end;

  fJSON.free;
end;

function TBCN.GetNetworkInfo: TNetWorkInfoRecord;
var
  ajson: string;
  aa: tjsonvalue;
begin
  ajson := post
    ('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getnetworkinfo", "params": [] }');

  fJSON := TJsonobject.Create;
  if fJSON.Parse(BytesOf(ajson), 0) > 0 then
  begin
    aa := fJSON.GetValue('result');
    result.version := aa.GetValue<string>('version');
    result.subversion := aa.GetValue<string>('subversion');
    result.protocolversion := aa.GetValue<string>('protocolversion');
    result.localservices := aa.GetValue<string>('localservices');
    result.localrelay := aa.GetValue<string>('localrelay');
    result.timeoffset := UnixToDateTime(aa.GetValue<int64>('timeoffset'));
    result.connections := aa.GetValue<string>('connections');
    result.relayfee := aa.GetValue<string>('relayfee');
    result.warnings := aa.GetValue<string>('warnings');
  end;

  fJSON.free;
end;

function TBCN.GetResultFromJSON(const ajson: string): string;
begin
  fJSON := TJsonobject.Create;
  if fJSON.Parse(BytesOf(ajson), 0) > 0 then
    result := fJSON.GetValue<string>('result');
  fJSON.free;
end;

function TBCN.GetRPCPassword: string;
begin
result := aHTTP.Request.Password;
end;

function TBCN.GetRPCUser: string;
begin
  result := aHTTP.Request.Username;
end;

function TBCN.GetTransaction(const atx: string): TTransaction;
var
  ajson: string;
  aa: tjsonvalue;
begin
  ajson := self.GetRawTransaction(atx);

  fJSON := TJsonobject.Create;
  if fJSON.Parse(BytesOf(ajson), 0) > 0 then
  begin
    aa := fJSON.GetValue('result');
    result.time := UnixToDateTime(aa.GetValue<int64>('time'));
    result.blocktime := UnixToDateTime(aa.GetValue<int64>('blocktime'));
  end;

  fJSON.free;
end;

function TBCN.GetRawTransaction(const atx: string): string;
begin
  try
    result := post
      (format('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getrawtransaction", "params": [%s,1] }',
      [atx]));
  except
    result := '';
  end;
end;

function TBCN.post(const command: string): string;
begin

  JsonToSend := TStringStream.Create(command);

  try
    result := aHTTP.post('http://192.168.1.150:8332', JsonToSend);
  finally
    JsonToSend.free;
  end;
end;

procedure TBCN.SetRPCPassword(const Value: string);
begin
  aHTTP.Request.Password := value;
end;

procedure TBCN.SetRPCUser(const Value: string);
begin
  aHTTP.Request.Username := value;
end;

procedure TBCN.Start;
var
  fReady: boolean;
begin
  fReady := false;
  while not fReady do
  begin
    try
      GetInfo;
      fReady := true;

      fBlockThread.Start;

      if assigned(fOnReady) then
        fOnReady(self);
    except
      Application.ProcessMessages;
    end;

  end;
end;

{ TBlock }
{
  function TBlock.GetRawTransaction(const txid: string): TTransaction;
  var
  ajson: string;
  aa: tjsonvalue;
  begin
  result := post
  (format('{"jsonrpc": "1.0", "id":"BTCExposed", "method": "getrawtransaction", "params": [%s,1] }
{ [atx]));

  fJSON := TJsonobject.Create;
  if fJSON.Parse(BytesOf(ajson), 0) > 0 then
  begin
  aa := fJSON.GetValue('result');
  result.time := UnixToDateTime(aa.GetValue<Int64>('time'));
  result.blocktime := UnixToDateTime(aa.GetValue<Int64>('blocktime'));
  end;

  fJSON.free;
  end;
}
{ TBlockThread }

constructor TBlockThread.Create(const aBCN: TBCN; CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);

  fBCN := aBCN;

  Priority := tpIdle;

  fBCNDB := TBlockChainDB.Create;
end;

destructor TBlockThread.Destroy;
begin
  fBCNDB.free;

  inherited;
end;

procedure TBlockThread.CallBlockCountEvent;
begin
  if assigned(fBCN.OnBlockCount) then
    fBCN.OnBlockCount(fBCN.GetBlockCount);

end;

procedure TBlockThread.CallEvents;
var
  aBlock: TBlock;
begin
  if LastBlockStored < fBCN.GetBlockCount then
  begin
    aBlock := fBCN.GetBlock(fBCN.GetBlockHash(LastBlockStored));

    if assigned(fBCN.OnNewBlock) then
      fBCN.OnNewBlock(aBlock);

    inc(LastBlockStored);

    fBCNDB.StoreBlock(aBlock);
  end;
end;

procedure TBlockThread.Execute;
begin
  inherited;

  LastBlockStored := 0; // read from database latest block stored

  Synchronize(CallBlockCountEvent);

  while not Terminated do
  begin
    fBCN.GetInfo;
    // Sleep(10);

    Synchronize(CallEvents);
    Synchronize(CallBlockCountEvent);
  end;

end;

{ TBlockChainDB }

constructor TBlockChainDB.Create;
begin
  FDConnection := TFDConnection.Create(nil);
  FDConnection.Params.Add
    ('Database=..\..\..\core-and-data\metadata\metadata.db');
  FDConnection.Params.Add('DriverID=SQLite');

  FDCommand := TFDCommand.Create(FDConnection);
  FDCommand.Connection := FDConnection;

  FDConnection.Connected := true;
end;

destructor TBlockChainDB.Destroy;
begin
  FDCommand.free;
  FDConnection.free;

  inherited;
end;

procedure TBlockChainDB.StoreBlock(const aBlock: TBlock);
begin
  FDCommand.CommandText.Clear;
  FDCommand.CommandText.Add
    (format('insert into blocks (height, hash, time, mediantime) values (%d,"%s",%d, %d)',
    [aBlock.height, aBlock.hash,DateTimeToUnix(aBlock.time),DateTimeToUnix(aBlock.mediantime)]));
  FDCommand.Execute;
end;

end.
