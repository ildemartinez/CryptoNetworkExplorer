unit BTCPeerNodeUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  IPeerNodeUnit,
  PeerNodeUnit,
  NodeObserverPattern,
  ipwcore,
  ipwtypes,
  ipwipport,
  btctypes,
  SeSHA256, RPC;

type

  TMessageEventVersion = procedure(Sender: TObject;
    versionMessage: TVersionMessage) of object;
  TMessageEventVerack = procedure(Sender: TObject) of object;

  TMessageEvent = procedure(Sender: TObject; const aMessage: string) of Object;

  TBTCPeerNode = class(TPeerNode, Inode, IBTCPeerNode)
  strict private

    fipwIPPort1: TipwIPPort;

    fMessage: TMessageEvent;

  private
    fMessageVersionEvent: TMessageEventVersion;
    fMessageVerackEvent: TMessageEventVerack;

    procedure Connected(Sender: TObject; StatusCode: Integer;
      const Description: String);
    procedure DataIn(Sender: TObject; Text: String; TextB: TBytes;
      EOL: boolean);
    procedure ConnectionStatus(Sender: TObject; const ConnectionEvent: string;
      StatusCode: Integer; const Description: string);
    procedure ReadyToSend(Sender: TObject);
    procedure Error(Sender: TObject; ErrorCode: Integer;
      const Description: string);

    procedure DoMessageDetected(const aMessageType: string;
      const apayload: TBytes);

    procedure VerackMessage(Sender: TObject);

    procedure SendCommand(const messagename: ansistring);
    procedure SendMessage(const messagename: ansistring; const payload: TBytes);

  public
    constructor Create(OWner: TComponent); override;
    destructor Destroy; override;

    procedure Connect; virtual;
    procedure Disconnect;

    procedure GetPeers(); override;
  published
    // property UserAgent: string read fUserAgent;

    property OnMessage: TMessageEvent read fMessage write fMessage;

    property OnVersionMessage: TMessageEventVersion read fMessageVersionEvent
      write fMessageVersionEvent;
    property OnVerackMessage: TMessageEventVerack read fMessageVerackEvent
      write fMessageVerackEvent;
    // property PeeerDiscovery : TBTCPeerDiscovery read fBTCPeerDiscovery write fBTCPeerDiscovery;
  end;

  TBTCRPCNode = class(TBTCPeerNode)
  private

    function GetRPCPassword: string;
    function GetRPCUser: string;
    procedure SetRPCPassword(const Value: string);
    procedure SetRPCUser(const Value: string);
  public
    // TODO hide to private ->
    fRPC: TBCN;

    // TODO refactor to properties
    rpcport: Integer;

    constructor Create(OWner: TComponent); override;
    destructor Destroy; override;

    procedure Connect; override;

    property rpcuser: string read GetRPCUser write SetRPCUser;
    property rpcpassword: string read GetRPCPassword write SetRPCPassword;
  end;

procedure Register;

implementation

uses
  Winapi.Windows,
  dateutils,
  dialogs;

function StringBytesToTBytes(const s: string): TBytes;
var
  k: Integer;
begin
  SetLength(result, length(s) div 2);
  for k := 0 to (s.length div 2) - 1 do
    result[k] := StrToInt('$' + s[(k * 2) + 1] + s[(k + 1) * 2]);
end;

procedure Register;
begin
  RegisterComponents('CryptoCurrency', [TBTCPeerNode]);
end;

procedure TBTCPeerNode.Connect;
begin
  if not fipwIPPort1.Connected then
    fipwIPPort1.Connect(PeerIp, 8333);
end;

procedure TBTCPeerNode.Connected(Sender: TObject; StatusCode: Integer;
  const Description: String);
begin
  fServerConnected := true;

  ImplNodeObsevable.Notify(msgtserverconnected, self)
end;

procedure TBTCPeerNode.ConnectionStatus(Sender: TObject;
  const ConnectionEvent: string; StatusCode: Integer;
  const Description: string);
begin

end;

constructor TBTCPeerNode.Create(OWner: TComponent);
begin
  inherited;

  fipwIPPort1 := TipwIPPort.Create(self);
  fipwIPPort1.OnConnected := Connected;
  // conectado al nodo pero no al protocolo
  fipwIPPort1.OnDataIn := DataIn;
  fipwIPPort1.OnConnectionStatus := ConnectionStatus;
  fipwIPPort1.OnReadyToSend := ReadyToSend;
  fipwIPPort1.OnError := Error;

  OnVerackMessage := VerackMessage;
  // aBTCThreadMonitor :=  TBTCThreadMonitor.Create(fIp,self);
end;

procedure TBTCPeerNode.DataIn(Sender: TObject; Text: String; TextB: TBytes;
  EOL: boolean);
var
  k, j: Integer;
  status: Integer;
  aMessage: string;
  alength: cardinal;
  // checksum: cardinal;
  payload: TBytes;
begin

  k := 0;
  status := 0;
  alength := 0;

  while k < length(TextB) do
  begin
    case status of
      // magic code detection
      0:
        if TextB[k] = $F9 then
          inc(status);
      1:
        if TextB[k] = $BE then
          inc(status);
      2:
        if TextB[k] = $B4 then
          inc(status);
      3:
        if TextB[k] = $D9 then
          inc(status);
      // command detection
      4:
        begin
          aMessage := '';

          j := 0;
          while (j < 12) and (TextB[j + k] <> 0) do
          begin
            aMessage := aMessage + char(TextB[k + j]);
            inc(j);
          end;

          // showmessage(amessage + ' ' + inttostr(amessage.length));

          k := k + 11;

          inc(status);
        end;
      // length detection
      5:
        begin

          alength := TextB[k] + TextB[k + 1] + TextB[k + 2] + TextB[k + 3];
          SetLength(payload, alength);
          /// ojo, solo el primer byte... añadir los siguientes

          // showmessage(inttostr(alength));

          k := k + 3;

          inc(status);
        end;
      // checksum detection
      6:
        begin

          /// Please get checksum (hay que calcularlo)
          // checksum := TextB[k] + TextB[k + 1] + TextB[k + 2] + TextB[k + 3];

          k := k + 4;

          if (alength = 0) then
          begin
            CopyMemory(payload, @TextB[k], alength);
            DoMessageDetected(aMessage, payload);
            status := 0;
          end
          else
            inc(status)
        end;
      7:
        begin
          CopyMemory(payload, @TextB[k - 1], alength);

          DoMessageDetected(aMessage, payload);
          status := 0;
        end;

    end;

    inc(k);

  end;

end;

destructor TBTCPeerNode.Destroy;
begin

  if fipwIPPort1.Connected then
    fipwIPPort1.Disconnect;

  fipwIPPort1.free;

  inherited;
end;

procedure TBTCPeerNode.Disconnect;
begin
  fipwIPPort1.Disconnect;

end;

procedure TBTCPeerNode.DoMessageDetected(const aMessageType: string;
  const apayload: TBytes);
var
  versionMessage: TVersionRawMessage;
  pversionMessage: ^TVersionRawMessage;
  aVersionMessage: TVersionMessage;
  k: Integer;
begin
  if aMessageType = MESSAGE_TYPE_VERSION then
  begin
    // Create the version message
    pversionMessage := @versionMessage;
    CopyMemory(pversionMessage, apayload, length(apayload));

    aVersionMessage.protocol_version := versionMessage.protocol_version;
    aVersionMessage.node_services := UInt64(versionMessage.node_services);
    aVersionMessage.node_timestamp := TTimeZone.Local.ToLocalTime
      (unixtodatetime(UInt64(versionMessage.node_timestamp)));

    aVersionMessage.receiving_node_ip := '....';
    aVersionMessage.receiving_node_port :=
      swap(versionMessage.receiving_node_port);

    aVersionMessage.emmiting_node_ip := '..-..';
    aVersionMessage.emmiting_node_port :=
      swap(versionMessage.emmiting_node_port);

    // get User Agent
    aVersionMessage.user_agent := '';
    for k := 1 to versionMessage.user_agent[0] do
      aVersionMessage.user_agent := aVersionMessage.user_agent +
        char(versionMessage.user_agent[k]);
    fUserAgent := aVersionMessage.user_agent;

    if assigned(fMessageVersionEvent) then
      fMessageVersionEvent(self, aVersionMessage);

    ImplNodeObsevable.Notify(msgtrefresh, self);
  end
  else if aMessageType = MESSAGE_TYPE_VERACK then
  begin
    if assigned(fMessageVerackEvent) then
      fMessageVerackEvent(self);

    ImplNodeObsevable.Notify(msgtprotocolconnected, self);

    SendCommand('verack');
  end
  else if assigned(fMessage) then
    fMessage(self, aMessageType);
end;

procedure TBTCPeerNode.Error(Sender: TObject; ErrorCode: Integer;
  const Description: string);
begin

end;

procedure TBTCPeerNode.GetPeers;
begin

  SendCommand('getaddr');

  // tb := StringBytesToTBytes('f9beb4d9 676574616464720000000000 00000000 5df6e0e2');
end;

procedure TBTCPeerNode.ReadyToSend(Sender: TObject);
begin
  SendMessage('version', StringBytesToTBytes( //
    '80110100' + // version
    // '0804000000000000' + // services
    '0004000000000000' + // services
    '0defbc6100000000' + //
    '090400000000000000000000000000000000ffff23c121bf208d' + // addr_recv
    '0804000000000000000000000000000000000000000000000000' + // addr_from
    'fdba00e1964f2006' + // nonce
    '10' + // 16 long
    '2f5361746f7368693a32322e302e302f' + // s
    '25c20300' + // start_height
    '01' // relay
    ));
end;

procedure TBTCPeerNode.SendCommand(const messagename: ansistring);
var
  tb: TBytes;
  aHeader: TBTCHeader;
begin
  aHeader.start_string := BTC_MAIN_MAGIC_VALUE;
  aHeader.command_name := StringToCommandName(messagename);

  aHeader.payload_size := 0;
  aHeader.checksum[1] := char($5D);
  aHeader.checksum[2] := char($F6);
  aHeader.checksum[3] := char($E0);
  aHeader.checksum[4] := char($E2);

  SetLength(tb, BTC_HEADER_SIZE);
  CopyMemory(tb, @aHeader, BTC_HEADER_SIZE);

  fipwIPPort1.send(tb);
end;

procedure TBTCPeerNode.SendMessage(const messagename: ansistring;
  const payload: TBytes);
var
  tb: TBytes;
  aHeader: TBTCHeader;
  ams: TMemoryStream;
  res: ansistring;
  k: Integer;
begin
  aHeader.start_string := BTC_MAIN_MAGIC_VALUE;
  aHeader.command_name := StringToCommandName(messagename);
  aHeader.apayload := payload;

  ams := TMemoryStream.Create;
  try
    // Guardamos el texto en un stream
    ams.Position := 0;
    ams.WriteBuffer(PAnsiChar(aHeader.apayload)^, length(aHeader.apayload));
    ams.Position := 0;
    // Calculamos el hash del stream
    res := SHA256ToBinaryStr(CalcSHA256(SHA256ToBinaryStr(CalcSHA256(ams))));

    aHeader.payload_size := length(aHeader.apayload);
    aHeader.checksum[1] := res[1];
    aHeader.checksum[2] := res[2];
    aHeader.checksum[3] := res[3];
    aHeader.checksum[4] := res[4];
  finally
    ams.free;
  end;

  SetLength(tb, BTC_HEADER_SIZE + aHeader.payload_size);
  CopyMemory(tb, @aHeader, BTC_HEADER_SIZE);

  for k := 0 to aHeader.payload_size - 1 do
    tb[BTC_HEADER_SIZE + k] := aHeader.apayload[k];

  fipwIPPort1.send(tb);
end;

procedure TBTCPeerNode.VerackMessage(Sender: TObject);
begin
  fConnected := true;
end;

{ TBTCRPCNode }

procedure TBTCRPCNode.Connect;
begin
  fServerConnected := true;

  ImplNodeObsevable.Notify(msgtserverconnected, self);
end;

constructor TBTCRPCNode.Create(OWner: TComponent);

// aetWorkInfoRecord: TNetWorkInfoRecord;
begin
  inherited;

  fRPC := TBCN.Create(self);
end;

destructor TBTCRPCNode.Destroy;
begin

  inherited;
end;

function TBTCRPCNode.GetRPCPassword: string;
begin
  result := fRPC.rpcpassword;
end;

function TBTCRPCNode.GetRPCUser: string;
begin
  result := fRPC.rpcuser;
end;

procedure TBTCRPCNode.SetRPCPassword(const Value: string);
begin
  fRPC.rpcpassword := Value;
end;

procedure TBTCRPCNode.SetRPCUser(const Value: string);
begin
  fRPC.rpcuser := Value;
end;

end.
