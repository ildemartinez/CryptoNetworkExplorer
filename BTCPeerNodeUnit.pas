unit BTCPeerNodeUnit;

interface

uses
  System.SysUtils, System.Classes,
  System.Generics.Collections,
  IPeerNodeUnit,
  PeerNodeUnit,
  NodeObserverPattern,
  ipwcore, ipwtypes, ipwipport, btctypes;

type

  TMessageEventVersion = procedure(Sender: TObject;
    versionMessage: TVersionMessage) of object;
  TMessageEventVerack = procedure(Sender: TObject) of object;

  TMessageEvent = procedure(Sender: TObject; const aMessage: string) of Object;

  TBTCPeerNode = class(TPeerNode, IBTCPeerNode)
  strict private

    fipwIPPort1: TipwIPPort;
    fUserAgent: string;

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

  public
    constructor Create(OWner: TComponent); override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;

  published
    property UserAgent: string read fUserAgent;

    property OnMessage: TMessageEvent read fMessage write fMessage;

    property OnVersionMessage: TMessageEventVersion read fMessageVersionEvent
      write fMessageVersionEvent;
    property OnVerackMessage: TMessageEventVerack read fMessageVerackEvent
      write fMessageVerackEvent;
    // property PeeerDiscovery : TBTCPeerDiscovery read fBTCPeerDiscovery write fBTCPeerDiscovery;
  end;

procedure Register;

implementation

uses
  Winapi.Windows, dateutils;

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
  alength, checksum: cardinal;
  payload: TBytes;
begin

  k := 0;
  status := 0;

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
          /// ojo, solo el primer byte... a�adir los siguientes

          // showmessage(inttostr(alength));

          k := k + 3;

          inc(status);
        end;
      // checksum detection
      6:
        begin

          /// Please get checksum
          checksum := TextB[k] + TextB[k + 1] + TextB[k + 2] + TextB[k + 3];

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
  t: UInt64;
  aDate: TDatetime;
  k: Integer;

  aObserver: INodeObserver;

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
  end
  else if assigned(fMessage) then
    fMessage(self, aMessageType);
end;

procedure TBTCPeerNode.Error(Sender: TObject; ErrorCode: Integer;
  const Description: string);
begin

end;

procedure TBTCPeerNode.ReadyToSend(Sender: TObject);
var
  tb: TBytes;
  pHeader: ^THeader;
  aHeader: THeader;
begin
  aHeader := BuildHeader('version');

  (* new(pHeader);
    pHeader^.start_string := $D9B4BEF9;

    pHeader^.command_name[1] := 'v';
    pHeader^.command_name[2] := 'e';
    pHeader^.command_name[3] := 'r';
    pHeader^.command_name[4] := 's';
    pHeader^.command_name[5] := 'i';
    pHeader^.command_name[6] := 'o';
    pHeader^.command_name[7] := 'n';
    pHeader^.command_name[8] := char(0);
    pHeader^.command_name[9] := char(0);
    pHeader^.command_name[10] := char(0);
    pHeader^.command_name[11] := char(0);
    pHeader^.command_name[12] := char(0);

    pHeader^.payload_size := $66;
    pHeader^.checksum := $9B3DBA94;
  *)
  SetLength(tb, sizeof(THeader));
  CopyMemory(tb, @aHeader, sizeof(THeader));

  // showmessage(Description);
  // tb := StringBytesToTBytes('f9beb4d976657273696f6e00000000006600000094ba3d9b');
  // tb := StringBytesToTBytes('9c');
  fipwIPPort1.send(tb);

  tb := StringBytesToTBytes('80110100' + '0804000000000000' +
    '0defbc6100000000090400000000000000000000000000000000ffff23c121bf208d0804000000000000000000000000000000000000000000000000fdba00e1964f2006102f5361746f7368693a32322e302e302f25c2030001');
  fipwIPPort1.send(tb);

end;

procedure TBTCPeerNode.VerackMessage(Sender: TObject);
begin
  fConnected := true;
end;

end.
