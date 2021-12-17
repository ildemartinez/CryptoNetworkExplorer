unit BTCThreadMonitorUnit;

interface

uses
  System.SysUtils, Winapi.Windows, Winapi.Messages,
  classes, dialogs, ipwcore, ipwtypes, ipwipport;

type

  TBTCThreadMonitor = class(TThread)
  strict private
    ipwIPPort1: TipwIPPort;
    fPeer: string;
  strict protected
    procedure Execute; override;

    procedure ipwIPPort1Connected(Sender: TObject; StatusCode: Integer;
      const Description: string);

    procedure DataIn(Sender: TObject; Text: String; TextB: TBytes;
      EOL: Boolean);
    procedure ConnectionStatus(Sender: TObject; const ConnectionEvent: string;
      StatusCode: Integer; const Description: string);
    procedure ReadyToSend(Sender: TObject);
    procedure Error(Sender: TObject; ErrorCode: Integer;
      const Description: string);
  public
    constructor Create(const Peer: string);
    destructor Destroy; override;
  end;

  Header = record
    start_string: uint32;
    command_name: array [1 .. 12] of byte;
    payload_size: uint32;
    checksum: uint32;
  end;

  Net_addr = record
    services: int64;
    ipv64: array [1 .. 16] of char;
    port: byte;
  end;

  BTCPayload = record
    version: Int32;
    services: int64;
    timestamp: int64;
    addr_recv: Net_addr;
  end;

implementation

function StringBytesToTBytes(const s: string): TBytes;
var
  k: Integer;
begin
  SetLength(result, length(s) div 2);
  for k := 0 to (s.length div 2) - 1 do
    result[k] := StrToInt('$' + s[(k * 2) + 1] + s[(k + 1) * 2]);
end;

procedure TBTCThreadMonitor.ConnectionStatus(Sender: TObject;
  const ConnectionEvent: string; StatusCode: Integer;
  const Description: string);
begin
  // showmessage(ConnectionEvent);
end;

constructor TBTCThreadMonitor.Create(const Peer: string);
begin
  inherited Create;

  fPeer := Peer;

  ipwIPPort1 := TipwIPPort.Create(nil);
  ipwIPPort1.OnConnected := ipwIPPort1Connected;
  ipwIPPort1.OnDataIn := DataIn;
  ipwIPPort1.OnConnectionStatus := ConnectionStatus;
  ipwIPPort1.OnReadyToSend := ReadyToSend;
  ipwIPPort1.OnError := Error;

end;

procedure TBTCThreadMonitor.DataIn(Sender: TObject; Text: String; TextB: TBytes;
  EOL: Boolean);
var
  k: Integer;
  status: Integer;
begin

  k := 0;
  status := 0;

  while k < length(TextB) do
  begin
    case status of
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
      4:
        begin
          showmessage('packeeet');
          status := 0;
        end;

    end;

    inc(k);

  end;

end;

destructor TBTCThreadMonitor.Destroy;
begin
  ipwIPPort1.Disconnect;

  inherited;
end;

procedure TBTCThreadMonitor.Error(Sender: TObject; ErrorCode: Integer;
  const Description: string);
begin

end;

procedure TBTCThreadMonitor.Execute;
begin
  inherited;

  ipwIPPort1.Connect(fPeer, 8333);
end;

procedure TBTCThreadMonitor.ipwIPPort1Connected(Sender: TObject;
  StatusCode: Integer; const Description: string);
begin
end;

procedure TBTCThreadMonitor.ReadyToSend(Sender: TObject);
var
  tb: TBytes;
  pHeader: ^Header;
  C: string;
begin
  new(pHeader);
  pHeader^.start_string := $D9B4BEF9;

  pHeader^.command_name[1] := $76;
  pHeader^.command_name[2] := $65;
  pHeader^.command_name[3] := $72;
  pHeader^.command_name[4] := $73;
  pHeader^.command_name[5] := $69;
  pHeader^.command_name[6] := $6F;
  pHeader^.command_name[7] := $6E;
  pHeader^.command_name[8] := 0;
  pHeader^.command_name[9] := 0;
  pHeader^.command_name[10] := 0;
  pHeader^.command_name[11] := 0;
  pHeader^.command_name[12] := 0;

  pHeader^.payload_size := $66;
  pHeader^.checksum := $9B3DBA94;

  SetLength(tb, sizeof(Header));
  CopyMemory(tb, pHeader, sizeof(Header));

  // showmessage(Description);
  // tb := StringBytesToTBytes('f9beb4d976657273696f6e00000000006600000094ba3d9b');
  // tb := StringBytesToTBytes('9c');
  ipwIPPort1.send(tb);

  tb := StringBytesToTBytes
    ('8011010008040000000000000defbc6100000000090400000000000000000000000000000000ffff23c121bf208d0804000000000000000000000000000000000000000000000000fdba00e1964f2006102f5361746f7368693a32322e302e302f25c2030001');
  ipwIPPort1.send(tb);

end;

end.
