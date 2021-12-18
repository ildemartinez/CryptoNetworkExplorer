unit BTCThreadMonitorUnit;

interface

uses
  System.SysUtils, Winapi.Windows, Winapi.Messages,
  classes, dialogs, ipwcore, ipwtypes, ipwipport, BTCTypes;

type



  TBTCThreadMonitor = class(TThread)
  strict private
    fparent: tcomponent;

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

    procedure DoMessageDetected(const aHeader: THeader);

  public
    constructor Create(const Peer: string; parent: tcomponent);
    destructor Destroy; override;
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

uses
  BTCMonitorUnit;

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

constructor TBTCThreadMonitor.Create(const Peer: string; parent: tcomponent);
begin
  inherited Create;

  fparent := parent;
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
  k, j: Integer;
  status: Integer;
  amessage: string;
  alength, checksum: cardinal;
  payload : TBytes;
  ppayload : ^byte;
  aHeader : THeader;
  command : string;
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
          amessage := '';

          j := 0;
          while (j < 12) and (TextB[j + k] <> 0) do
          begin
            amessage := amessage + char(TextB[k + j]);
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

          /// ojo, solo el primer byte... añadir los siguientes

          // showmessage(inttostr(alength));

          k := k + 3;

          inc(status);
        end;
      // checksum detection
      6:
        begin

          /// Please get checksum
          checksum := TextB[k] + TextB[k + 1] + TextB[k + 2] + TextB[k + 3];

          k := k + 3;

          setlength(payload,alength);
          CopyMemory(payload, @TextB[k], alength);


          DoMessageDetected(BuildHeader(amessage));
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

procedure TBTCThreadMonitor.DoMessageDetected(const aHeader : THeader);
begin
  Synchronize(
    procedure
    begin
      TBTCMonitorComponent(fparent).PrintMessage(fPeer+' '+aHeader.command_name);
    end)
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
  pHeader: ^THeader;
  aHeader : THeader;
begin
  aHeader := BuildHeader('version');

  (*new(pHeader);
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
  ipwIPPort1.send(tb);

  tb := StringBytesToTBytes
    ('8011010008040000000000000defbc6100000000090400000000000000000000000000000000ffff23c121bf208d0804000000000000000000000000000000000000000000000000fdba00e1964f2006102f5361746f7368693a32322e302e302f25c2030001');
  ipwIPPort1.send(tb);

end;

end.
