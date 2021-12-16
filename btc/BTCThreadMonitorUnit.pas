unit BTCThreadMonitorUnit;

interface

uses
  classes, dialogs;

type

  TBTCThreadMonitor = class(TThread)
  strict protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TBTCThreadMonitor }

constructor TBTCThreadMonitor.Create;
begin
  inherited;

  // showmessage('creado');
end;

destructor TBTCThreadMonitor.Destroy;
begin
  showmessage('die');
  inherited;
end;

procedure TBTCThreadMonitor.Execute;
begin
  inherited;

  while not self.Terminated do
    Sleep(10 * random(10));
end;

end.
