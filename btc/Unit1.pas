unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BTCMonitorUnit,
  Vcl.ExtCtrls, Vcl.AppEvnts, ipwcore, ipwtypes, ipwipport;

type
  TForm1 = class(TForm)

    Memo1: TMemo;
    TrayIcon1: TTrayIcon;
    ApplicationEvents1: TApplicationEvents;
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);

  private
    { Private declarations }
    fBTCMonitorComponent : TBTCMonitorComponent;
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure PrintMessage(const am : string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ApplicationEvents1Minimize(Sender: TObject);
begin
  { Hide the window and set its state variable to wsMinimized. }
  hide();
  WindowState := wsMinimized;
  { Show the animated tray icon and also a hint balloon. }
  TrayIcon1.Visible := True;
  TrayIcon1.Animate := True;
  TrayIcon1.ShowBalloonHint;
end;


constructor TForm1.Create(Owner: TComponent);
begin
  inherited;

  TrayIcon1.Hint := 'BTC Agent';
 // hide();
  //WindowState := wsMinimized;

  { Set up a hint balloon. }
  TrayIcon1.BalloonTitle := 'Restoring the window.';
  TrayIcon1.BalloonHint :=
    'Double click the system tray icon to restore the window.';
  TrayIcon1.BalloonFlags := bfInfo;
  TrayIcon1.ShowBalloonHint();

  { Show the animated tray icon and also a hint balloon. }
  TrayIcon1.Visible := True;
  TrayIcon1.Animate := True;
  TrayIcon1.ShowBalloonHint;

  fBTCMonitorComponent :=TBTCMonitorComponent.Create(self);

end;

destructor TForm1.Destroy;
begin
  fBTCMonitorComponent.free;
  inherited;
end;

procedure TForm1.PrintMessage(const am: string);
begin
  memo1.lines.add(am);
end;

procedure TForm1.TrayIcon1DblClick(Sender: TObject);
begin
  { Hide the tray icon and show the window,
    setting its state property to wsNormal. }
  TrayIcon1.Visible := False;
  Show();
  WindowState := wsNormal;
  Application.BringToFront();
end;

end.
