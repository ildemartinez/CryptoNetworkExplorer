unit NodeObserverPattern;

interface

uses
  classes,
  ipeernodeunit,
  System.Generics.Collections;

type
  // Node Observer pattern
  INodeObserver = interface
    ['{C805DD3C-5A0C-4FD3-A1D5-30DF1BC53835}']
    procedure NodeConnected(aNode: INode);
  end;

  INodeObservable = interface
    ['{1ECE5AAA-F65E-4BF8-8527-75185C73A1D5}']
    procedure RegisterObserver(aObserver: INodeObserver);
    procedure UnregisterObserver(aObserver: INodeObserver);
    procedure Notify(aNode: INode);
  end;

  TNodeSubject = class(TInterfacedObject, INodeObservable)
  private
    fObserverList: Tlist<INodeObserver>;

    destructor Destroy; override;
  public
    procedure RegisterObserver(aObserver: INodeObserver);
    procedure UnregisterObserver(aObserver: INodeObserver);

    procedure Notify(aNode: INode);
  end;

procedure AttachObserverToSubject(aObserver: INodeObserver;
  aSubject: INodeObservable);
procedure DeattachObserverFromSubject(aObserver: INodeObserver;
  aSubject: INodeObservable);

implementation

{ TNodeSubject }

procedure AttachObserverToSubject(aObserver: INodeObserver;
  aSubject: INodeObservable);
begin
  aSubject.RegisterObserver(aObserver);
end;

procedure DeattachObserverFromSubject(aObserver: INodeObserver;
  aSubject: INodeObservable);
begin
  aSubject.UnregisterObserver(aObserver);
end;

destructor TNodeSubject.Destroy;
begin
  fObserverList.free;

  inherited;
end;

procedure TNodeSubject.Notify(aNode: INode);
var
  aObserver: INodeObserver;
begin
  for aObserver in fObserverList do
  begin
    aObserver.NodeConnected(aNode);
  end;
end;

procedure TNodeSubject.RegisterObserver(aObserver: INodeObserver);
begin
  if fObserverList = nil then
    fObserverList := Tlist<INodeObserver>.Create;

  if fObserverList.IndexOf(aObserver) < 0 then
    fObserverList.Add(aObserver);
end;

procedure TNodeSubject.UnregisterObserver(aObserver: INodeObserver);
begin
  if fObserverList <> nil then
    fObserverList.Remove(aObserver);
end;

end.
