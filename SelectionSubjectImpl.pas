unit SelectionSubjectImpl;

interface

uses
  Classes,
  SelectionSubjectIntf,
  SubjectImpl,
//  ObserverIntf,
  AcceptSelectionObserverIntf;

type
  TSelectionSubject = class(TInterfacedObject, ISelectionSubject)
  protected
    fController: Pointer;
    fObservers: IInterfaceList;
    fUpdateCount: Integer;
    procedure Attach(const Observer: IAcceptSelectionObserver);
    procedure BeginUpdate;
    procedure Detach(const Observer: IAcceptSelectionObserver);
    procedure EndUpdate;
    function GetController: IInterface;
    procedure Notify; virtual;
  public
    constructor Create(const Controller: IInterface);
    destructor Destroy; override;
  end;

implementation

//uses
//  AcceptSelectionObserverIntf;

{ TSelectionSubject }

constructor TSelectionSubject.Create(const Controller: IInterface);
begin
  inherited Create;
  fController := Pointer(Controller);
end;

destructor TSelectionSubject.Destroy;
begin
  inherited;
end;

{ private methods }

procedure TSelectionSubject.Attach(const Observer: IAcceptSelectionObserver);
begin
  if fObservers = nil then
    fObservers := TInterfaceList.Create;
  if fObservers.IndexOf(Observer) < 0 then
    fObservers.Add(Observer);
end;

procedure TSelectionSubject.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TSelectionSubject.Detach(const Observer: IAcceptSelectionObserver);
begin
  if fObservers <> nil then
  begin
    fObservers.Remove(Observer);
    if fObservers.Count = 0 then
      fObservers := nil;
  end;
end;

procedure TSelectionSubject.EndUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount = 0 then
    Notify;
end;

function TSelectionSubject.GetController: IInterface;
begin
  Result := IInterface(fController);
end;

procedure TSelectionSubject.Notify;
var
  i: Integer;
begin
  if fObservers <> nil then
    for i := 0 to Pred(fObservers.Count) do
      (fObservers[i] as IAcceptSelectionObserver).UpdateAcceptedSelection(GetController);
end;

end.
 