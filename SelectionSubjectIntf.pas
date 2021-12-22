unit SelectionSubjectIntf;

interface

uses
  SubjectIntf,
  AcceptSelectionObserverIntf;
  
type
  ISelectionSubject = interface
  ['{4109D7F1-F49C-4D7F-94B9-2B3E9C9D9DF6}']
    procedure Attach(const Observer: IAcceptSelectionObserver);
    procedure BeginUpdate;
    procedure Detach(const Observer: IAcceptSelectionObserver);
    procedure EndUpdate;
    procedure Notify;
  end;

implementation

end.
 