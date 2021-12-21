unit ISubjectUnit;

interface

uses
  BTCAgentUnit;

type
  IObserver = interface
    ['{25E3E49D-F67A-4E5E-8ECE-E98C19ED99D4}']
    procedure NewBTCAgentAdded(aBTCAgent : TBTCAgent);
  end;

  ISubject = interface
    ['{ADFAD0D5-EE82-4A82-8305-3896AD218D51}']
    procedure RegisterObserver(aObserver : IObserver);
   // procedure UnRegisterObserver(aObserver : IObserver);
   procedure NotifyNewBTCAgent(const aBTCAgent : TBTCAgent);
  end;

implementation

end.
