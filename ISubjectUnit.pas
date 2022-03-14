unit ISubjectUnit;

interface

uses
  BTCPeerNodeUnit;

type
  // Network observer pattern
  INetworkObserver = interface
    ['{25E3E49D-F67A-4E5E-8ECE-E98C19ED99D4}']
    procedure NewBTCAgentAdded(aBTCAgent: TBTCPeerNode);
    procedure NodeConnected(aBTCAgent: TBTCPeerNode);
  end;

  ISubject = interface
    ['{ADFAD0D5-EE82-4A82-8305-3896AD218D51}']
    // procedure UnRegisterObserver(aObserver : IObserver);
    procedure NotifyNewBTCAgent(const aBTCAgent: TBTCPeerNode);
    procedure NotifyNodeConnected(const aBTCAgent: TBTCPeerNode);
    procedure RegisterObserver(aObserver: INetworkObserver);
  end;

implementation

end.
