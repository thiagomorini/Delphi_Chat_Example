program PrjClient;

uses
  Vcl.Forms,
  ClientMain in 'ClientMain.pas' {FClient},
  Client in 'Client.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFClient, FClient);
  Application.Run;
end.
