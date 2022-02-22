program PrjServer;

uses
  Vcl.Forms,
  ServerMain in 'ServerMain.pas' {FMain},
  Server in 'Server.pas',
  Database in 'Database.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
