unit ClientMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls,

  Client;

type
  TFClient = class(TForm)
    Panel1: TPanel;
    StatusBar: TStatusBar;
    Chat: TMemo;
    edtMessage: TEdit;
    btnSend: TBitBtn;
    Panel2: TPanel;
    Label1: TLabel;
    edtNickname: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtIpAddress: TEdit;
    edtPort: TEdit;
    btnConnect: TBitBtn;
    ListUsers: TListBox;
    btnNickname: TBitBtn;
    procedure btnConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure edtMessageKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure btnNicknameClick(Sender: TObject);
  private
    procedure CheckServer;
    procedure UpdateChat(Sender: TObject; UpdatedLine: string);
    procedure UpdateUsers(Sender: TObject; Users: TStrings);
  public
    Client: TClient;
  end;

var
  FClient: TFClient;

implementation

{$R *.dfm}

procedure TFClient.btnConnectClick(Sender: TObject);
begin
  Client.Host := edtIpAddress.Text;
  Client.Port := StrToInt(edtPort.Text);

  if btnConnect.Caption = 'Connect' then
  begin
    Client.Active := True;
    btnConnect.Caption := 'Disconnect';
  end
  else
  begin
    Client.Active := False;
    btnConnect.Caption := 'Connect';
  end;

  CheckServer;
end;

procedure TFClient.btnNicknameClick(Sender: TObject);
begin
  if not Client.Active then Exit;

  Client.Nickname := edtNickname.Text;
end;

procedure TFClient.btnSendClick(Sender: TObject);
begin
  if (Trim(edtMessage.Text) = '') or not (Client.Active) then Exit;

  Client.Send(ListUsers.ItemIndex-1, '['+ edtNickname.Text + '] says: ' + edtMessage.Text);

  edtMessage.Clear;
  edtMessage.SetFocus;
end;

procedure TFClient.CheckServer;
begin
  if not Assigned(Client) then Exit;

  StatusBar.SimpleText := Client.Status;
  Client.Nickname := edtNickname.Text;
  ListUsers.Clear;
end;

procedure TFClient.edtMessageKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    btnSend.Click;
end;

procedure TFClient.FormCreate(Sender: TObject);
begin
  Client := TClient.Create;
  Client.OnUpdateChat := UpdateChat;
  Client.OnUpdateUsers := UpdateUsers;
  CheckServer;
end;

procedure TFClient.FormDestroy(Sender: TObject);
begin
  if Assigned(Client) then
    FreeAndNil(Client);
end;

procedure TFClient.UpdateChat(Sender: TObject; UpdatedLine: string);
begin
  Chat.Lines.Add(UpdatedLine);
end;

procedure TFClient.UpdateUsers(Sender: TObject; Users: TStrings);
begin
  ListUsers.Clear;
  ListUsers.Items.AddStrings(Users);
  ListUsers.ItemIndex := 0;
end;

end.
