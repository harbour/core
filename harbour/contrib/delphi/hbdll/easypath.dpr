{*
 * $id$
 *}

program EasyPath;

uses
  Forms,
  Main in 'Main.pas' {Main_FRM};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMain_FRM, Main_FRM);
  Application.Run;
end.
