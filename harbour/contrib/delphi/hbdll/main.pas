{*
 * $id$
 *}

{*
 * Harbour Project source code:
 *
 * Copyright 2002 Jorge A. Giraldo S. <jgiraldo@col2.telecom.com.co>
 *                                    <jorgeagiraldo@hotmail.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 *}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls;

type
  TMain_FRM = class(TForm)
    BitBtn1: TBitBtn;
    ProgressBar1: TProgressBar;
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  Main_FRM: TMain_FRM;
  oApplication : TApplication;

type
  CallBackFuncType = function (mesg: PChar): integer; stdcall;

procedure SetCallBack(cbfunc: CallBackFuncType); stdcall;
       external 'MyProg' name 'SetCallBack';

function MacroCall( pParam : pchar ) : pchar; stdcall;
       external 'MyProg' name 'MacroCall';

function h( sParam : String ) : variant;

implementation

{$R *.DFM}

procedure TMain_FRM.BitBtn1Click(Sender: TObject);
begin
  ProgressBar1.Position := 0;
  ShowMessage( h('  MakeIndex("..\..\..\TESTS\TEST", "ZIP")  ') );

end;

function h( sParam : String ) : variant;
Var
   sRtnVal,
   sType,
   sResult : String;
begin
  sRtnVal := MacroCall( PChar(sParam) );
  sType   := Copy(sRtnVal,1,1);
  sResult := Copy(sRtnVal,2,Length(sRtnVal)-1);

  // Changes string result to the expected type
  if sType = 'C' then // is character or string
    result := sResult
  else
  if sType = 'I' then  // integer
    result := StrToInt(sResult)
  else
  if sType = 'F' then  // float
    result := StrToFloat(sResult)
  else
  if sType = 'D' then  // date
    result := StrToDate(sResult)
  else
  if sType = 'L' then  // boolean
    if sResult = 'True' then
      result := true
    else
      result := false;

end;

function ReceiveCallBack(mesg: PChar): integer; stdcall;
const
  nCallsMade : integer = 0;
begin
  Main_FRM.ProgressBar1.Position := StrToInt(String(mesg));
  oApplication.ProcessMessages;
  Inc(nCallsMade);
  // The answer our Harbour program expects.
  // Callbackresult is a Harbour Public variable
  // that is requested when expecting some answer.
  // h(' CallBackResult := "NOANSWER"  ');
  result := 0;
end;

Initialization
begin
  SetCallBack(ReceiveCallBack);
  oApplication := Application;
end;

end.
