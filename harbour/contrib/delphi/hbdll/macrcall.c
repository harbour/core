/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Macro processing requested from Delphi and setting callbacks
 * to interact with Delphi
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
 */

#define HB_OS_WIN_32_USED

#include "hbvm.h"
#include "hbapiitm.h"
#include "hbstack.h"

#if defined(HB_OS_WIN_32)

//  Function pointer type
typedef long (__stdcall *CallBackFuncType)(const char* message);

//  Setter function
HB_EXPORT __stdcall void SetCallBack(CallBackFuncType fun);

char * HB_EXPORT  __stdcall MacroCall( char * sParam )
{
   char *szFunc = "MacroCall";

   PHB_DYNS pDynSym = hb_dynsymFindName( szFunc ); /* The PRG function to use */

   if( pDynSym )
   {

      hb_vmPushSymbol( pDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmPushString( sParam, strlen( sParam ) );

      hb_vmFunction( 1 ); /* as we receive one parameter */

      return hb_stack.Return.item.asString.value;

   }
   else
      return "error ...";

}

static CallBackFuncType cbfun = 0;
void __stdcall SetCallBack(CallBackFuncType fun)
{
  CallBackFuncType oldfun = cbfun;
  cbfun = fun;
  return oldfun;
}

HB_FUNC( CALLBACK )
{
  long result = 0;

  if (cbfun != 0)
  {

     result = cbfun( hb_parc( 1 ) );
  }
  else
  {
      MessageBox( NULL,  //HWINDOW of the window that owns the message box
                  "CallBack failed under MacrCall.c" , //Text
                  "Warning" ,//Title
                  MB_OK | MB_ICONINFORMATION );//Bit mask flags
  }

  hb_retl( result );  // QUESTION: If you can make this to return a char pointer
                      // being interpreted correctly by Delphi,
                      // please inform me, I'm all ears, thank you.
}

HB_FUNC( MSGBOX )
   {
      MessageBox( 0, hb_parc( 1 ), "Warning", 0 );
   }

#endif
