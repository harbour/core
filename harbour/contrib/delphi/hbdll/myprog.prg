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

#Include 'HbClass.ch'

MEMVAR CallBackResult

FUNCTION MakeIndex( cFileName, cField )
PUBLIC CallBackResult := ''

Use (cFileName) Alias FIL
Index on &(cField) to (cFileName) EVAL IndexStatus() EVERY LastRec()/10
Close FIL
RETURN 'The file '+cFileName+' has been indexed'

FUNCTION IndexStatus
LOCAL cCompleted := LTrim( Str(Int((RecNo()/LastRec()) * 100)) ), nSeconds
D(cCompleted)

nSeconds := Seconds()          // Let's make this thing to go slowly
WHILE nSeconds+1 >= Seconds()
END

RETURN .T.

//---------------------------------------//
//            Library functions          //
//---------------------------------------//

FUNCTION D( cString )
CallBack( cString )
RETURN CallBackResult

FUNCTION MacroCall( cString )
LOCAL xMacroResult, cRtnType, cRtnVal

cRtnType := ValType( xMacroResult := &(cString) )

// Well, if memory is like a string, why not to use strings
// for any type of vars?...  ok, ok, I know. :-)

Do Case
Case cRtnType = 'C'
   cRtnVal := 'C'+xMacroResult
Case cRtnType = 'N'
   If Int(xMacroResult) = xMacroResult
      cRtnVal := 'I'+AllTrim(Str(xMacroResult,,0))
   Else
      cRtnVal := 'F'+AllTrim(Str(xMacroResult))
   EndIf
Case cRtnType = 'L'
   cRtnVal := 'L'+If(xMacroResult,'True','False')
Case cRtnType = 'D'
   cRtnVal := 'D'+DtoC(xMacroResult)
OtherWise
   cRtnVal := NIL  // NOTE: If Delphi doesn't expect a result, returning any result
                   //       will corrupt Delphi's memory.  So this is set to NIL.
EndCase

RETURN cRtnVal

