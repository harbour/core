/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Message Line Class
 *
 * Copyright 2002 Larry Sevilla <lsevilla@nddc.edu.ph>
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

#include "hbclass.ch"

#ifdef HB_COMPAT_C53

CLASS MssgLine

   DATA Flag
   DATA Row
   DATA Left
   DATA Right
   DATA Color
   DATA aMsg            // for backwards compatibility

/*
// Graphics support - not yet implemented
   DATA Back1
   DATA Back2
   DATA Fore
   DATA FontCol
   DATA FontRow
*/
   DATA ScreenSaved     PROTECTED

   METHOD New( nRow, nLeft, nRight, cColor )
   METHOD SaveScreen()
   METHOD Show( cMsg )
   METHOD RestScreen()
   MESSAGE Erase() METHOD RestScreen()

ENDCLASS

METHOD New( nRow, nLeft, nRight, cColor ) CLASS MssgLine

   ::Row   := nRow
   ::Left  := nLeft
   ::Right := nRight
   ::Color := cColor

   ::Flag := ( VALTYPE(nRow) + VALTYPE(nLeft) + VALTYPE(nRight) == "NNN" )

   IF !( VALTYPE(cColor) == "C" )
      ::Color := GetClrPair( SetColor(), 1 )
   ENDIF

   ::aMsg := { ::Flag, nRow, nLeft, nRight, ::Color ,,,,, }
                                                 // GUI not yet supported

return Self

METHOD SaveScreen() CLASS MssgLine

   ::ScreenSaved := saveScreen( ::row, ::left, ::row, ::right )

return Self

METHOD RestScreen() CLASS MssgLine

   restScreen( ::row, ::left, ::row, ::right, ::ScreenSaved )

return Self

/***
*
*  ShowMsg() --> NIL
*
***/
METHOD Show( cMsg ) CLASS MssgLine

   local nRow, nCol

   IF ::Right == NIL
      RETURN Self
   ENDIF

   nRow := row()
   nCol := col()

   @ ::row, ::left SAY PadC( cMsg, ::right - ::left + 1 ) COLOR ::Color

   setPos( nRow, nCol )

return Self


CLASS GetMssgLine FROM MssgLine

   METHOD Show( oGet )

ENDCLASS

/***
*
*  ShowGetMsg() --> NIL
*
***/
METHOD Show( oGet ) CLASS GetMssgLine

   local cMsg := IIF( VALTYPE( oGet:Control ) == "O", ;
                               oGet:Control:Message, oGet:Message )

   IF !EMPTY( cMsg )
      ::super:Show( cMsg )
   ENDIF

return Self


CLASS MenuMssgLine FROM MssgLine

   METHOD Show( oMenu, lMode )

ENDCLASS

/***
*
*  ShowMsg( <aMsg>, <lMode> ) --> .T.
*
*  Erase and Show Messages.
*  Erase Message then ShowMsg() if lMode is .T.
*  Only erases Menu Message if lMode is .F.
*  SaveScreen()/RestScreen() is used for the
*  Message area in both text or graphics mode.
*
***/
METHOD Show( oMenu, lMode ) CLASS MenuMssgLine

   LOCAL nCurrent, cMsg := NIL
   LOCAL cSaveColor := SetColor()
   LOCAL mlOldState := MSetCursor( .F. )

   IF ( ValType( oMenu:lOldMsgFlag ) == "L" .AND. oMenu:lOldMsgFlag )
      ::RestScreen()
   ENDIF

   IF lMode
      IF ( ::Flag .AND. ;
         ( nCurrent := oMenu:oMenu:Current ) != 0 )

         IF !EMPTY( cMsg := oMenu:oMenu:GetItem( nCurrent ):Message )
            ::super:show( cMsg )
         ENDIF
      ENDIF

      oMenu:cOldMessage := cMsg
      oMenu:lOldMsgFlag := ::Flag

   ENDIF
   MSetCursor( mlOldState )

   RETURN ( .T. )

#endif

