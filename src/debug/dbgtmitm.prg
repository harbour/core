/*
 * Harbour Project source code:
 * The Debugger (HBDbMenuItem Class)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszakats] */

#pragma -b-

#define HB_CLS_NOTOBJECT      /* do not inherit from HBObject calss */
#include "hbclass.ch"

CREATE CLASS HBDbMenuItem

   VAR nRow
   VAR nCol
   VAR cPrompt
   VAR bAction
   VAR lChecked
   VAR Ident

   ACCESS Checked()           INLINE ::lChecked
   ASSIGN Checked( lChecked ) INLINE ::lChecked := lChecked

   METHOD New( cPrompt, bAction, lChecked, xIdent )
   METHOD Display( cClrText, cClrHotKey )
   METHOD Toggle() INLINE ::lChecked := ! ::lChecked

ENDCLASS

METHOD New( cPrompt, bAction, lChecked, xIdent ) CLASS HBDbMenuItem

   hb_default( @lChecked, .F. )

   ::cPrompt  := cPrompt
   ::bAction  := bAction
   ::lChecked := lChecked
   ::Ident    := xIdent

   RETURN Self

METHOD Display( cClrText, cClrHotKey ) CLASS HBDbMenuItem

   LOCAL nAt

   hb_DispOutAt( ::nRow, ::nCol, StrTran( ::cPrompt, "~" ), cClrText )

   hb_DispOutAt( ::nRow, ::nCol + ;
      ( nAt := At( "~", ::cPrompt ) ) - 1, ;
      SubStr( ::cPrompt, nAt + 1, 1 ), cClrHotKey )

   IF ::lChecked
      hb_DispOutAtBox( ::nRow, ::nCol, hb_UTF8ToStrBox( "√" ), cClrText )
   ENDIF

   RETURN Self
