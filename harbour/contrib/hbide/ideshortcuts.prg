/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                              Harbour IDE
 *
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                               04Apr2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "hbqt.ch"
#include "common.ch"
#include "hbclass.ch"

/*----------------------------------------------------------------------*/

CLASS IdeShortcuts INHERIT IdeObject

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()

   METHOD evalMacro( cString )
   METHOD fetchAndExecMacro()

   METHOD getWord( lSelect )
   METHOD getLine( lSelect )
   METHOD getText()
   METHOD execTool( ... )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:execTool( ... )
   RETURN ::oTM:execTool( ... )

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:getWord( lSelect )
   RETURN ::oEM:getWord( lSelect )

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:getLine( lSelect )
   RETURN ::oEM:getLine( lSelect )

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:getText()
   RETURN ::oEM:getText()

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:evalMacro( cString )
   LOCAL bError := ErrorBlock( {|o| break( o ) } )
   LOCAL oErr, bBlock, n, cBlock, cParam

   IF ( n := at( "|", cString ) ) > 0
      cString := substr( cString, n + 1 )
      IF ( n := at( "|", cString ) ) == 0
         RETURN Self
      ENDIF
      cParam  := substr( cString, 1, n - 1 )
      cString := substr( cString, n + 1 )
      cBlock  := "{|o," + cParam + "|" + cString + " }"
   ELSE
      cBlock := "{|o| " + cString + " }"
   ENDIF
   cBlock := strtran( cBlock, "::", "o:" )

   bBlock := &( cBlock )

hbide_dbg( cBlock )
   BEGIN SEQUENCE
      eval( bBlock, self )
   RECOVER USING oErr
      MsgBox( "Wrongly defined block. Syntax is |var| method_call( var ) --- " + oErr:description )
   END SEQUENCE

   ErrorBlock( bError )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeShortcuts:fetchAndExecMacro()
   LOCAL cStr

   cStr := hbide_fetchAString( ::oDlg:oWidget, "", "Macro", "Compilation" )
   IF !empty( cStr )
      ::evalMacro( cStr )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

