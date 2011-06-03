/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                            Harbour-Qt IDE
 *
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                               25May2011
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"

/*----------------------------------------------------------------------*/

CLASS IdeChangeLog INHERIT IdeObject

   DATA   aLog                                    INIT {}
   DATA   cUser                                   INIT ""
   DATA   nCntr                                   INIT 0

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD execEvent( cEvent, p )
   METHOD updateLog( cLogFile )
   METHOD refresh()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeChangeLog:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeChangeLog:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeChangeLog:destroy()

   IF !empty( ::oUI )
      ::oUI:q_buttonNew   :disconnect( "clicked()" )

      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeChangeLog:show()

   IF empty( ::oUI )
      ::oUI := hbide_getUI( "changelog", ::oDlg:oWidget )
      ::oUI:setWindowFlags( Qt_Sheet )
      ::oUI:setWindowIcon( hbide_image( "hbide" ) )

      ::oUI:q_buttonChangelog :setIcon( hbide_image( "dc_folder"  ) )
      ::oUI:q_buttonAddSrc    :setIcon( hbide_image( "dc_plus"  ) )

      ::oUI:q_buttonChangelog :connect( "clicked()", {|| ::execEvent( "buttonChangelog_clicked"         ) } )
      ::oUI:q_buttonAddSrc    :connect( "clicked()", {|| ::execEvent( "buttonAddSrc_clicked"            ) } )
      ::oUI:q_buttonDone      :connect( "clicked()", {|| ::execEvent( "buttonDone_clicked"              ) } )
      ::oUI:q_buttonRefresh   :connect( "clicked()", {|| ::execEvent( "buttonRefresh_clicked"           ) } )
      ::oUI:q_buttonSave      :connect( "clicked()", {|| ::execEvent( "buttonSave_clicked"              ) } )

      ::oUI:q_editChangelog   :connect( "textChanged(QString)", {|p| ::execEvent( "editChangelog_textChanged", p   ) } )

      ::updateLog( ::oINI:cChangeLog )

      ::cUser := hbide_fetchAString( ::oDlg:oWidget, , , "Developer Name" )
   ENDIF

   ::oUI:show()

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_getLogCounter( cBuffer )
   LOCAL n, n1, nCntr := 0

   IF ( n := at( "$<", cBuffer ) ) > 0
      n1 := at( ">", cBuffer )
      nCntr := val( substr( cBuffer, n + 2, n1 - n - 2 ) )
   ENDIF

   RETURN nCntr

/*----------------------------------------------------------------------*/

METHOD IdeChangeLog:execEvent( cEvent, p )
   LOCAL cTmp, cTmp1, s, n

   HB_SYMBOL_UNUSED( p )

   SWITCH cEvent

   CASE "buttonSave_clicked"
      IF !empty( cTmp := ::oUI:q_plainLogEntry:toPlainText() )
         cTmp1 := hb_memoread( ::oINI:cChangeLog )
         ::nCntr := hbide_getLogCounter( cTmp1 )
         s := "$<" + strzero( ::nCntr, 6 ) + "> " + hbide_dtosFmt() + " " + time() + " " + ::cUser

         IF ( n := at( "$<", cTmp1 ) ) > 0
            cTmp1 := substr( cTmp1, 1, n - 1 ) + hb_eol() + s + hb_eol() + cTmp + hb_eol() + substr( cTmp1, n )
         ELSE
            cTmp1 += hb_eol() + s + hb_eol() + cTmp + hb_eol()
         ENDIF
         hb_memowrit( ::oINI:cChangeLog, cTmp1 )  /* TODO: put it under locking protocol */

         ::aLog := {}
         ::oUI:q_plainLogEntry:setPlainText( "" )
         ::oUI:q_plainCurrentLog:setPlainText( "" )
      ENDIF

      EXIT
   CASE "buttonRefresh_clicked"
      ::refresh()
      EXIT
   CASE "buttonDone_clicked"
      IF !empty( cTmp := ::oUI:q_plainCurrentLog:toPlainText() )
         aadd( ::aLog, { "Desc", cTmp, "" } )
         ::oUI:q_plainLogEntry:setPlainText( "" )
         ::refresh()
      ENDIF
      EXIT
   CASE "buttonAddSrc_clicked"
      IF !empty( cTmp := ::oUI:q_editSource:text() )
         aadd( ::aLog, { "Source", cTmp, "" } )
         ::refresh()
      ENDIF
      EXIT
   CASE "buttonChangelog_clicked"
      cTmp := hbide_fetchAFile( ::oDlg, "Select a ChangeLog File" )
      ::updateLog( cTmp )
      EXIT
   CASE "editChangelog_textChanged"
      ::updateLog( p )
      EXIT

   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeChangeLog:updateLog( cLogFile )

   IF !empty( cLogFile ) .AND. hb_fileExists( cLogFile )
      ::oUI:q_editChangelog:setStyleSheet( "" )
      ::oINI:cChangeLog := cLogFile
      ::oUI:q_editChangelog:setText( cLogFile )

      ::oUI:q_plainChangelog:clear()
      ::oUI:q_plainChangelog:setPlainText( memoread( cLogFile ) )
      ::refresh()
   ELSE
      ::oUI:q_editChangelog:setStyleSheet( "background-color: rgba( 240,120,120,255 );" )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_dtosFmt( dDate )
   LOCAL s

   DEFAULT dDate TO date()

   s := dtos( dDate )

   RETURN substr( s, 1, 4 ) + "-" + substr( s, 5, 2 ) + "-" + substr( s, 7, 2 )

/*----------------------------------------------------------------------*/


METHOD IdeChangeLog:refresh()
   LOCAL s := "", a_

   ::oUI:q_plainLogEntry:clear()
   #if 0
   ::nCntr := hbide_getlogCounter( hb_memoread( ::oINI:cChangeLog ) )
   s := "$<" + strzero( ::nCntr, 6 ) + "> " + hbide_dtosFmt() + " " + time() + " " + ::cUser
   #endif
   FOR EACH a_ IN ::aLog
      IF a_[ 1 ] == "Source"
         s += hb_eol() + "  * " + a_[ 2 ]

      ELSEIF a_[ 1 ] == "Desc"
         s += hb_eol() + "    ! " + a_[ 2 ]

      ENDIF
   NEXT

   ::oUI:q_plainLogEntry:clear()
   ::oUI:q_plainLogEntry:setPlainText( s )

   RETURN Self

/*----------------------------------------------------------------------*/
