/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2008-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
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
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                Xbase++ Compatible xbpPartHandler Class
 *
 *                             Pritpal Bedi
 *                               08Nov2008
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "xbp.ch"

/*----------------------------------------------------------------------*/

CLASS XbpPartHandler

   DATA     cargo

   METHOD   init( oParent, oOwner )
   METHOD   create( oParent, oOwner )
   METHOD   configure( oParent, oOwner )
   METHOD   destroy()
   METHOD   handleEvent( hEvent, mp1, mp2 )
   METHOD   status( nStatus )                     SETGET

   METHOD   addChild( oXbp )
   METHOD   addAsChild()
   METHOD   addAsOwned( oXbp )
   METHOD   childFromName( nNameId )
   METHOD   childList()
   METHOD   delChild( oXbp )
   METHOD   delOwned( oXbp )
   METHOD   setName( nNameId )
   METHOD   setOwner( oOwner )
   METHOD   setParent( oParent )
   METHOD   moveOwned( nOffSetX, nOffSetY )

   METHOD   notifier()

   DATA     aChildren                             INIT    {}
   DATA     a_Owned                               INIT    {}
   DATA     nNameId
   DATA     oParent
   DATA     oOwner
   DATA     nStatus                               INIT    NIL

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:init( oParent, oOwner )

   ::oParent := oParent
   ::oOwner  := oOwner
   ::status  := XBP_STAT_INIT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:create( oParent, oOwner )

   DEFAULT oParent TO ::oParent
   DEFAULT oOwner  TO ::oOwner

   ::oParent := oParent
   ::oOwner  := oOwner

   // DEFAULT ::oOwner TO ::oParent

   IF hb_isObject( ::oOwner )
      ::oOwner:addAsOwned( Self )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:configure( oParent, oOwner )

   DEFAULT oParent TO ::oParent
   DEFAULT oOwner  TO ::oOwner

   ::oParent := oParent
   ::oOwner  := oOwner

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:destroy()

   ::aChildren  := NIL
   ::nNameId    := NIL
   ::oParent    := NIL
   ::oOwner     := NIL
   ::nStatus    := NIL
   ::status     := XBP_STAT_FAILURE

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:handleEvent( hEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( hEvent )
   HB_SYMBOL_UNUSED( mp1 )
   HB_SYMBOL_UNUSED( mp2 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:status( nStatus )
   LOCAL nOldStatus := ::nStatus

   IF hb_isNumeric( nStatus )
      ::nStatus := nStatus
   ENDIF

   RETURN nOldStatus

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:addChild( oXbp )

   oXbp:nNameID := oXbp:nID
   aadd( ::aChildren, oXbp )

   IF __objHasMsg( Self, "QLAYOUT" ) .AND. !empty( ::qLayout )
      ::qLayout:addWidget( oXbp:oWidget )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:addAsChild()

   IF !empty( ::oParent )
      ::oParent:addChild( Self )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:addAsOwned( oXbp )

   IF ! empty( oXbp )
      aadd( ::a_Owned, oXbp )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:childFromName( nNameId )
   LOCAL i

   FOR i := 1 TO len( ::aChildren )
      IF ::aChildren[ i ]:nNameID <> NIL .AND. ::aChildren[ i ]:nNameID == nNameID
         RETURN ::aChildren[ i ]
      ENDIF
   NEXT

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:childList()

   RETURN ::aChildren

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:delChild( oXbp )
   LOCAL n

   n := ascan( ::aChildren, {|o| o == oXbp } )
   IF n > 0
      oXbp:oOwner:delOwned( oXbp )
      oXbp:destroy()
      hb_adel( ::aChildren, n, .t. )
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:delOwned( oXbp )
   LOCAL n

   IF ( n := ascan( ::a_Owned, {|o| o == oXbp } ) ) > 0
      hb_adel( ::a_Owned, n, .t. )
      IF empty( ::a_Owned )
         ::a_Owned := {}
      ENDIF
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:setName( nNameId )
   LOCAL nOldNameId := ::nNameId

   IF Valtype( nNameId ) == "N"
      ::nNameID := nNameId
   ENDIF

   RETURN nOldNameId

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:setOwner( oOwner )
   LOCAL oOldXbp := ::oOwner

   IF valtype( oOwner ) == "O"
      ::oOwner := oOwner
   ENDIF

   RETURN oOldXbp

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:setParent( oParent )
   LOCAL oRect
   LOCAL oOldXbp := ::oParent

   IF valtype( oParent ) == "O"
      IF __objHasMsg( Self, "OMDI" )
         IF ! empty( ::oMdi )
            ::oParent:oWidget:removeSubWindow( ::oWidget )
            ::oWidget:setWindowFlags( ::nFlags )
            ::oMdi:close()
            ::oMdi := NIL
         ELSEIF __objGetClsName( oParent ) == "XBPDRAWINGAREA"
            oRect := ::oWidget:frameGeometry()
            ::oMdi := oParent:oWidget:addSubWindow( ::oWidget )
            ::oMdi:resize( oRect:width(), oRect:height() )
         ENDIF
         ::oWidget:show()
      ENDIF
      ::oParent := oParent
   ENDIF

   RETURN oOldXbp

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:notifier()

   RETURN self

/*----------------------------------------------------------------------*/

METHOD XbpPartHandler:moveOwned( nOffSetX, nOffSetY )
   LOCAL oXbp, oPos

   FOR EACH oXbp IN ::a_Owned
      IF __objHasMsg( oXbp, "MOVEWITHOWNER" ) .AND. oXbp:moveWithOwner
         IF !( oXbp:oParent:className() == "XBPDRAWINGAREA" )
            oPos := oXbp:oWidget:pos()
            oXbp:oWidget:move( oPos:x() + nOffSetX, oPos:y() + nOffSetY )
         ENDIF
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

