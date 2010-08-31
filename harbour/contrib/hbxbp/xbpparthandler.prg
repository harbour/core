/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                  Pritpal Bedi <pritpal@vouchcac.com>
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
   METHOD   childFromName( nNameId )
   METHOD   childList()
   METHOD   delChild( oXbp )
   METHOD   setName( nNameId )
   METHOD   setOwner( oXbp )
   METHOD   setParent( oXbp )

   METHOD   notifier()

   DATA     aChildren                             INIT    {}
   DATA     nNameId
   DATA     oParent
   DATA     oOwner
   DATA     nStatus                               INIT    NIL

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD init( oParent, oOwner ) CLASS XbpPartHandler

   ::oParent := oParent
   ::oOwner  := oOwner
   ::status  := XBP_STAT_INIT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD create( oParent, oOwner ) CLASS XbpPartHandler

   DEFAULT oParent TO ::oParent
   DEFAULT oOwner  TO ::oOwner

   ::oParent := oParent
   ::oOwner  := oOwner

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD configure( oParent, oOwner ) CLASS XbpPartHandler

   DEFAULT oParent TO ::oParent
   DEFAULT oOwner  TO ::oOwner

   ::oParent := oParent
   ::oOwner  := oOwner

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD destroy() CLASS XbpPartHandler

   ::aChildren  := NIL
   ::nNameId    := NIL
   ::oParent    := NIL
   ::oOwner     := NIL
   ::nStatus    := NIL
   ::status     := XBP_STAT_FAILURE

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD handleEvent( hEvent, mp1, mp2 ) CLASS XbpPartHandler

   HB_SYMBOL_UNUSED( hEvent )
   HB_SYMBOL_UNUSED( mp1 )
   HB_SYMBOL_UNUSED( mp2 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD status( nStatus ) CLASS XbpPartHandler
   LOCAL nOldStatus := ::nStatus

   IF hb_isNumeric( nStatus )
      ::nStatus := nStatus
   ENDIF

   RETURN nOldStatus

/*----------------------------------------------------------------------*/

METHOD addChild( oXbp ) CLASS XbpPartHandler

   oXbp:nNameID := oXbp:nID
   aadd( ::aChildren, oXbp )

   IF __objHasMsg( Self, "QLAYOUT" ) .AND. !empty( ::qLayout )
      ::qLayout:addWidget( oXbp:oWidget )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD addAsChild() CLASS XbpPartHandler

   IF !empty( ::oParent )
      ::oParent:addChild( Self )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD childFromName( nNameId ) CLASS XbpPartHandler
   LOCAL i, oXbp

   FOR i := 1 TO len( ::aChildren )
      IF ::aChildren[ i ]:nNameID <> NIL .and. ::aChildren[ i ]:nNameID == nNameID
         oXbp := ::aChildren[ i ]
      ENDIF
   NEXT

   RETURN oXbp

/*----------------------------------------------------------------------*/

METHOD childList() CLASS XbpPartHandler

   RETURN ::aChildren

/*----------------------------------------------------------------------*/

METHOD delChild( oXbp ) CLASS XbpPartHandler
   LOCAL n

   n := ascan( ::aChildren, {|o| o == oXbp } )
   IF n > 0
      oXbp:destroy()
      adel( ::aChildren, n )
      asize( ::aChildren, len( ::aChildren )-1 )
   endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD setName( nNameId ) CLASS XbpPartHandler
   LOCAL nOldNameId := ::nNameId

   IF Valtype( nNameId ) == "N"
      ::nNameID := nNameId
   ENDIF

   RETURN nOldNameId

/*----------------------------------------------------------------------*/

METHOD setOwner( oXbp ) CLASS XbpPartHandler
   LOCAL oOldXbp := ::oOwner

   IF valtype( oXbp ) == "O"
      ::oOwner := oXbp
   ENDIF

   RETURN oOldXbp

/*----------------------------------------------------------------------*/

METHOD setParent( oXbp ) CLASS XbpPartHandler
   LOCAL oOldXbp := ::oParent

   IF valtype( oXbp ) == "O"
      ::oParent := oXbp
   ENDIF

   RETURN oOldXbp

/*----------------------------------------------------------------------*/

METHOD notifier() CLASS XbpPartHandler

   RETURN self

/*----------------------------------------------------------------------*/
