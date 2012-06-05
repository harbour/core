/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MENUITEM class
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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

#include "hbclass.ch"

#include "button.ch"

/* NOTE: Harbour doesn't support CA-Cl*pper 5.3 GUI functionality, but
         it has all related variables and methods. */

#ifdef HB_COMPAT_C53

CREATE CLASS MENUITEM FUNCTION HBMenuItem

   EXPORTED:

   VAR cargo

   METHOD caption( cCaption ) SETGET
   METHOD checked( lChecked ) SETGET
   METHOD data( boData ) SETGET
   METHOD enabled( lEnabled ) SETGET
   METHOD id( nID ) SETGET
   METHOD message( cMessage ) SETGET
   METHOD shortcut( nShortcut ) SETGET
   METHOD style( cStyle ) SETGET

   VAR __col      INIT -1 AS NUMERIC                    /* NOTE: This is a Harbour extension. */
   VAR __row      INIT -1 AS NUMERIC                    /* NOTE: This is a Harbour extension. */

   METHOD isPopUp()

   METHOD New( cCaption, boData, nShortcut, cMessage, nID ) /* NOTE: This method is a Harbour extension [vszakats] */

   PROTECTED:

   VAR cCaption   INIT ""
   VAR lChecked   INIT .F.
   VAR boData
   VAR lEnabled   INIT .T.
   VAR nID
   VAR cMessage
   VAR nShortcut
   VAR cStyle     INIT Chr( 251 ) + Chr( 16 )

ENDCLASS

METHOD caption( cCaption ) CLASS MENUITEM

   IF cCaption != NIL

      ::cCaption := __eInstVar53( Self, "CAPTION", cCaption, "C", 1001 )

      IF ::cCaption == MENU_SEPARATOR
         ::boData   := NIL
         ::lChecked := .F.
         ::lEnabled := .F.
      ENDIF
   ENDIF

   RETURN ::cCaption

METHOD checked( lChecked ) CLASS MENUITEM

   IF lChecked != NIL .AND. !( ::cCaption == MENU_SEPARATOR )
      ::lChecked := __eInstVar53( Self, "CHECKED", lChecked, "L", 1001 )
   ENDIF

   RETURN ::lChecked

METHOD data( boData ) CLASS MENUITEM

   IF boData != NIL
      IF HB_ISBLOCK( boData )
         ::boData := boData
      ELSE
         ::boData := __eInstVar53( Self, "DATA", boData, "O", 1001, {|| boData:ClassName() $ "POPUPMENU|HB_POPUPMENU" } )
      ENDIF
   ENDIF

   RETURN ::boData

METHOD enabled( lEnabled ) CLASS MENUITEM

   IF lEnabled != NIL .AND. !( ::cCaption == MENU_SEPARATOR )
      ::lEnabled := __eInstVar53( Self, "ENABLED", lEnabled, "L", 1001 )
   ENDIF

   RETURN ::lEnabled

METHOD id( nID ) CLASS MENUITEM

   IF nID != NIL
      ::nID := __eInstVar53( Self, "ID", nID, "N", 1001 )
   ENDIF

   RETURN ::nID

METHOD message( cMessage ) CLASS MENUITEM

   IF cMessage != NIL
      ::cMessage := __eInstVar53( Self, "MESSAGE", cMessage, "C", 1001 )
   ENDIF

   RETURN ::cMessage

METHOD shortcut( nShortcut ) CLASS MENUITEM

   IF nShortcut != NIL
      ::nShortcut := __eInstVar53( Self, "SHORTCUT", nShortcut, "N", 1001 )
   ENDIF

   RETURN ::nShortcut

METHOD style( cStyle ) CLASS MENUITEM

   IF cStyle != NIL
      ::cStyle := __eInstVar53( Self, "STYLE", cStyle, "C", 1001, {|| Len( cStyle ) == 2 } )
   ENDIF

   RETURN ::cStyle

METHOD isPopUp() CLASS MENUITEM
   RETURN HB_ISOBJECT( ::data ) .AND. ::data:ClassName() $ "POPUPMENU|HB_POPUPMENU"

METHOD New( cCaption, boData, nShortcut, cMessage, nID ) CLASS MENUITEM

   IF ! HB_ISNUMERIC( nShortcut )
      nShortcut := 0
   ENDIF
   IF ! HB_ISSTRING( cMessage )
      cMessage := ""
   ENDIF
   IF ! HB_ISNUMERIC( nID )
      nID := 0
   ENDIF

   ::data      := boData
   ::nID       := nID
   ::cMessage  := cMessage
   ::nShortcut := nShortcut
   ::caption   := cCaption

   RETURN Self

FUNCTION MenuItem( cCaption, boData, nShortcut, cMessage, nID )
   RETURN HBMenuItem():New( cCaption, boData, nShortcut, cMessage, nID )

#ifdef HB_CLP_UNDOC

FUNCTION __miColumn( o, nColumn )

   IF HB_ISOBJECT( o ) .AND. o:ClassName() == "MENUITEM"

      IF HB_ISNUMERIC( nColumn )
         o:__col := nColumn
      ENDIF

      RETURN o:__col
   ENDIF

   RETURN -1

FUNCTION __miRow( o, nRow )

   IF HB_ISOBJECT( o ) .AND. o:ClassName() == "MENUITEM"

      IF HB_ISNUMERIC( nRow )
         o:__row := nRow
      ENDIF

      RETURN o:__row
   ENDIF

   RETURN -1

#endif

#endif
