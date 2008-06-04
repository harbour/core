/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour GUI framework for Gtk
 * Class HBMenu
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * Copyright 2001 Maurilio Longo <maurilio.longo@libero.it>
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
 * Additional Copyright notes :
 *  adapted for Hgf Gtk by Marek Paliwoda <paliwoda@inetia.pl>
 */

/* ********************************************************************* */

#include "hbclass.ch"
#include "harbgtk.ch"

/* ********************************************************************* */

CLASS HBMenu FROM HBPersistent

    DATA   nHandle
    DATA   Items  PROPERTY INIT {}

    DATA   Container

    METHOD New( oForm )
    METHOD Add( oMenuItem )
    METHOD FindItem( nId )   // Searches for a sub menuitem given its id

HIDDEN:
    DATA   nType

ENDCLASS

/* ********************************************************************* */

METHOD New( oForm ) CLASS HBMenu
    IF oForm != nil
        ::Container := oForm
        ::nHandle := hb_GtkCreateMenuBar( oForm:hWnd[ 2 ] )
        oForm:hWnd[ 6 ] := ::nHandle
        ::nType := HBGTKMENUTYPEBAR
    ELSE
        ::nHandle := hb_GtkCreateMenu()
        ::nType := HBGTKMENUTYPEPOP
    ENDIF
RETURN Self

/* ********************************************************************* */

METHOD Add( oMenuItem ) CLASS HBMenu
    /* required because of a stupid Harbour compiler error */
    LOCAL nHandle := ::nHandle

    IF oMenuItem != nil
        IF ::nType == HBGTKMENUTYPEBAR
            oMenuItem:hMenuItem := hb_GtkBarAddMenuItem( ::nHandle, ;
                oMenuItem:Caption, oMenuItem:nId, oMenuItem:Enabled, ;
                ::Container:hWnd[ 1 ] )
        ELSE
            oMenuItem:hMenuItem := hb_GtkAddMenuItem( @nHandle, ::nHandle, ;
                oMenuItem:hMenuItem, oMenuItem:Caption, oMenuItem:nId, ;
                oMenuItem:Enabled, ::Container:hWnd[ 1 ] )

            //::nHandle := nHandle
        ENDIF

        oMenuItem:oParent := Self
        AAdd( ::Items, oMenuItem )
    ENDIF
RETURN nil

/* ********************************************************************* */

METHOD FindItem( nId ) CLASS HBMenu
    LOCAL oMenuItem, n

    FOR n := 1 TO Len( ::Items )
       IF ( oMenuItem := ::Items[ n ] ):nId == nId
           RETURN oMenuItem
       ELSE
           IF oMenuItem:Items != nil
               IF ( oMenuItem := oMenuItem:FindItem( nId ) ) != nil
                   RETURN oMenuItem
               ENDIF
           ENDIF
       ENDIF
    NEXT
RETURN oMenuItem

/* ********************************************************************* */
