/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour GUI framework for Gtk
 * Class HBMenuItem
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
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

#include "common.ch"
#include "hbclass.ch"
#include "harbgtk.ch"

/* ********************************************************************* */

CLASS HBMenuItem FROM HBPersistent

    DATA   Caption   INIT ""  PROPERTY // Specifies the text of the menu item
    DATA   Name               PROPERTY // The name of this component
    DATA   OnClick            PROPERTY // A character description of the method to invoke
    DATA   Enabled   INIT .T. PROPERTY // Specifies whether the menu item is enabled
    DATA   Items     INIT {}  PROPERTY // Contains the menu items in the submenu of the menu item

    DATA   nId                   // Command value to send to the container form
    DATA   oParent               // Identifies the parent menu item of this menu item
    DATA   Container

    DATA   nHandle   INIT 0      // The handle of the submenu of this menu item
    DATA   hMenuItem INIT 0      // The handle of this menu item

    CLASSDATA nIdStart INIT 110  // start value for commands value to assign to menu items

    METHOD New( oOwner )         // Creates a new menu item
    METHOD Add( oMenuItem )      // Adds a new drop down menu item
    METHOD FindItem( nId )       // Searches for a sub menuitem given its id

ENDCLASS

/* ********************************************************************* */

METHOD New( oOwner ) CLASS HBMenuItem

    ::nId := ::nIdStart++
    IF oOwner != nil
        ::Container := oOwner:Container
    ENDIF
    ::oParent := oOwner

    // NOTE : physical creation is delayed until MenuItem is added to a parent
RETURN Self

/* ********************************************************************* */

METHOD Add( oMenuItem ) CLASS HBMenuItem
    /* required because of a stupid Harbour compiler error */
    LOCAL nHandle := ::nHandle

    oMenuItem:hMenuItem := hb_GtkAddMenuItem( @nHandle, ::hMenuItem, ;
         oMenuItem:hMenuItem, oMenuItem:Caption, oMenuItem:nId,     ;
         oMenuItem:Enabled, ::Container:hWnd[ 1 ] )

    ::nHandle := nHandle

    oMenuItem:oParent = Self
    AAdd( ::Items, oMenuItem )
RETURN nil

/* ********************************************************************* */

METHOD FindItem( nId ) CLASS HBMenuItem
   LOCAL oMenuItem, n

   FOR n = 1 TO Len( ::Items )
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
