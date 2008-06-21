/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour GUI framework for Gtk
 * Class HBForm
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * Copyright 2001 Alexander Kresin <alex@belacy.belgorod.su>
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

static aForms := {}

/* ********************************************************************* */

CLASS HBForm FROM HBWinControl

    DATA      oMainMenu

    DATA      aControls   INIT {}                            PROPERTY
    DATA      OnClick                                        PROPERTY

    CLASSDATA lRegistered INIT .T.

    ACCESS    Caption() INLINE hb_GtkWindowGetText( ::hWnd ) PROPERTY
    ASSIGN    Caption( cNewCaption ) INLINE ;
                  hb_GtkWindowSetText( ::hWnd, cNewCaption )

    METHOD    New()
    /* NOTE: currently ::Close() probably does not work as in Hgf Win */
    METHOD    Close() INLINE hb_GtkWindowRequestDelete( ::hWnd )
    METHOD        Command( nCmd, nID, aEventData )
    METHOD    CtrlCommand( nCmd, nID, aEventData )
    METHOD    LButtonDown( nCmd, nID, aEventData )
    METHOD    HandleEvent( nCmd, nID, aEventData )
    METHOD    ShowModal()

    METHOD    InsertControl( oControl )

    ACCESS    Menu() INLINE ::oMainMenu                      PROPERTY
    ASSIGN    Menu( oNewMenu )

ENDCLASS

/* ********************************************************************* */

METHOD New() CLASS HBForm
    /* HBForm is derived from HBWinControl so we should set its nID */
    ::hWnd := hb_GtkWindowCreate( ::GetNewID() )
    ::YSize := 400
    ::XSize := 500
    AAdd( aForms, Self )
RETURN Self

/* ********************************************************************* */

/* it is only for menu commands so it should be named MenuCommand() or so */
METHOD Command( nCmd, nID, aEventData ) CLASS HBForm
    LOCAL oMenuItem

    IF ::Menu != nil
        IF ( oMenuItem := ::Menu:FindItem( nId ) ) != nil
            IF oMenuItem:OnClick != nil
                __ObjSendMsg( Self, oMenuItem:OnClick, oMenuItem )
            ENDIF
        ENDIF
    ENDIF
RETURN nil

/* ********************************************************************* */

METHOD CtrlCommand( nCmd, nID, aEventData ) CLASS HBForm
    LOCAL nAt, oControl

    IF ( nAt := AScan( ::aControls, { | o | o:nID == nID } ) ) != 0
        oControl = ::aControls[ nAt ]
        IF oControl:OnClick != nil
            __ObjSendMsg( Self, oControl:OnClick, oControl )
        ENDIF
    ENDIF
/* NOTE: currently return value is not portable. Needs fixing */
RETURN 1

/* ********************************************************************* */

METHOD LButtonDown( nCmd, nID, aEventData ) CLASS HBForm
   IF ::OnClick != nil
       /* NOTE: aEventData[ 1 ] conatins mouse y position   */
       /*       aEventData[ 2 ] conatins mouse x position   */
       /*       aEventData[ 3 ] conatins keyboard modifiers */
       __ObjSendMsg( Self, ::OnClick, Self, aEventData )
   ENDIF
/* NOTE: currently return value is not portable. Needs fixing */
RETURN 1

/* ********************************************************************* */

/* NOTE: aEventData should be well defined because of portability reasons */
/* it could be an array with additional parameters - like mouse x and y   */
/* position, keyboard state, time of the event, and so on                 */
METHOD HandleEvent( nCmd, nID, aEventData ) CLASS HBForm
    DO CASE
    CASE nCmd == HGF_EV_MENU
        RETURN ::Command( nCmd, nID, aEventData )

    CASE nCmd == HGF_EV_CLICK
        RETURN ::CtrlCommand( nCmd, nID, aEventData )

    CASE nCmd == HGF_EV_LBUTTONPRESSED
        RETURN ::LButtonDown( nCmd, nID, aEventData )

    CASE nCmd == HGF_EV_RBUTTONPRESSED
        /* NOTE: currently return value is not portable. Needs fixing */
        RETURN 1

    CASE nCmd == HGF_EV_DESTROY
        /* NOTE: currently return value is not portable. Needs fixing */
        RETURN 0

    CASE nCmd == HGF_EV_CLOSE
        /* NOTE: currently return value is not portable. Needs fixing */
        RETURN 0

    ENDCASE
RETURN nil

/* ********************************************************************* */

METHOD ShowModal() CLASS HBForm
    hb_GtkShowModal( ::hWnd )
RETURN nil

/* ********************************************************************* */

METHOD InsertControl( oControl ) CLASS HBForm
   AAdd( ::aControls, oControl )
   oControl:Show()
RETURN nil

/* ********************************************************************* */

ASSIGN Menu( oNewMenu ) CLASS HBForm
    ::oMainMenu := oNewMenu
RETURN nil

/* ********************************************************************* */

// messages entry point - I think it can be different for different GUIs
FUNCTION HB_GUI( hWnd, Widget, nCmd, nID, aEventData )
   LOCAL aReturn := 0
   LOCAL nForm := AScan( aForms, { | oForm | oForm:hWnd[ 1 ] == hWnd } )

   IF nForm != 0
      aReturn := aForms[ nForm ]:HandleEvent( nCmd, nID, aEventData )
   ENDIF
RETURN aReturn

/* ********************************************************************* */
