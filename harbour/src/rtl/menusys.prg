/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MENUSYS parts
 *
 * Copyright 2002 Larry Sevilla <lsevilla@nddc.edu.ph>
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

#include "getexit.ch"
#include "inkey.ch"
#include "setcurs.ch"

/* NOTE: Harbour doesn't support CA-Cl*pper 5.3 GUI functionality, but
         it has all related variables and methods. */

#ifdef HB_COMPAT_C53

/* Standard Menu System Modal handling for Menu Items */
FUNCTION MenuModal( oTopMenu, nSelection, nMsgRow, nMsgLeft, nMsgRight, cMsgColor, GetList )
   RETURN HBMenuSys():New( oTopMenu ):modal( nSelection, nMsgRow, nMsgLeft, nMsgRight, cMsgColor, GetList )

/* Dummy function */
FUNCTION ShowMsg( aMsg, lMode )

   HB_SYMBOL_UNUSED( aMsg )
   HB_SYMBOL_UNUSED( lMode )

   RETURN .F.

/***
*
*  ShortCut processing for initial Get or Menu Item.
*
***/
FUNCTION IsShortcut( oMenu, nKey, nID )

   LOCAL nItem
   LOCAL nTotal
   LOCAL nShortCut
   LOCAL oItem
   LOCAL i

   // Test for top menu item not a TopBar Menu:
   IF !( oMenu:ClassName() == "TOPBARMENU" )

      RETURN IsQuick( oMenu, nKey, @nID )

   // Test and assign top menu item shortCut, enabled, and ! PopUp:
   // Changed by enclosing assignment before ':Enabled':
   ELSEIF ( nShortCut := oMenu:getShortCt( nKey ) ) > 0 .AND. ;
          ( oItem := oMenu:getItem( nShortcut ) ):enabled .AND. ;
          ! oItem:isPopUp()

      oMenu:select( nShortCut )
      Eval( oItem:data, oItem )
      nID := oItem:ID

      RETURN .T.

   // Test and assignment for TopBar MenuItem:
   ELSEIF nShortCut == 0

      nTotal := oMenu:itemCount
      nItem  := oMenu:current

      IF nItem == 0
         nItem := 1
      ENDIF

      // Loop to wrap around through TopMenu from Current Item:
      FOR i := 1 TO nTotal

         IF ( oItem := oMenu:getItem( nItem ) ):enabled .AND. ;
            oItem:isPopUp() .AND. ;
            IsQuick( oItem:data, nKey, @nID )

            RETURN .T.
         ENDIF

         IF ++nItem > nTotal
            nItem := 1
         ENDIF
      NEXT

   ENDIF

   RETURN .F.

/***
*
*  Navigates to the next Get or Menu Item from the
*  Current if more than one uses the same ShortCut.
*
***/
FUNCTION IsQuick( oMenu, nKey, nID )

   LOCAL nItem
   LOCAL nTotal
   LOCAL nShortCut
   LOCAL oItem

   IF ( nShortCut := oMenu:getShortCt( nKey ) ) == 0

      nTotal := oMenu:itemCount

      FOR nItem := 1 TO nTotal

         IF ( oItem := oMenu:getItem( nItem ) ):enabled .AND. ;
            oItem:isPopUp() .AND. ;
            IsQuick( oItem:data, nKey, @nID )

            RETURN .T.
         ENDIF
      NEXT

   ELSEIF !( oItem := oMenu:getItem( nShortCut ) ):isPopUp() .AND. oItem:enabled

      oMenu:select( nShortCut )
      Eval( oItem:data, oItem )
      nID := oItem:ID

      RETURN .T.

   ENDIF

   RETURN .F.

#endif
