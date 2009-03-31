/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include 'hbclass.ch'


CLASS QAction INHERIT QObject

   DATA    pPtr

   METHOD  New()

   METHOD  actionGroup()                       INLINE  Qt_QAction_actionGroup( ::pPtr )
   METHOD  activate( nActionEvent )            INLINE  Qt_QAction_activate( ::pPtr, nActionEvent )
   METHOD  autoRepeat()                        INLINE  Qt_QAction_autoRepeat( ::pPtr )
   METHOD  iconText()                          INLINE  Qt_QAction_iconText( ::pPtr )
   METHOD  isCheckable()                       INLINE  Qt_QAction_isCheckable( ::pPtr )
   METHOD  isChecked()                         INLINE  Qt_QAction_isChecked( ::pPtr )
   METHOD  isEnabled()                         INLINE  Qt_QAction_isEnabled( ::pPtr )
   METHOD  isIconVisibleInMenu()               INLINE  Qt_QAction_isIconVisibleInMenu( ::pPtr )
   METHOD  isSeparator()                       INLINE  Qt_QAction_isSeparator( ::pPtr )
   METHOD  isVisible()                         INLINE  Qt_QAction_isVisible( ::pPtr )
   METHOD  menu()                              INLINE  Qt_QAction_menu( ::pPtr )
   METHOD  menuRole()                          INLINE  Qt_QAction_menuRole( ::pPtr )
   METHOD  parentWidget()                      INLINE  Qt_QAction_parentWidget( ::pPtr )
   METHOD  setActionGroup( pGroup )            INLINE  Qt_QAction_setActionGroup( ::pPtr, pGroup )
   METHOD  setAutoRepeat( lBool )              INLINE  Qt_QAction_setAutoRepeat( ::pPtr, lBool )
   METHOD  setCheckable( lBool )               INLINE  Qt_QAction_setCheckable( ::pPtr, lBool )
   METHOD  setIcon( cIcon )                    INLINE  Qt_QAction_setIcon( ::pPtr, cIcon )
   METHOD  setIconText( cText )                INLINE  Qt_QAction_setIconText( ::pPtr, cText )
   METHOD  setIconVisibleInMenu( lVisible )    INLINE  Qt_QAction_setIconVisibleInMenu( ::pPtr, lVisible )
   METHOD  setMenu( pMenu )                    INLINE  Qt_QAction_setMenu( ::pPtr, pMenu )
   METHOD  setMenuRole( nMenuRole )            INLINE  Qt_QAction_setMenuRole( ::pPtr, nMenuRole )
   METHOD  setSeparator( lB )                  INLINE  Qt_QAction_setSeparator( ::pPtr, lB )
   METHOD  setShortcutContext( nContext )      INLINE  Qt_QAction_setShortcutContext( ::pPtr, nContext )
   METHOD  setShortcuts( nKey )                INLINE  Qt_QAction_setShortcuts( ::pPtr, nKey )
   METHOD  setStatusTip( cStatusTip )          INLINE  Qt_QAction_setStatusTip( ::pPtr, cStatusTip )
   METHOD  setText( cText )                    INLINE  Qt_QAction_setText( ::pPtr, cText )
   METHOD  setToolTip( cTip )                  INLINE  Qt_QAction_setToolTip( ::pPtr, cTip )
   METHOD  setWhatsThis( cWhat )               INLINE  Qt_QAction_setWhatsThis( ::pPtr, cWhat )
   METHOD  shortcutContext()                   INLINE  Qt_QAction_shortcutContext( ::pPtr )
   METHOD  showStatusText( pWidget )           INLINE  Qt_QAction_showStatusText( ::pPtr, pWidget )
   METHOD  statusTip()                         INLINE  Qt_QAction_statusTip( ::pPtr )
   METHOD  text()                              INLINE  Qt_QAction_text( ::pPtr )
   METHOD  toolTip()                           INLINE  Qt_QAction_toolTip( ::pPtr )
   METHOD  whatsThis()                         INLINE  Qt_QAction_whatsThis( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QAction

   ::pPtr := Qt_QAction( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

