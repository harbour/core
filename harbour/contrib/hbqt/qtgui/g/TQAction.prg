/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QAction( ... )
   RETURN HB_QAction():new( ... )


CREATE CLASS QAction INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QAction

   METHOD  new( ... )

   METHOD  actionGroup()
   METHOD  activate( nEvent )
   METHOD  associatedWidgets()
   METHOD  autoRepeat()
   METHOD  data()
   METHOD  font()
   METHOD  icon()
   METHOD  iconText()
   METHOD  isCheckable()
   METHOD  isChecked()
   METHOD  isEnabled()
   METHOD  isIconVisibleInMenu()
   METHOD  isSeparator()
   METHOD  isVisible()
   METHOD  menu()
   METHOD  menuRole()
   METHOD  parentWidget()
   METHOD  setActionGroup( pGroup )
   METHOD  setAutoRepeat( lBool )
   METHOD  setCheckable( lBool )
   METHOD  setData( pUserData )
   METHOD  setFont( pFont )
   METHOD  setIcon( pIcon )
   METHOD  setIconText( cText )
   METHOD  setIconVisibleInMenu( lVisible )
   METHOD  setMenu( pMenu )
   METHOD  setMenuRole( nMenuRole )
   METHOD  setSeparator( lB )
   METHOD  setShortcut( pShortcut )
   METHOD  setShortcutContext( nContext )
   METHOD  setShortcuts( nKey )
   METHOD  setStatusTip( cStatusTip )
   METHOD  setText( cText )
   METHOD  setToolTip( cTip )
   METHOD  setWhatsThis( cWhat )
   METHOD  shortcut()
   METHOD  shortcutContext()
   METHOD  shortcuts()
   METHOD  showStatusText( pWidget )
   METHOD  statusTip()
   METHOD  text()
   METHOD  toolTip()
   METHOD  whatsThis()
   METHOD  hover()
   METHOD  setChecked( lBool )
   METHOD  setDisabled( lB )
   METHOD  setEnabled( lBool )
   METHOD  setVisible( lBool )
   METHOD  toggle()
   METHOD  trigger()

   ENDCLASS


METHOD QAction:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAction( ... )
   RETURN Self


METHOD QAction:actionGroup()
   RETURN HB_QActionGroup():from( Qt_QAction_actionGroup( ::pPtr ) )


METHOD QAction:activate( nEvent )
   RETURN Qt_QAction_activate( ::pPtr, nEvent )


METHOD QAction:associatedWidgets()
   RETURN HB_QList():from( Qt_QAction_associatedWidgets( ::pPtr ) )


METHOD QAction:autoRepeat()
   RETURN Qt_QAction_autoRepeat( ::pPtr )


METHOD QAction:data()
   RETURN HB_QVariant():from( Qt_QAction_data( ::pPtr ) )


METHOD QAction:font()
   RETURN HB_QFont():from( Qt_QAction_font( ::pPtr ) )


METHOD QAction:icon()
   RETURN HB_QIcon():from( Qt_QAction_icon( ::pPtr ) )


METHOD QAction:iconText()
   RETURN Qt_QAction_iconText( ::pPtr )


METHOD QAction:isCheckable()
   RETURN Qt_QAction_isCheckable( ::pPtr )


METHOD QAction:isChecked()
   RETURN Qt_QAction_isChecked( ::pPtr )


METHOD QAction:isEnabled()
   RETURN Qt_QAction_isEnabled( ::pPtr )


METHOD QAction:isIconVisibleInMenu()
   RETURN Qt_QAction_isIconVisibleInMenu( ::pPtr )


METHOD QAction:isSeparator()
   RETURN Qt_QAction_isSeparator( ::pPtr )


METHOD QAction:isVisible()
   RETURN Qt_QAction_isVisible( ::pPtr )


METHOD QAction:menu()
   RETURN HB_QMenu():from( Qt_QAction_menu( ::pPtr ) )


METHOD QAction:menuRole()
   RETURN Qt_QAction_menuRole( ::pPtr )


METHOD QAction:parentWidget()
   RETURN HB_QWidget():from( Qt_QAction_parentWidget( ::pPtr ) )


METHOD QAction:setActionGroup( pGroup )
   RETURN Qt_QAction_setActionGroup( ::pPtr, hbqt_ptr( pGroup ) )


METHOD QAction:setAutoRepeat( lBool )
   RETURN Qt_QAction_setAutoRepeat( ::pPtr, lBool )


METHOD QAction:setCheckable( lBool )
   RETURN Qt_QAction_setCheckable( ::pPtr, lBool )


METHOD QAction:setData( pUserData )
   RETURN Qt_QAction_setData( ::pPtr, hbqt_ptr( pUserData ) )


METHOD QAction:setFont( pFont )
   RETURN Qt_QAction_setFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QAction:setIcon( pIcon )
   RETURN Qt_QAction_setIcon( ::pPtr, hbqt_ptr( pIcon ) )


METHOD QAction:setIconText( cText )
   RETURN Qt_QAction_setIconText( ::pPtr, cText )


METHOD QAction:setIconVisibleInMenu( lVisible )
   RETURN Qt_QAction_setIconVisibleInMenu( ::pPtr, lVisible )


METHOD QAction:setMenu( pMenu )
   RETURN Qt_QAction_setMenu( ::pPtr, hbqt_ptr( pMenu ) )


METHOD QAction:setMenuRole( nMenuRole )
   RETURN Qt_QAction_setMenuRole( ::pPtr, nMenuRole )


METHOD QAction:setSeparator( lB )
   RETURN Qt_QAction_setSeparator( ::pPtr, lB )


METHOD QAction:setShortcut( pShortcut )
   RETURN Qt_QAction_setShortcut( ::pPtr, hbqt_ptr( pShortcut ) )


METHOD QAction:setShortcutContext( nContext )
   RETURN Qt_QAction_setShortcutContext( ::pPtr, nContext )


METHOD QAction:setShortcuts( nKey )
   RETURN Qt_QAction_setShortcuts( ::pPtr, nKey )


METHOD QAction:setStatusTip( cStatusTip )
   RETURN Qt_QAction_setStatusTip( ::pPtr, cStatusTip )


METHOD QAction:setText( cText )
   RETURN Qt_QAction_setText( ::pPtr, cText )


METHOD QAction:setToolTip( cTip )
   RETURN Qt_QAction_setToolTip( ::pPtr, cTip )


METHOD QAction:setWhatsThis( cWhat )
   RETURN Qt_QAction_setWhatsThis( ::pPtr, cWhat )


METHOD QAction:shortcut()
   RETURN HB_QKeySequence():from( Qt_QAction_shortcut( ::pPtr ) )


METHOD QAction:shortcutContext()
   RETURN Qt_QAction_shortcutContext( ::pPtr )


METHOD QAction:shortcuts()
   RETURN HB_QList():from( Qt_QAction_shortcuts( ::pPtr ) )


METHOD QAction:showStatusText( pWidget )
   RETURN Qt_QAction_showStatusText( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QAction:statusTip()
   RETURN Qt_QAction_statusTip( ::pPtr )


METHOD QAction:text()
   RETURN Qt_QAction_text( ::pPtr )


METHOD QAction:toolTip()
   RETURN Qt_QAction_toolTip( ::pPtr )


METHOD QAction:whatsThis()
   RETURN Qt_QAction_whatsThis( ::pPtr )


METHOD QAction:hover()
   RETURN Qt_QAction_hover( ::pPtr )


METHOD QAction:setChecked( lBool )
   RETURN Qt_QAction_setChecked( ::pPtr, lBool )


METHOD QAction:setDisabled( lB )
   RETURN Qt_QAction_setDisabled( ::pPtr, lB )


METHOD QAction:setEnabled( lBool )
   RETURN Qt_QAction_setEnabled( ::pPtr, lBool )


METHOD QAction:setVisible( lBool )
   RETURN Qt_QAction_setVisible( ::pPtr, lBool )


METHOD QAction:toggle()
   RETURN Qt_QAction_toggle( ::pPtr )


METHOD QAction:trigger()
   RETURN Qt_QAction_trigger( ::pPtr )

