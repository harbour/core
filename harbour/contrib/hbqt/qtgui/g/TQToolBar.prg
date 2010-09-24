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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QToolBar( ... )
   RETURN HB_QToolBar():new( ... )


CREATE CLASS QToolBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QToolBar

   METHOD  new( ... )

   METHOD  actionAt( ... )
   METHOD  addAction( ... )
   METHOD  addSeparator()
   METHOD  addWidget( pWidget )
   METHOD  allowedAreas()
   METHOD  clear()
   METHOD  iconSize()
   METHOD  insertSeparator( pBefore )
   METHOD  insertWidget( pBefore, pWidget )
   METHOD  isAreaAllowed( nArea )
   METHOD  isFloatable()
   METHOD  isFloating()
   METHOD  isMovable()
   METHOD  orientation()
   METHOD  setAllowedAreas( nAreas )
   METHOD  setFloatable( lFloatable )
   METHOD  setMovable( lMovable )
   METHOD  setOrientation( nOrientation )
   METHOD  toggleViewAction()
   METHOD  toolButtonStyle()
   METHOD  widgetForAction( pAction )
   METHOD  setIconSize( pIconSize )
   METHOD  setToolButtonStyle( nToolButtonStyle )

   ENDCLASS


METHOD QToolBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QToolBar( ... )
   RETURN Self


METHOD QToolBar:actionAt( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // QAction * actionAt ( int x, int y ) const
                // N n int, N n int
         RETURN QAction():from( Qt_QToolBar_actionAt_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QAction * actionAt ( const QPoint & p ) const
                // PO p QPoint
         RETURN QAction():from( Qt_QToolBar_actionAt( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QToolBar:addAction( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "PCO" .AND. aV[ 2 ] $ "C" .AND. aV[ 3 ] $ "PO" .AND. aV[ 4 ] $ "PO"
                // QAction * addAction ( const QIcon & icon, const QString & text, const QObject * receiver, const char * member )   [*D=3*]
                // PCO p QIcon, C c QString, PO p QObject, PO p char
         RETURN QAction():from( Qt_QToolBar_addAction_4( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "PO"
                // QAction * addAction ( const QString & text, const QObject * receiver, const char * member )
                // C c QString, PO p QObject, PO p char
         RETURN QAction():from( Qt_QToolBar_addAction_3( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PCO" .AND. aV[ 2 ] $ "C"
                // QAction * addAction ( const QIcon & icon, const QString & text )
                // PCO p QIcon, C c QString
         RETURN QAction():from( Qt_QToolBar_addAction_2( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // QAction * addAction ( const QString & text )
                // C c QString
         RETURN QAction():from( Qt_QToolBar_addAction_1( ::pPtr, ... ) )
      CASE aV[ 1 ] $ "PO"
                // void addAction ( QAction * action )   [*D=1*]
                // PO p QAction
         RETURN Qt_QToolBar_addAction( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QToolBar:addSeparator()
   RETURN Qt_QToolBar_addSeparator( ::pPtr )


METHOD QToolBar:addWidget( pWidget )
   RETURN Qt_QToolBar_addWidget( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QToolBar:allowedAreas()
   RETURN Qt_QToolBar_allowedAreas( ::pPtr )


METHOD QToolBar:clear()
   RETURN Qt_QToolBar_clear( ::pPtr )


METHOD QToolBar:iconSize()
   RETURN Qt_QToolBar_iconSize( ::pPtr )


METHOD QToolBar:insertSeparator( pBefore )
   RETURN Qt_QToolBar_insertSeparator( ::pPtr, hbqt_ptr( pBefore ) )


METHOD QToolBar:insertWidget( pBefore, pWidget )
   RETURN Qt_QToolBar_insertWidget( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pWidget ) )


METHOD QToolBar:isAreaAllowed( nArea )
   RETURN Qt_QToolBar_isAreaAllowed( ::pPtr, nArea )


METHOD QToolBar:isFloatable()
   RETURN Qt_QToolBar_isFloatable( ::pPtr )


METHOD QToolBar:isFloating()
   RETURN Qt_QToolBar_isFloating( ::pPtr )


METHOD QToolBar:isMovable()
   RETURN Qt_QToolBar_isMovable( ::pPtr )


METHOD QToolBar:orientation()
   RETURN Qt_QToolBar_orientation( ::pPtr )


METHOD QToolBar:setAllowedAreas( nAreas )
   RETURN Qt_QToolBar_setAllowedAreas( ::pPtr, nAreas )


METHOD QToolBar:setFloatable( lFloatable )
   RETURN Qt_QToolBar_setFloatable( ::pPtr, lFloatable )


METHOD QToolBar:setMovable( lMovable )
   RETURN Qt_QToolBar_setMovable( ::pPtr, lMovable )


METHOD QToolBar:setOrientation( nOrientation )
   RETURN Qt_QToolBar_setOrientation( ::pPtr, nOrientation )


METHOD QToolBar:toggleViewAction()
   RETURN Qt_QToolBar_toggleViewAction( ::pPtr )


METHOD QToolBar:toolButtonStyle()
   RETURN Qt_QToolBar_toolButtonStyle( ::pPtr )


METHOD QToolBar:widgetForAction( pAction )
   RETURN Qt_QToolBar_widgetForAction( ::pPtr, hbqt_ptr( pAction ) )


METHOD QToolBar:setIconSize( pIconSize )
   RETURN Qt_QToolBar_setIconSize( ::pPtr, hbqt_ptr( pIconSize ) )


METHOD QToolBar:setToolButtonStyle( nToolButtonStyle )
   RETURN Qt_QToolBar_setToolButtonStyle( ::pPtr, nToolButtonStyle )

