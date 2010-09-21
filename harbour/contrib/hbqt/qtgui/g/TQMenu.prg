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


FUNCTION QMenu( ... )
   RETURN HB_QMenu():new( ... )


CREATE CLASS QMenu INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QMenu

   METHOD  new( ... )

   METHOD  actionAt( pPt )
   METHOD  actionGeometry( pAct )
   METHOD  activeAction()
   METHOD  addAction( ... )
   METHOD  addMenu( ... )
   METHOD  addSeparator()
   METHOD  clear()
   METHOD  defaultAction()
   METHOD  exec( ... )
   METHOD  hideTearOffMenu()
   METHOD  icon()
   METHOD  insertMenu( pBefore, pMenu )
   METHOD  insertSeparator( pBefore )
   METHOD  isEmpty()
   METHOD  isTearOffEnabled()
   METHOD  isTearOffMenuVisible()
   METHOD  menuAction()
   METHOD  popup( pP, pAtAction )
   METHOD  separatorsCollapsible()
   METHOD  setActiveAction( pAct )
   METHOD  setDefaultAction( pAct )
   METHOD  setIcon( pIcon )
   METHOD  setSeparatorsCollapsible( lCollapse )
   METHOD  setTearOffEnabled( lBool )
   METHOD  setTitle( cTitle )
   METHOD  title()

   ENDCLASS


METHOD QMenu:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMenu( ... )
   RETURN Self


METHOD QMenu:actionAt( pPt )
   RETURN Qt_QMenu_actionAt( ::pPtr, hbqt_ptr( pPt ) )


METHOD QMenu:actionGeometry( pAct )
   RETURN Qt_QMenu_actionGeometry( ::pPtr, hbqt_ptr( pAct ) )


METHOD QMenu:activeAction()
   RETURN Qt_QMenu_activeAction( ::pPtr )


METHOD QMenu:addAction( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QMenu_addAction( ::pPtr, ... )


METHOD QMenu:addMenu( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QMenu_addMenu( ::pPtr, ... )


METHOD QMenu:addSeparator()
   RETURN Qt_QMenu_addSeparator( ::pPtr )


METHOD QMenu:clear()
   RETURN Qt_QMenu_clear( ::pPtr )


METHOD QMenu:defaultAction()
   RETURN Qt_QMenu_defaultAction( ::pPtr )


METHOD QMenu:exec( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   RETURN Qt_QMenu_exec( ::pPtr, ... )


METHOD QMenu:hideTearOffMenu()
   RETURN Qt_QMenu_hideTearOffMenu( ::pPtr )


METHOD QMenu:icon()
   RETURN Qt_QMenu_icon( ::pPtr )


METHOD QMenu:insertMenu( pBefore, pMenu )
   RETURN Qt_QMenu_insertMenu( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pMenu ) )


METHOD QMenu:insertSeparator( pBefore )
   RETURN Qt_QMenu_insertSeparator( ::pPtr, hbqt_ptr( pBefore ) )


METHOD QMenu:isEmpty()
   RETURN Qt_QMenu_isEmpty( ::pPtr )


METHOD QMenu:isTearOffEnabled()
   RETURN Qt_QMenu_isTearOffEnabled( ::pPtr )


METHOD QMenu:isTearOffMenuVisible()
   RETURN Qt_QMenu_isTearOffMenuVisible( ::pPtr )


METHOD QMenu:menuAction()
   RETURN Qt_QMenu_menuAction( ::pPtr )


METHOD QMenu:popup( pP, pAtAction )
   RETURN Qt_QMenu_popup( ::pPtr, hbqt_ptr( pP ), hbqt_ptr( pAtAction ) )


METHOD QMenu:separatorsCollapsible()
   RETURN Qt_QMenu_separatorsCollapsible( ::pPtr )


METHOD QMenu:setActiveAction( pAct )
   RETURN Qt_QMenu_setActiveAction( ::pPtr, hbqt_ptr( pAct ) )


METHOD QMenu:setDefaultAction( pAct )
   RETURN Qt_QMenu_setDefaultAction( ::pPtr, hbqt_ptr( pAct ) )


METHOD QMenu:setIcon( pIcon )
   RETURN Qt_QMenu_setIcon( ::pPtr, hbqt_ptr( pIcon ) )


METHOD QMenu:setSeparatorsCollapsible( lCollapse )
   RETURN Qt_QMenu_setSeparatorsCollapsible( ::pPtr, lCollapse )


METHOD QMenu:setTearOffEnabled( lBool )
   RETURN Qt_QMenu_setTearOffEnabled( ::pPtr, lBool )


METHOD QMenu:setTitle( cTitle )
   RETURN Qt_QMenu_setTitle( ::pPtr, cTitle )


METHOD QMenu:title()
   RETURN Qt_QMenu_title( ::pPtr )

