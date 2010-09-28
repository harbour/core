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
   RETURN HB_QAction():from( Qt_QMenu_actionAt( ::pPtr, hbqt_ptr( pPt ) ) )


METHOD QMenu:actionGeometry( pAct )
   RETURN HB_QRect():from( Qt_QMenu_actionGeometry( ::pPtr, hbqt_ptr( pAct ) ) )


METHOD QMenu:activeAction()
   RETURN HB_QAction():from( Qt_QMenu_activeAction( ::pPtr ) )


METHOD QMenu:addAction( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) )
         RETURN HB_QAction():from( Qt_QMenu_addAction_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) )
         RETURN HB_QAction():from( Qt_QMenu_addAction_2( ::pPtr, ... ) )
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) )
         RETURN HB_QAction():from( Qt_QMenu_addAction_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN HB_QAction():from( Qt_QMenu_addAction_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN HB_QAction():from( Qt_QMenu_addAction_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QAction():from( Qt_QMenu_addAction( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMenu_addAction_4( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMenu:addMenu( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN HB_QMenu():from( Qt_QMenu_addMenu_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QMenu():from( Qt_QMenu_addMenu_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QAction():from( Qt_QMenu_addMenu( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMenu:addSeparator()
   RETURN HB_QAction():from( Qt_QMenu_addSeparator( ::pPtr ) )


METHOD QMenu:clear()
   RETURN Qt_QMenu_clear( ::pPtr )


METHOD QMenu:defaultAction()
   RETURN HB_QAction():from( Qt_QMenu_defaultAction( ::pPtr ) )


METHOD QMenu:exec( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN HB_QAction():from( Qt_QMenu_exec_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QAction():from( Qt_QMenu_exec_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QAction():from( Qt_QMenu_exec( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMenu:hideTearOffMenu()
   RETURN Qt_QMenu_hideTearOffMenu( ::pPtr )


METHOD QMenu:icon()
   RETURN HB_QIcon():from( Qt_QMenu_icon( ::pPtr ) )


METHOD QMenu:insertMenu( pBefore, pMenu )
   RETURN HB_QAction():from( Qt_QMenu_insertMenu( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pMenu ) ) )


METHOD QMenu:insertSeparator( pBefore )
   RETURN HB_QAction():from( Qt_QMenu_insertSeparator( ::pPtr, hbqt_ptr( pBefore ) ) )


METHOD QMenu:isEmpty()
   RETURN Qt_QMenu_isEmpty( ::pPtr )


METHOD QMenu:isTearOffEnabled()
   RETURN Qt_QMenu_isTearOffEnabled( ::pPtr )


METHOD QMenu:isTearOffMenuVisible()
   RETURN Qt_QMenu_isTearOffMenuVisible( ::pPtr )


METHOD QMenu:menuAction()
   RETURN HB_QAction():from( Qt_QMenu_menuAction( ::pPtr ) )


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

