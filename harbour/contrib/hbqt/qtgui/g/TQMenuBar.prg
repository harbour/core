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


FUNCTION QMenuBar( ... )
   RETURN HB_QMenuBar():new( ... )


CREATE CLASS QMenuBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QMenuBar

   METHOD  new( ... )

   METHOD  activeAction()
   METHOD  addAction( cText )
   METHOD  addAction_1( cText, pReceiver, pMember )
   METHOD  addAction_2( pAction )
   METHOD  addMenu( pMenu )
   METHOD  addMenu_1( cTitle )
   METHOD  addMenu_2( pIcon, cTitle )
   METHOD  addSeparator()
   METHOD  clear()
   METHOD  insertMenu( pBefore, pMenu )
   METHOD  insertSeparator( pBefore )
   METHOD  isDefaultUp()
   METHOD  setActiveAction( pAct )
   METHOD  setDefaultUp( lBool )

   ENDCLASS


METHOD QMenuBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMenuBar( ... )
   RETURN Self


METHOD QMenuBar:activeAction()
   RETURN Qt_QMenuBar_activeAction( ::pPtr )


METHOD QMenuBar:addAction( cText )
   RETURN Qt_QMenuBar_addAction( ::pPtr, cText )


METHOD QMenuBar:addAction_1( cText, pReceiver, pMember )
   RETURN Qt_QMenuBar_addAction_1( ::pPtr, cText, hbqt_ptr( pReceiver ), hbqt_ptr( pMember ) )


METHOD QMenuBar:addAction_2( pAction )
   RETURN Qt_QMenuBar_addAction_2( ::pPtr, hbqt_ptr( pAction ) )


METHOD QMenuBar:addMenu( pMenu )
   RETURN Qt_QMenuBar_addMenu( ::pPtr, hbqt_ptr( pMenu ) )


METHOD QMenuBar:addMenu_1( cTitle )
   RETURN Qt_QMenuBar_addMenu_1( ::pPtr, cTitle )


METHOD QMenuBar:addMenu_2( pIcon, cTitle )
   RETURN Qt_QMenuBar_addMenu_2( ::pPtr, hbqt_ptr( pIcon ), cTitle )


METHOD QMenuBar:addSeparator()
   RETURN Qt_QMenuBar_addSeparator( ::pPtr )


METHOD QMenuBar:clear()
   RETURN Qt_QMenuBar_clear( ::pPtr )


METHOD QMenuBar:insertMenu( pBefore, pMenu )
   RETURN Qt_QMenuBar_insertMenu( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pMenu ) )


METHOD QMenuBar:insertSeparator( pBefore )
   RETURN Qt_QMenuBar_insertSeparator( ::pPtr, hbqt_ptr( pBefore ) )


METHOD QMenuBar:isDefaultUp()
   RETURN Qt_QMenuBar_isDefaultUp( ::pPtr )


METHOD QMenuBar:setActiveAction( pAct )
   RETURN Qt_QMenuBar_setActiveAction( ::pPtr, hbqt_ptr( pAct ) )


METHOD QMenuBar:setDefaultUp( lBool )
   RETURN Qt_QMenuBar_setDefaultUp( ::pPtr, lBool )

