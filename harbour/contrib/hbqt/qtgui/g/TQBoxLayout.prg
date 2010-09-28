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


FUNCTION QBoxLayout( ... )
   RETURN HB_QBoxLayout():new( ... )


CREATE CLASS QBoxLayout INHERIT HbQtObjectHandler, HB_QLayout FUNCTION HB_QBoxLayout

   METHOD  new( ... )

   METHOD  addLayout( pLayout, nStretch )
   METHOD  addSpacerItem( pSpacerItem )
   METHOD  addSpacing( nSize )
   METHOD  addStretch( nStretch )
   METHOD  addStrut( nSize )
   METHOD  addWidget( pWidget, nStretch, nAlignment )
   METHOD  direction()
   METHOD  insertLayout( nIndex, pLayout, nStretch )
   METHOD  insertSpacerItem( nIndex, pSpacerItem )
   METHOD  insertSpacing( nIndex, nSize )
   METHOD  insertStretch( nIndex, nStretch )
   METHOD  insertWidget( nIndex, pWidget, nStretch, nAlignment )
   METHOD  invalidate()
   METHOD  setDirection( nDirection )
   METHOD  setSpacing( nSpacing )
   METHOD  setStretch( nIndex, nStretch )
   METHOD  setStretchFactor( ... )
   METHOD  spacing()
   METHOD  stretch( nIndex )

   ENDCLASS


METHOD QBoxLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QBoxLayout( ... )
   RETURN Self


METHOD QBoxLayout:addLayout( pLayout, nStretch )
   RETURN Qt_QBoxLayout_addLayout( ::pPtr, hbqt_ptr( pLayout ), nStretch )


METHOD QBoxLayout:addSpacerItem( pSpacerItem )
   RETURN Qt_QBoxLayout_addSpacerItem( ::pPtr, hbqt_ptr( pSpacerItem ) )


METHOD QBoxLayout:addSpacing( nSize )
   RETURN Qt_QBoxLayout_addSpacing( ::pPtr, nSize )


METHOD QBoxLayout:addStretch( nStretch )
   RETURN Qt_QBoxLayout_addStretch( ::pPtr, nStretch )


METHOD QBoxLayout:addStrut( nSize )
   RETURN Qt_QBoxLayout_addStrut( ::pPtr, nSize )


METHOD QBoxLayout:addWidget( pWidget, nStretch, nAlignment )
   RETURN Qt_QBoxLayout_addWidget( ::pPtr, hbqt_ptr( pWidget ), nStretch, nAlignment )


METHOD QBoxLayout:direction()
   RETURN Qt_QBoxLayout_direction( ::pPtr )


METHOD QBoxLayout:insertLayout( nIndex, pLayout, nStretch )
   RETURN Qt_QBoxLayout_insertLayout( ::pPtr, nIndex, hbqt_ptr( pLayout ), nStretch )


METHOD QBoxLayout:insertSpacerItem( nIndex, pSpacerItem )
   RETURN Qt_QBoxLayout_insertSpacerItem( ::pPtr, nIndex, hbqt_ptr( pSpacerItem ) )


METHOD QBoxLayout:insertSpacing( nIndex, nSize )
   RETURN Qt_QBoxLayout_insertSpacing( ::pPtr, nIndex, nSize )


METHOD QBoxLayout:insertStretch( nIndex, nStretch )
   RETURN Qt_QBoxLayout_insertStretch( ::pPtr, nIndex, nStretch )


METHOD QBoxLayout:insertWidget( nIndex, pWidget, nStretch, nAlignment )
   RETURN Qt_QBoxLayout_insertWidget( ::pPtr, nIndex, hbqt_ptr( pWidget ), nStretch, nAlignment )


METHOD QBoxLayout:invalidate()
   RETURN Qt_QBoxLayout_invalidate( ::pPtr )


METHOD QBoxLayout:setDirection( nDirection )
   RETURN Qt_QBoxLayout_setDirection( ::pPtr, nDirection )


METHOD QBoxLayout:setSpacing( nSpacing )
   RETURN Qt_QBoxLayout_setSpacing( ::pPtr, nSpacing )


METHOD QBoxLayout:setStretch( nIndex, nStretch )
   RETURN Qt_QBoxLayout_setStretch( ::pPtr, nIndex, nStretch )


METHOD QBoxLayout:setStretchFactor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QWIDGET"
            RETURN Qt_QBoxLayout_setStretchFactor( ::pPtr, ... )
         CASE "QLAYOUT"
            RETURN Qt_QBoxLayout_setStretchFactor_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QBoxLayout:spacing()
   RETURN Qt_QBoxLayout_spacing( ::pPtr )


METHOD QBoxLayout:stretch( nIndex )
   RETURN Qt_QBoxLayout_stretch( ::pPtr, nIndex )

