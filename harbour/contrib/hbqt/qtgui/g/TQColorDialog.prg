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


FUNCTION QColorDialog( ... )
   RETURN HB_QColorDialog():new( ... )


CREATE CLASS QColorDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QColorDialog

   METHOD  new( ... )

   METHOD  currentColor()
   METHOD  open()
   METHOD  options()
   METHOD  selectedColor()
   METHOD  setCurrentColor( pColor )
   METHOD  setOption( nOption, lOn )
   METHOD  setOptions( nOptions )
   METHOD  setVisible( lVisible )
   METHOD  testOption( nOption )
   METHOD  customColor( nIndex )
   METHOD  customCount()
   METHOD  getColor( ... )
   METHOD  setCustomColor( nIndex, nColor )
   METHOD  setStandardColor( nIndex, nColor )

   ENDCLASS


METHOD QColorDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QColorDialog( ... )
   RETURN Self


METHOD QColorDialog:currentColor()
   RETURN HB_QColor():from( Qt_QColorDialog_currentColor( ::pPtr ) )


METHOD QColorDialog:open()
   RETURN Qt_QColorDialog_open( ::pPtr )


METHOD QColorDialog:options()
   RETURN Qt_QColorDialog_options( ::pPtr )


METHOD QColorDialog:selectedColor()
   RETURN HB_QColor():from( Qt_QColorDialog_selectedColor( ::pPtr ) )


METHOD QColorDialog:setCurrentColor( pColor )
   RETURN Qt_QColorDialog_setCurrentColor( ::pPtr, hbqt_ptr( pColor ) )


METHOD QColorDialog:setOption( nOption, lOn )
   RETURN Qt_QColorDialog_setOption( ::pPtr, nOption, lOn )


METHOD QColorDialog:setOptions( nOptions )
   RETURN Qt_QColorDialog_setOptions( ::pPtr, nOptions )


METHOD QColorDialog:setVisible( lVisible )
   RETURN Qt_QColorDialog_setVisible( ::pPtr, lVisible )


METHOD QColorDialog:testOption( nOption )
   RETURN Qt_QColorDialog_testOption( ::pPtr, nOption )


METHOD QColorDialog:customColor( nIndex )
   RETURN Qt_QColorDialog_customColor( ::pPtr, nIndex )


METHOD QColorDialog:customCount()
   RETURN Qt_QColorDialog_customCount( ::pPtr )


METHOD QColorDialog:getColor( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN HB_QColor():from( Qt_QColorDialog_getColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN HB_QColor():from( Qt_QColorDialog_getColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN HB_QColor():from( Qt_QColorDialog_getColor_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QColor():from( Qt_QColorDialog_getColor_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QColorDialog:setCustomColor( nIndex, nColor )
   RETURN Qt_QColorDialog_setCustomColor( ::pPtr, nIndex, nColor )


METHOD QColorDialog:setStandardColor( nIndex, nColor )
   RETURN Qt_QColorDialog_setStandardColor( ::pPtr, nIndex, nColor )

