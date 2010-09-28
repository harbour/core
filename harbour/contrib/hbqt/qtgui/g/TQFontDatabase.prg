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


FUNCTION QFontDatabase( ... )
   RETURN HB_QFontDatabase():new( ... )


CREATE CLASS QFontDatabase INHERIT HbQtObjectHandler FUNCTION HB_QFontDatabase

   METHOD  new( ... )

   METHOD  bold( cFamily, cStyle )
   METHOD  families( nWritingSystem )
   METHOD  font( cFamily, cStyle, nPointSize )
   METHOD  isBitmapScalable( cFamily, cStyle )
   METHOD  isFixedPitch( cFamily, cStyle )
   METHOD  isScalable( cFamily, cStyle )
   METHOD  isSmoothlyScalable( cFamily, cStyle )
   METHOD  italic( cFamily, cStyle )
   METHOD  pointSizes( cFamily, cStyle )
   METHOD  smoothSizes( cFamily, cStyle )
   METHOD  styleString( ... )
   METHOD  styles( cFamily )
   METHOD  weight( cFamily, cStyle )
   METHOD  addApplicationFont( cFileName )
   METHOD  addApplicationFontFromData( pFontData )
   METHOD  applicationFontFamilies( nId )
   METHOD  removeAllApplicationFonts()
   METHOD  removeApplicationFont( nId )
   METHOD  standardSizes()
   METHOD  supportsThreadedFontRendering()
   METHOD  writingSystemName( nWritingSystem )
   METHOD  writingSystemSample( nWritingSystem )

   ENDCLASS


METHOD QFontDatabase:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontDatabase( ... )
   RETURN Self


METHOD QFontDatabase:bold( cFamily, cStyle )
   RETURN Qt_QFontDatabase_bold( ::pPtr, cFamily, cStyle )


METHOD QFontDatabase:families( nWritingSystem )
   RETURN HB_QStringList():from( Qt_QFontDatabase_families( ::pPtr, nWritingSystem ) )


METHOD QFontDatabase:font( cFamily, cStyle, nPointSize )
   RETURN HB_QFont():from( Qt_QFontDatabase_font( ::pPtr, cFamily, cStyle, nPointSize ) )


METHOD QFontDatabase:isBitmapScalable( cFamily, cStyle )
   RETURN Qt_QFontDatabase_isBitmapScalable( ::pPtr, cFamily, cStyle )


METHOD QFontDatabase:isFixedPitch( cFamily, cStyle )
   RETURN Qt_QFontDatabase_isFixedPitch( ::pPtr, cFamily, cStyle )


METHOD QFontDatabase:isScalable( cFamily, cStyle )
   RETURN Qt_QFontDatabase_isScalable( ::pPtr, cFamily, cStyle )


METHOD QFontDatabase:isSmoothlyScalable( cFamily, cStyle )
   RETURN Qt_QFontDatabase_isSmoothlyScalable( ::pPtr, cFamily, cStyle )


METHOD QFontDatabase:italic( cFamily, cStyle )
   RETURN Qt_QFontDatabase_italic( ::pPtr, cFamily, cStyle )


METHOD QFontDatabase:pointSizes( cFamily, cStyle )
   RETURN HB_QList():from( Qt_QFontDatabase_pointSizes( ::pPtr, cFamily, cStyle ) )


METHOD QFontDatabase:smoothSizes( cFamily, cStyle )
   RETURN HB_QList():from( Qt_QFontDatabase_smoothSizes( ::pPtr, cFamily, cStyle ) )


METHOD QFontDatabase:styleString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QFONT"
            RETURN Qt_QFontDatabase_styleString( ::pPtr, ... )
         CASE "QFONTINFO"
            RETURN Qt_QFontDatabase_styleString_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFontDatabase:styles( cFamily )
   RETURN HB_QStringList():from( Qt_QFontDatabase_styles( ::pPtr, cFamily ) )


METHOD QFontDatabase:weight( cFamily, cStyle )
   RETURN Qt_QFontDatabase_weight( ::pPtr, cFamily, cStyle )


METHOD QFontDatabase:addApplicationFont( cFileName )
   RETURN Qt_QFontDatabase_addApplicationFont( ::pPtr, cFileName )


METHOD QFontDatabase:addApplicationFontFromData( pFontData )
   RETURN Qt_QFontDatabase_addApplicationFontFromData( ::pPtr, hbqt_ptr( pFontData ) )


METHOD QFontDatabase:applicationFontFamilies( nId )
   RETURN HB_QStringList():from( Qt_QFontDatabase_applicationFontFamilies( ::pPtr, nId ) )


METHOD QFontDatabase:removeAllApplicationFonts()
   RETURN Qt_QFontDatabase_removeAllApplicationFonts( ::pPtr )


METHOD QFontDatabase:removeApplicationFont( nId )
   RETURN Qt_QFontDatabase_removeApplicationFont( ::pPtr, nId )


METHOD QFontDatabase:standardSizes()
   RETURN HB_QList():from( Qt_QFontDatabase_standardSizes( ::pPtr ) )


METHOD QFontDatabase:supportsThreadedFontRendering()
   RETURN Qt_QFontDatabase_supportsThreadedFontRendering( ::pPtr )


METHOD QFontDatabase:writingSystemName( nWritingSystem )
   RETURN Qt_QFontDatabase_writingSystemName( ::pPtr, nWritingSystem )


METHOD QFontDatabase:writingSystemSample( nWritingSystem )
   RETURN Qt_QFontDatabase_writingSystemSample( ::pPtr, nWritingSystem )

