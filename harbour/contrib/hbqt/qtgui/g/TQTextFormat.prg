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


FUNCTION QTextFormat( ... )
   RETURN HB_QTextFormat():new( ... )


CREATE CLASS QTextFormat INHERIT HbQtObjectHandler FUNCTION HB_QTextFormat

   METHOD  new( ... )

   METHOD  background()
   METHOD  boolProperty( nPropertyId )
   METHOD  brushProperty( nPropertyId )
   METHOD  clearBackground()
   METHOD  clearForeground()
   METHOD  clearProperty( nPropertyId )
   METHOD  colorProperty( nPropertyId )
   METHOD  doubleProperty( nPropertyId )
   METHOD  foreground()
   METHOD  hasProperty( nPropertyId )
   METHOD  intProperty( nPropertyId )
   METHOD  isBlockFormat()
   METHOD  isCharFormat()
   METHOD  isFrameFormat()
   METHOD  isImageFormat()
   METHOD  isListFormat()
   METHOD  isTableCellFormat()
   METHOD  isTableFormat()
   METHOD  isValid()
   METHOD  layoutDirection()
   METHOD  lengthProperty( nPropertyId )
   METHOD  merge( pOther )
   METHOD  objectIndex()
   METHOD  objectType()
   METHOD  penProperty( nPropertyId )
   METHOD  property( nPropertyId )
   METHOD  propertyCount()
   METHOD  setBackground( pBrush )
   METHOD  setForeground( pBrush )
   METHOD  setLayoutDirection( nDirection )
   METHOD  setObjectIndex( nIndex )
   METHOD  setObjectType( nType )
   METHOD  setProperty( nPropertyId, pValue )
   METHOD  stringProperty( nPropertyId )
   METHOD  toBlockFormat()
   METHOD  toCharFormat()
   METHOD  toFrameFormat()
   METHOD  toImageFormat()
   METHOD  toListFormat()
   METHOD  toTableFormat()
   METHOD  type()

   ENDCLASS


METHOD QTextFormat:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextFormat( ... )
   RETURN Self


METHOD QTextFormat:background()
   RETURN HB_QBrush():from( Qt_QTextFormat_background( ::pPtr ) )


METHOD QTextFormat:boolProperty( nPropertyId )
   RETURN Qt_QTextFormat_boolProperty( ::pPtr, nPropertyId )


METHOD QTextFormat:brushProperty( nPropertyId )
   RETURN HB_QBrush():from( Qt_QTextFormat_brushProperty( ::pPtr, nPropertyId ) )


METHOD QTextFormat:clearBackground()
   RETURN Qt_QTextFormat_clearBackground( ::pPtr )


METHOD QTextFormat:clearForeground()
   RETURN Qt_QTextFormat_clearForeground( ::pPtr )


METHOD QTextFormat:clearProperty( nPropertyId )
   RETURN Qt_QTextFormat_clearProperty( ::pPtr, nPropertyId )


METHOD QTextFormat:colorProperty( nPropertyId )
   RETURN HB_QColor():from( Qt_QTextFormat_colorProperty( ::pPtr, nPropertyId ) )


METHOD QTextFormat:doubleProperty( nPropertyId )
   RETURN Qt_QTextFormat_doubleProperty( ::pPtr, nPropertyId )


METHOD QTextFormat:foreground()
   RETURN HB_QBrush():from( Qt_QTextFormat_foreground( ::pPtr ) )


METHOD QTextFormat:hasProperty( nPropertyId )
   RETURN Qt_QTextFormat_hasProperty( ::pPtr, nPropertyId )


METHOD QTextFormat:intProperty( nPropertyId )
   RETURN Qt_QTextFormat_intProperty( ::pPtr, nPropertyId )


METHOD QTextFormat:isBlockFormat()
   RETURN Qt_QTextFormat_isBlockFormat( ::pPtr )


METHOD QTextFormat:isCharFormat()
   RETURN Qt_QTextFormat_isCharFormat( ::pPtr )


METHOD QTextFormat:isFrameFormat()
   RETURN Qt_QTextFormat_isFrameFormat( ::pPtr )


METHOD QTextFormat:isImageFormat()
   RETURN Qt_QTextFormat_isImageFormat( ::pPtr )


METHOD QTextFormat:isListFormat()
   RETURN Qt_QTextFormat_isListFormat( ::pPtr )


METHOD QTextFormat:isTableCellFormat()
   RETURN Qt_QTextFormat_isTableCellFormat( ::pPtr )


METHOD QTextFormat:isTableFormat()
   RETURN Qt_QTextFormat_isTableFormat( ::pPtr )


METHOD QTextFormat:isValid()
   RETURN Qt_QTextFormat_isValid( ::pPtr )


METHOD QTextFormat:layoutDirection()
   RETURN Qt_QTextFormat_layoutDirection( ::pPtr )


METHOD QTextFormat:lengthProperty( nPropertyId )
   RETURN HB_QTextLength():from( Qt_QTextFormat_lengthProperty( ::pPtr, nPropertyId ) )


METHOD QTextFormat:merge( pOther )
   RETURN Qt_QTextFormat_merge( ::pPtr, hbqt_ptr( pOther ) )


METHOD QTextFormat:objectIndex()
   RETURN Qt_QTextFormat_objectIndex( ::pPtr )


METHOD QTextFormat:objectType()
   RETURN Qt_QTextFormat_objectType( ::pPtr )


METHOD QTextFormat:penProperty( nPropertyId )
   RETURN HB_QPen():from( Qt_QTextFormat_penProperty( ::pPtr, nPropertyId ) )


METHOD QTextFormat:property( nPropertyId )
   RETURN HB_QVariant():from( Qt_QTextFormat_property( ::pPtr, nPropertyId ) )


METHOD QTextFormat:propertyCount()
   RETURN Qt_QTextFormat_propertyCount( ::pPtr )


METHOD QTextFormat:setBackground( pBrush )
   RETURN Qt_QTextFormat_setBackground( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QTextFormat:setForeground( pBrush )
   RETURN Qt_QTextFormat_setForeground( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QTextFormat:setLayoutDirection( nDirection )
   RETURN Qt_QTextFormat_setLayoutDirection( ::pPtr, nDirection )


METHOD QTextFormat:setObjectIndex( nIndex )
   RETURN Qt_QTextFormat_setObjectIndex( ::pPtr, nIndex )


METHOD QTextFormat:setObjectType( nType )
   RETURN Qt_QTextFormat_setObjectType( ::pPtr, nType )


METHOD QTextFormat:setProperty( nPropertyId, pValue )
   RETURN Qt_QTextFormat_setProperty( ::pPtr, nPropertyId, hbqt_ptr( pValue ) )


METHOD QTextFormat:stringProperty( nPropertyId )
   RETURN Qt_QTextFormat_stringProperty( ::pPtr, nPropertyId )


METHOD QTextFormat:toBlockFormat()
   RETURN HB_QTextBlockFormat():from( Qt_QTextFormat_toBlockFormat( ::pPtr ) )


METHOD QTextFormat:toCharFormat()
   RETURN HB_QTextCharFormat():from( Qt_QTextFormat_toCharFormat( ::pPtr ) )


METHOD QTextFormat:toFrameFormat()
   RETURN HB_QTextFrameFormat():from( Qt_QTextFormat_toFrameFormat( ::pPtr ) )


METHOD QTextFormat:toImageFormat()
   RETURN HB_QTextImageFormat():from( Qt_QTextFormat_toImageFormat( ::pPtr ) )


METHOD QTextFormat:toListFormat()
   RETURN HB_QTextListFormat():from( Qt_QTextFormat_toListFormat( ::pPtr ) )


METHOD QTextFormat:toTableFormat()
   RETURN HB_QTextTableFormat():from( Qt_QTextFormat_toTableFormat( ::pPtr ) )


METHOD QTextFormat:type()
   RETURN Qt_QTextFormat_type( ::pPtr )

