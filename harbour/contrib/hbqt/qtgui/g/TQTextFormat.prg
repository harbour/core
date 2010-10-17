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

FUNCTION QTextFormatFrom( ... )
   RETURN HB_QTextFormat():from( ... )

FUNCTION QTextFormatFromPointer( ... )
   RETURN HB_QTextFormat():fromPointer( ... )


CREATE CLASS QTextFormat INHERIT HbQtObjectHandler FUNCTION HB_QTextFormat

   METHOD  new( ... )

   METHOD  background                    // (  )                                               -> oQBrush
   METHOD  boolProperty                  // ( nPropertyId )                                    -> lBool
   METHOD  brushProperty                 // ( nPropertyId )                                    -> oQBrush
   METHOD  clearBackground               // (  )                                               -> NIL
   METHOD  clearForeground               // (  )                                               -> NIL
   METHOD  clearProperty                 // ( nPropertyId )                                    -> NIL
   METHOD  colorProperty                 // ( nPropertyId )                                    -> oQColor
   METHOD  doubleProperty                // ( nPropertyId )                                    -> nQreal
   METHOD  foreground                    // (  )                                               -> oQBrush
   METHOD  hasProperty                   // ( nPropertyId )                                    -> lBool
   METHOD  intProperty                   // ( nPropertyId )                                    -> nInt
   METHOD  isBlockFormat                 // (  )                                               -> lBool
   METHOD  isCharFormat                  // (  )                                               -> lBool
   METHOD  isFrameFormat                 // (  )                                               -> lBool
   METHOD  isImageFormat                 // (  )                                               -> lBool
   METHOD  isListFormat                  // (  )                                               -> lBool
   METHOD  isTableCellFormat             // (  )                                               -> lBool
   METHOD  isTableFormat                 // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  layoutDirection               // (  )                                               -> nQt_LayoutDirection
   METHOD  lengthProperty                // ( nPropertyId )                                    -> oQTextLength
   METHOD  merge                         // ( oQTextFormat )                                   -> NIL
   METHOD  objectIndex                   // (  )                                               -> nInt
   METHOD  objectType                    // (  )                                               -> nInt
   METHOD  penProperty                   // ( nPropertyId )                                    -> oQPen
   METHOD  property                      // ( nPropertyId )                                    -> oQVariant
   METHOD  propertyCount                 // (  )                                               -> nInt
   METHOD  setBackground                 // ( oQBrush )                                        -> NIL
   METHOD  setForeground                 // ( oQBrush )                                        -> NIL
   METHOD  setLayoutDirection            // ( nDirection )                                     -> NIL
   METHOD  setObjectIndex                // ( nIndex )                                         -> NIL
   METHOD  setObjectType                 // ( nType )                                          -> NIL
   METHOD  setProperty                   // ( nPropertyId, oQVariant )                         -> NIL
   METHOD  stringProperty                // ( nPropertyId )                                    -> cQString
   METHOD  toBlockFormat                 // (  )                                               -> oQTextBlockFormat
   METHOD  toCharFormat                  // (  )                                               -> oQTextCharFormat
   METHOD  toFrameFormat                 // (  )                                               -> oQTextFrameFormat
   METHOD  toImageFormat                 // (  )                                               -> oQTextImageFormat
   METHOD  toListFormat                  // (  )                                               -> oQTextListFormat
   METHOD  toTableFormat                 // (  )                                               -> oQTextTableFormat
   METHOD  type                          // (  )                                               -> nInt

   ENDCLASS


METHOD QTextFormat:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextFormat( ... )
   RETURN Self


METHOD QTextFormat:background( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QTextFormat_background( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:boolProperty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_boolProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:brushProperty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QBrushFromPointer( Qt_QTextFormat_brushProperty( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:clearBackground( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_clearBackground( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:clearForeground( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_clearForeground( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:clearProperty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_clearProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:colorProperty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QTextFormat_colorProperty( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:doubleProperty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_doubleProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:foreground( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QTextFormat_foreground( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:hasProperty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_hasProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:intProperty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_intProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:isBlockFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_isBlockFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:isCharFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_isCharFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:isFrameFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_isFrameFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:isImageFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_isImageFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:isListFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_isListFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:isTableCellFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_isTableCellFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:isTableFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_isTableFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:layoutDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_layoutDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:lengthProperty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTextLengthFromPointer( Qt_QTextFormat_lengthProperty( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:merge( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_merge( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:objectIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_objectIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:objectType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_objectType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:penProperty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPenFromPointer( Qt_QTextFormat_penProperty( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:property( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QTextFormat_property( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:propertyCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_propertyCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:setBackground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_setBackground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:setForeground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_setForeground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:setLayoutDirection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_setLayoutDirection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:setObjectIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_setObjectIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:setObjectType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_setObjectType( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:setProperty( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTextFormat_setProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:stringProperty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextFormat_stringProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:toBlockFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextBlockFormatFromPointer( Qt_QTextFormat_toBlockFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:toCharFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCharFormatFromPointer( Qt_QTextFormat_toCharFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:toFrameFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextFrameFormatFromPointer( Qt_QTextFormat_toFrameFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:toImageFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextImageFormatFromPointer( Qt_QTextFormat_toImageFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:toListFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextListFormatFromPointer( Qt_QTextFormat_toListFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:toTableFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextTableFormatFromPointer( Qt_QTextFormat_toTableFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextFormat:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextFormat_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

