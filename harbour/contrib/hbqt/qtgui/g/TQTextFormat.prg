/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


#include "hbclass.ch"


REQUEST __HBQTGUI


FUNCTION QTextFormat( ... )
   RETURN HB_QTextFormat():new( ... )

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

