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


FUNCTION QBrush( ... )
   RETURN HB_QBrush():new( ... )

FUNCTION QBrushFromPointer( ... )
   RETURN HB_QBrush():fromPointer( ... )


CREATE CLASS QBrush INHERIT HbQtObjectHandler FUNCTION HB_QBrush

   METHOD  new( ... )

   METHOD  color                         // (  )                                               -> oQColor
   METHOD  gradient                      // (  )                                               -> oQGradient
   METHOD  isOpaque                      // (  )                                               -> lBool
   METHOD  matrix                        // (  )                                               -> oQMatrix
   METHOD  setColor                      // ( oQColor )                                        -> NIL
                                         // ( nColor )                                         -> NIL
   METHOD  setMatrix                     // ( oQMatrix )                                       -> NIL
   METHOD  setStyle                      // ( nStyle )                                         -> NIL
   METHOD  setTexture                    // ( oQPixmap )                                       -> NIL
   METHOD  setTextureImage               // ( oQImage )                                        -> NIL
   METHOD  setTransform                  // ( oQTransform )                                    -> NIL
   METHOD  style                         // (  )                                               -> nQt_BrushStyle
   METHOD  texture                       // (  )                                               -> oQPixmap
   METHOD  textureImage                  // (  )                                               -> oQImage
   METHOD  transform                     // (  )                                               -> oQTransform

   ENDCLASS


METHOD QBrush:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QBrush( ... )
   RETURN Self


METHOD QBrush:color( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QBrush_color( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:gradient( ... )
   SWITCH PCount()
   CASE 0
      RETURN QGradientFromPointer( Qt_QBrush_gradient( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:isOpaque( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QBrush_isOpaque( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:matrix( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMatrixFromPointer( Qt_QBrush_matrix( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:setColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBrush_setColor_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QBrush_setColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:setMatrix( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QBrush_setMatrix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:setStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBrush_setStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:setTexture( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QBrush_setTexture( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:setTextureImage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QBrush_setTextureImage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:setTransform( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QBrush_setTransform( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QBrush_style( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:texture( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_QBrush_texture( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:textureImage( ... )
   SWITCH PCount()
   CASE 0
      RETURN QImageFromPointer( Qt_QBrush_textureImage( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBrush:transform( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTransformFromPointer( Qt_QBrush_transform( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

