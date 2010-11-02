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


FUNCTION QPixmap( ... )
   RETURN HB_QPixmap():new( ... )

FUNCTION QPixmapFromPointer( ... )
   RETURN HB_QPixmap():fromPointer( ... )


CREATE CLASS QPixmap INHERIT HbQtObjectHandler, HB_QPaintDevice FUNCTION HB_QPixmap

   METHOD  new( ... )

   METHOD  alphaChannel                  // (  )                                               -> oQPixmap
   METHOD  cacheKey                      // (  )                                               -> nQint64
   METHOD  copy                          // ( oQRect )                                         -> oQPixmap
                                         // ( nX, nY, nWidth, nHeight )                        -> oQPixmap
   METHOD  createHeuristicMask           // ( lClipTight )                                     -> oQBitmap
   METHOD  createMaskFromColor           // ( oQColor, nMode )                                 -> oQBitmap
                                         // ( oQColor )                                        -> oQBitmap
   METHOD  depth                         // (  )                                               -> nInt
   METHOD  detach                        // (  )                                               -> NIL
   METHOD  fill                          // ( oQColor )                                        -> NIL
                                         // ( oQWidget, oQPoint )                              -> NIL
                                         // ( oQWidget, nX, nY )                               -> NIL
   METHOD  hasAlpha                      // (  )                                               -> lBool
   METHOD  hasAlphaChannel               // (  )                                               -> lBool
   METHOD  height                        // (  )                                               -> nInt
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isQBitmap                     // (  )                                               -> lBool
   METHOD  load                          // ( cFileName, cFormat, nFlags )                     -> lBool
   METHOD  loadFromData                  // ( oQByteArray, cFormat, nFlags )                   -> lBool
   METHOD  mask                          // (  )                                               -> oQBitmap
   METHOD  rect                          // (  )                                               -> oQRect
   METHOD  save                          // ( cFileName, cFormat, nQuality )                   -> lBool
                                         // ( oQIODevice, cFormat, nQuality )                  -> lBool
   METHOD  scaled                        // ( nWidth, nHeight, nAspectRatioMode, nTransformMode ) -> oQPixmap
                                         // ( oQSize, nAspectRatioMode, nTransformMode )       -> oQPixmap
   METHOD  scaledToHeight                // ( nHeight, nMode )                                 -> oQPixmap
   METHOD  scaledToWidth                 // ( nWidth, nMode )                                  -> oQPixmap
   METHOD  setAlphaChannel               // ( oQPixmap )                                       -> NIL
   METHOD  setMask                       // ( oQBitmap )                                       -> NIL
   METHOD  size                          // (  )                                               -> oQSize
   METHOD  toImage                       // (  )                                               -> oQImage
   METHOD  transformed                   // ( oQTransform, nMode )                             -> oQPixmap
                                         // ( oQMatrix, nMode )                                -> oQPixmap
   METHOD  width                         // (  )                                               -> nInt
   METHOD  defaultDepth                  // (  )                                               -> nInt
   METHOD  fromImage                     // ( oQImage, nFlags )                                -> oQPixmap
   METHOD  grabWidget                    // ( oQWidget, oQRect )                               -> oQPixmap
                                         // ( oQWidget, nX, nY, nWidth, nHeight )              -> oQPixmap
   METHOD  trueMatrix                    // ( oQTransform, nWidth, nHeight )                   -> oQTransform
                                         // ( oQMatrix, nW, nH )                               -> oQMatrix

   ENDCLASS


METHOD QPixmap:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPixmap( ... )
   RETURN Self


METHOD QPixmap:alphaChannel( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_QPixmap_alphaChannel( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:cacheKey( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPixmap_cacheKey( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:copy( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_copy_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_copy( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QPixmapFromPointer( Qt_QPixmap_copy( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:createHeuristicMask( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN QBitmapFromPointer( Qt_QPixmap_createHeuristicMask( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QBitmapFromPointer( Qt_QPixmap_createHeuristicMask( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:createMaskFromColor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QBitmapFromPointer( Qt_QPixmap_createMaskFromColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QBitmapFromPointer( Qt_QPixmap_createMaskFromColor_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:depth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPixmap_depth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:detach( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPixmap_detach( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:fill( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPixmap_fill_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPixmap_fill_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPixmap_fill( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QPixmap_fill( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:hasAlpha( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPixmap_hasAlpha( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:hasAlphaChannel( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPixmap_hasAlphaChannel( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPixmap_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPixmap_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:isQBitmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPixmap_isQBitmap( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:load( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPixmap_load( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QPixmap_load( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPixmap_load( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:loadFromData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPixmap_loadFromData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QPixmap_loadFromData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPixmap_loadFromData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:mask( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBitmapFromPointer( Qt_QPixmap_mask( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:rect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QPixmap_rect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:save( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPixmap_save( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QPixmap_save_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QPixmap_save( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QPixmap_save_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPixmap_save( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPixmap_save_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:scaled( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_scaled( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_scaled( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_scaled_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_scaled( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_scaled_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_scaled_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:scaledToHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_scaledToHeight( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_scaledToHeight( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:scaledToWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_scaledToWidth( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_scaledToWidth( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:setAlphaChannel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPixmap_setAlphaChannel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:setMask( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPixmap_setMask( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QPixmap_size( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:toImage( ... )
   SWITCH PCount()
   CASE 0
      RETURN QImageFromPointer( Qt_QPixmap_toImage( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:transformed( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN QPixmapFromPointer( Qt_QPixmap_transformed( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN QPixmapFromPointer( Qt_QPixmap_transformed_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN QPixmapFromPointer( Qt_QPixmap_transformed( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN QPixmapFromPointer( Qt_QPixmap_transformed_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPixmap_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:defaultDepth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPixmap_defaultDepth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:fromImage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_fromImage( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_fromImage( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:grabWidget( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_grabWidget_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_grabWidget_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_grabWidget_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_grabWidget_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_grabWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QPixmap_grabWidget_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPixmap:trueMatrix( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN QTransformFromPointer( Qt_QPixmap_trueMatrix( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN QMatrixFromPointer( Qt_QPixmap_trueMatrix_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

