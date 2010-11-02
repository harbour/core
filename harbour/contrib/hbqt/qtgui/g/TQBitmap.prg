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


FUNCTION QBitmap( ... )
   RETURN HB_QBitmap():new( ... )

FUNCTION QBitmapFromPointer( ... )
   RETURN HB_QBitmap():fromPointer( ... )


CREATE CLASS QBitmap INHERIT HbQtObjectHandler, HB_QPixmap FUNCTION HB_QBitmap

   METHOD  new( ... )

   METHOD  QBitmap                       // (  )                                               -> oQBitmap
                                         // ( oQPixmap )                                       -> oQBitmap
                                         // ( nWidth, nHeight )                                -> oQBitmap
                                         // ( oQSize )                                         -> oQBitmap
                                         // ( cFileName, cFormat )                             -> oQBitmap
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  transformed                   // ( oQTransform )                                    -> oQBitmap
                                         // ( oQMatrix )                                       -> oQBitmap
   METHOD  fromImage                     // ( oQImage, nFlags )                                -> oQBitmap

   ENDCLASS


METHOD QBitmap:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QBitmap( ... )
   RETURN Self


METHOD QBitmap:QBitmap( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QBitmapFromPointer( Qt_QBitmap_QBitmap_4( ::pPtr, ... ) )
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QBitmapFromPointer( Qt_QBitmap_QBitmap_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QBitmapFromPointer( Qt_QBitmap_QBitmap_4( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPIXMAP"
            RETURN QBitmapFromPointer( Qt_QBitmap_QBitmap_1( ::pPtr, ... ) )
         CASE "QSIZE"
            RETURN QBitmapFromPointer( Qt_QBitmap_QBitmap_3( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN QBitmapFromPointer( Qt_QBitmap_QBitmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitmap:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QBitmap_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitmap:transformed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTRANSFORM"
            RETURN QBitmapFromPointer( Qt_QBitmap_transformed( ::pPtr, ... ) )
         CASE "QMATRIX"
            RETURN QBitmapFromPointer( Qt_QBitmap_transformed_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBitmap:fromImage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QBitmapFromPointer( Qt_QBitmap_fromImage( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QBitmapFromPointer( Qt_QBitmap_fromImage( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

