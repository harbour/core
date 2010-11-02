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


FUNCTION QPicture( ... )
   RETURN HB_QPicture():new( ... )

FUNCTION QPictureFromPointer( ... )
   RETURN HB_QPicture():fromPointer( ... )


CREATE CLASS QPicture INHERIT HbQtObjectHandler, HB_QPaintDevice FUNCTION HB_QPicture

   METHOD  new( ... )

   METHOD  boundingRect                  // (  )                                               -> oQRect
   METHOD  data                          // (  )                                               -> cChar
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  load                          // ( cFileName, cFormat )                             -> lBool
                                         // ( oQIODevice, cFormat )                            -> lBool
   METHOD  play                          // ( oQPainter )                                      -> lBool
   METHOD  save                          // ( cFileName, cFormat )                             -> lBool
                                         // ( oQIODevice, cFormat )                            -> lBool
   METHOD  setBoundingRect               // ( oQRect )                                         -> NIL
   METHOD  setData                       // ( cData, nSize )                                   -> NIL
   METHOD  size                          // (  )                                               -> nUint

   ENDCLASS


METHOD QPicture:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPicture( ... )
   RETURN Self


METHOD QPicture:boundingRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QPicture_boundingRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPicture:data( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPicture_data( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPicture:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPicture_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPicture:load( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QPicture_load( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QPicture_load_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPicture_load( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPicture_load_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPicture:play( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPicture_play( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPicture:save( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QPicture_save( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QPicture_save_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPicture_save( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPicture_save_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPicture:setBoundingRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPicture_setBoundingRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPicture:setData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPicture_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPicture:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPicture_size( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

