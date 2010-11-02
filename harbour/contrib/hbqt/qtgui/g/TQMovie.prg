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


FUNCTION QMovie( ... )
   RETURN HB_QMovie():new( ... )

FUNCTION QMovieFromPointer( ... )
   RETURN HB_QMovie():fromPointer( ... )


CREATE CLASS QMovie INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QMovie

   METHOD  new( ... )

   METHOD  backgroundColor               // (  )                                               -> oQColor
   METHOD  cacheMode                     // (  )                                               -> nCacheMode
   METHOD  currentFrameNumber            // (  )                                               -> nInt
   METHOD  currentImage                  // (  )                                               -> oQImage
   METHOD  currentPixmap                 // (  )                                               -> oQPixmap
   METHOD  device                        // (  )                                               -> oQIODevice
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  format                        // (  )                                               -> oQByteArray
   METHOD  frameCount                    // (  )                                               -> nInt
   METHOD  frameRect                     // (  )                                               -> oQRect
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  jumpToFrame                   // ( nFrameNumber )                                   -> lBool
   METHOD  loopCount                     // (  )                                               -> nInt
   METHOD  nextFrameDelay                // (  )                                               -> nInt
   METHOD  scaledSize                    // (  )                                               -> oQSize
   METHOD  setBackgroundColor            // ( oQColor )                                        -> NIL
   METHOD  setCacheMode                  // ( nMode )                                          -> NIL
   METHOD  setDevice                     // ( oQIODevice )                                     -> NIL
   METHOD  setFileName                   // ( cFileName )                                      -> NIL
   METHOD  setFormat                     // ( oQByteArray )                                    -> NIL
   METHOD  setScaledSize                 // ( oQSize )                                         -> NIL
   METHOD  speed                         // (  )                                               -> nInt
   METHOD  state                         // (  )                                               -> nMovieState
   METHOD  jumpToNextFrame               // (  )                                               -> lBool
   METHOD  setPaused                     // ( lPaused )                                        -> NIL
   METHOD  setSpeed                      // ( nPercentSpeed )                                  -> NIL
   METHOD  start                         // (  )                                               -> NIL
   METHOD  stop                          // (  )                                               -> NIL

   ENDCLASS


METHOD QMovie:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMovie( ... )
   RETURN Self


METHOD QMovie:backgroundColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QMovie_backgroundColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:cacheMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_cacheMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:currentFrameNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_currentFrameNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:currentImage( ... )
   SWITCH PCount()
   CASE 0
      RETURN QImageFromPointer( Qt_QMovie_currentImage( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:currentPixmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_QMovie_currentPixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:device( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIODeviceFromPointer( Qt_QMovie_device( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:format( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QMovie_format( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:frameCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_frameCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:frameRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QMovie_frameRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:jumpToFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_jumpToFrame( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:loopCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_loopCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:nextFrameDelay( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_nextFrameDelay( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:scaledSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QMovie_scaledSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setBackgroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setCacheMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setCacheMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setDevice( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setDevice( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setFileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setFileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setScaledSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setScaledSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:speed( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_speed( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:state( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_state( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:jumpToNextFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_jumpToNextFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setPaused( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setPaused( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:setSpeed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QMovie_setSpeed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:start( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_start( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMovie:stop( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMovie_stop( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

