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


REQUEST __HBQTCORE


FUNCTION QDataStream( ... )
   RETURN HB_QDataStream():new( ... )

FUNCTION QDataStreamFromPointer( ... )
   RETURN HB_QDataStream():fromPointer( ... )


CREATE CLASS QDataStream INHERIT HbQtObjectHandler FUNCTION HB_QDataStream

   METHOD  new( ... )

   METHOD  atEnd                         // (  )                                               -> lBool
   METHOD  byteOrder                     // (  )                                               -> nByteOrder
   METHOD  device                        // (  )                                               -> oQIODevice
   METHOD  readRawData                   // ( cS, nLen )                                       -> nInt
   METHOD  resetStatus                   // (  )                                               -> NIL
   METHOD  setByteOrder                  // ( nBo )                                            -> NIL
   METHOD  setDevice                     // ( oQIODevice )                                     -> NIL
   METHOD  setStatus                     // ( nStatus )                                        -> NIL
   METHOD  setVersion                    // ( nV )                                             -> NIL
   METHOD  skipRawData                   // ( nLen )                                           -> nInt
   METHOD  status                        // (  )                                               -> nStatus
   METHOD  version                       // (  )                                               -> nInt
   METHOD  writeRawData                  // ( cS, nLen )                                       -> nInt

   ENDCLASS


METHOD QDataStream:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDataStream( ... )
   RETURN Self


METHOD QDataStream:atEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDataStream_atEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:byteOrder( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDataStream_byteOrder( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:device( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIODeviceFromPointer( Qt_QDataStream_device( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:readRawData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDataStream_readRawData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:resetStatus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDataStream_resetStatus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:setByteOrder( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDataStream_setByteOrder( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:setDevice( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDataStream_setDevice( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:setStatus( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDataStream_setStatus( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:setVersion( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDataStream_setVersion( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:skipRawData( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDataStream_skipRawData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:status( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDataStream_status( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:version( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDataStream_version( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDataStream:writeRawData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDataStream_writeRawData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

