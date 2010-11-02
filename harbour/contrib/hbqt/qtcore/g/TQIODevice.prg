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


FUNCTION QIODevice( ... )
   RETURN HB_QIODevice():new( ... )

FUNCTION QIODeviceFromPointer( ... )
   RETURN HB_QIODevice():fromPointer( ... )


CREATE CLASS QIODevice INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QIODevice

   METHOD  new( ... )

   METHOD  atEnd                         // (  )                                               -> lBool
   METHOD  bytesAvailable                // (  )                                               -> nQint64
   METHOD  bytesToWrite                  // (  )                                               -> nQint64
   METHOD  canReadLine                   // (  )                                               -> lBool
   METHOD  close                         // (  )                                               -> NIL
   METHOD  errorString                   // (  )                                               -> cQString
   METHOD  getChar                       // ( cC )                                             -> lBool
   METHOD  isOpen                        // (  )                                               -> lBool
   METHOD  isReadable                    // (  )                                               -> lBool
   METHOD  isSequential                  // (  )                                               -> lBool
   METHOD  isTextModeEnabled             // (  )                                               -> lBool
   METHOD  isWritable                    // (  )                                               -> lBool
   METHOD  open                          // ( nMode )                                          -> lBool
   METHOD  openMode                      // (  )                                               -> nOpenMode
   METHOD  peek                          // ( cData, nMaxSize )                                -> nQint64
                                         // ( nMaxSize )                                       -> oQByteArray
   METHOD  pos                           // (  )                                               -> nQint64
   METHOD  putChar                       // ( nC )                                             -> lBool
   METHOD  read                          // ( cData, nMaxSize )                                -> nQint64
                                         // ( nMaxSize )                                       -> oQByteArray
   METHOD  readAll                       // (  )                                               -> oQByteArray
   METHOD  readLine                      // ( cData, nMaxSize )                                -> nQint64
                                         // ( nMaxSize )                                       -> oQByteArray
   METHOD  reset                         // (  )                                               -> lBool
   METHOD  seek                          // ( nPos )                                           -> lBool
   METHOD  setTextModeEnabled            // ( lEnabled )                                       -> NIL
   METHOD  size                          // (  )                                               -> nQint64
   METHOD  ungetChar                     // ( nC )                                             -> NIL
   METHOD  waitForBytesWritten           // ( nMsecs )                                         -> lBool
   METHOD  waitForReadyRead              // ( nMsecs )                                         -> lBool
   METHOD  write                         // ( cData, nMaxSize )                                -> nQint64
                                         // ( cData )                                          -> nQint64
                                         // ( oQByteArray )                                    -> nQint64

   ENDCLASS


METHOD QIODevice:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QIODevice( ... )
   RETURN Self


METHOD QIODevice:atEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_atEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:bytesAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_bytesAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:bytesToWrite( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_bytesToWrite( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:canReadLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_canReadLine( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:close( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_close( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:errorString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_errorString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:getChar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_getChar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:isOpen( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_isOpen( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:isReadable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_isReadable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:isSequential( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_isSequential( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:isTextModeEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_isTextModeEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:isWritable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_isWritable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:open( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_open( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:openMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_openMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:peek( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QIODevice_peek( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QIODevice_peek_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_pos( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:putChar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_putChar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:read( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QIODevice_read( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QIODevice_read_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:readAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QIODevice_readAll( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:readLine( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QIODevice_readLine( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QIODevice_readLine_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QIODevice_readLine_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:reset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_reset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:seek( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_seek( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:setTextModeEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_setTextModeEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QIODevice_size( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:ungetChar( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_ungetChar( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:waitForBytesWritten( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_waitForBytesWritten( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:waitForReadyRead( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_waitForReadyRead( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QIODevice:write( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QIODevice_write( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_write_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QIODevice_write_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

