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


REQUEST __HBQTNETWORK


FUNCTION QFtp( ... )
   RETURN HB_QFtp():new( ... )

FUNCTION QFtpFromPointer( ... )
   RETURN HB_QFtp():fromPointer( ... )


CREATE CLASS QFtp INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QFtp

   METHOD  new( ... )

   METHOD  bytesAvailable                // (  )                                               -> nQint64
   METHOD  cd                            // ( cDir )                                           -> nInt
   METHOD  clearPendingCommands          // (  )                                               -> NIL
   METHOD  close                         // (  )                                               -> nInt
   METHOD  connectToHost                 // ( cHost, nPort )                                   -> nInt
   METHOD  currentCommand                // (  )                                               -> nCommand
   METHOD  currentDevice                 // (  )                                               -> oQIODevice
   METHOD  currentId                     // (  )                                               -> nInt
   METHOD  error                         // (  )                                               -> nError
   METHOD  errorString                   // (  )                                               -> cQString
   METHOD  get                           // ( cFile, oQIODevice, nType )                       -> nInt
   METHOD  hasPendingCommands            // (  )                                               -> lBool
   METHOD  list                          // ( cDir )                                           -> nInt
   METHOD  login                         // ( cUser, cPassword )                               -> nInt
   METHOD  mkdir                         // ( cDir )                                           -> nInt
   METHOD  put                           // ( oQIODevice, cFile, nType )                       -> nInt
                                         // ( oQByteArray, cFile, nType )                      -> nInt
   METHOD  rawCommand                    // ( cCommand )                                       -> nInt
   METHOD  readAll                       // (  )                                               -> oQByteArray
   METHOD  remove                        // ( cFile )                                          -> nInt
   METHOD  rename                        // ( cOldname, cNewname )                             -> nInt
   METHOD  rmdir                         // ( cDir )                                           -> nInt
   METHOD  setProxy                      // ( cHost, nPort )                                   -> nInt
   METHOD  setTransferMode               // ( nMode )                                          -> nInt
   METHOD  state                         // (  )                                               -> nState
   METHOD  abort                         // (  )                                               -> NIL

   ENDCLASS


METHOD QFtp:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFtp( ... )
   RETURN Self


METHOD QFtp:bytesAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_bytesAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:cd( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_cd( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:clearPendingCommands( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_clearPendingCommands( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:close( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_close( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:connectToHost( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFtp_connectToHost( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_connectToHost( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:currentCommand( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_currentCommand( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:currentDevice( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIODeviceFromPointer( Qt_QFtp_currentDevice( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:currentId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_currentId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:error( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_error( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:errorString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_errorString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:get( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFtp_get( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QFtp_get( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_get( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:hasPendingCommands( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_hasPendingCommands( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:list( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_list( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFtp_list( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:login( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFtp_login( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_login( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFtp_login( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:mkdir( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_mkdir( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:put( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QIODEVICE"
            RETURN Qt_QFtp_put( ::pPtr, ... )
         CASE "QBYTEARRAY"
            RETURN Qt_QFtp_put_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QIODEVICE"
            RETURN Qt_QFtp_put( ::pPtr, ... )
         CASE "QBYTEARRAY"
            RETURN Qt_QFtp_put_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:rawCommand( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_rawCommand( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:readAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QFtp_readAll( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:remove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_remove( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:rename( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFtp_rename( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:rmdir( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_rmdir( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:setProxy( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFtp_setProxy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:setTransferMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFtp_setTransferMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:state( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_state( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFtp:abort( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFtp_abort( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

