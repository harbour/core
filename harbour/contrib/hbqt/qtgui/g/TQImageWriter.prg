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


FUNCTION QImageWriter( ... )
   RETURN HB_QImageWriter():new( ... )

FUNCTION QImageWriterFromPointer( ... )
   RETURN HB_QImageWriter():fromPointer( ... )


CREATE CLASS QImageWriter INHERIT HbQtObjectHandler FUNCTION HB_QImageWriter

   METHOD  new( ... )

   METHOD  canWrite                      // (  )                                               -> lBool
   METHOD  compression                   // (  )                                               -> nInt
   METHOD  device                        // (  )                                               -> oQIODevice
   METHOD  error                         // (  )                                               -> nImageWriterError
   METHOD  errorString                   // (  )                                               -> cQString
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  format                        // (  )                                               -> oQByteArray
   METHOD  gamma                         // (  )                                               -> nFloat
   METHOD  quality                       // (  )                                               -> nInt
   METHOD  setCompression                // ( nCompression )                                   -> NIL
   METHOD  setDevice                     // ( oQIODevice )                                     -> NIL
   METHOD  setFileName                   // ( cFileName )                                      -> NIL
   METHOD  setFormat                     // ( oQByteArray )                                    -> NIL
   METHOD  setGamma                      // ( nGamma )                                         -> NIL
   METHOD  setQuality                    // ( nQuality )                                       -> NIL
   METHOD  setText                       // ( cKey, cText )                                    -> NIL
   METHOD  supportsOption                // ( nOption )                                        -> lBool
   METHOD  write                         // ( oQImage )                                        -> lBool
   METHOD  supportedImageFormats         // (  )                                               -> oQList_QByteArray>

   ENDCLASS


METHOD QImageWriter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QImageWriter( ... )
   RETURN Self


METHOD QImageWriter:canWrite( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageWriter_canWrite( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:compression( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageWriter_compression( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:device( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIODeviceFromPointer( Qt_QImageWriter_device( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:error( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageWriter_error( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:errorString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageWriter_errorString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageWriter_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:format( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QImageWriter_format( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:gamma( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageWriter_gamma( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:quality( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageWriter_quality( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:setCompression( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImageWriter_setCompression( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:setDevice( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImageWriter_setDevice( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:setFileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QImageWriter_setFileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:setFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImageWriter_setFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:setGamma( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImageWriter_setGamma( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:setQuality( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImageWriter_setQuality( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:setText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QImageWriter_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:supportsOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImageWriter_supportsOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:write( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImageWriter_write( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageWriter:supportedImageFormats( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QImageWriter_supportedImageFormats( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

