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


FUNCTION QTextDocumentWriter( ... )
   RETURN HB_QTextDocumentWriter():new( ... )

FUNCTION QTextDocumentWriterFromPointer( ... )
   RETURN HB_QTextDocumentWriter():fromPointer( ... )


CREATE CLASS QTextDocumentWriter INHERIT HbQtObjectHandler FUNCTION HB_QTextDocumentWriter

   METHOD  new( ... )

   METHOD  codec                         // (  )                                               -> oQTextCodec
   METHOD  device                        // (  )                                               -> oQIODevice
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  format                        // (  )                                               -> oQByteArray
   METHOD  setCodec                      // ( oQTextCodec )                                    -> NIL
   METHOD  setDevice                     // ( oQIODevice )                                     -> NIL
   METHOD  setFileName                   // ( cFileName )                                      -> NIL
   METHOD  setFormat                     // ( oQByteArray )                                    -> NIL
   METHOD  write                         // ( oQTextDocument )                                 -> lBool
                                         // ( oQTextDocumentFragment )                         -> lBool

   ENDCLASS


METHOD QTextDocumentWriter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextDocumentWriter( ... )
   RETURN Self


METHOD QTextDocumentWriter:codec( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCodecFromPointer( Qt_QTextDocumentWriter_codec( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentWriter:device( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIODeviceFromPointer( Qt_QTextDocumentWriter_device( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentWriter:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextDocumentWriter_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentWriter:format( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QTextDocumentWriter_format( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentWriter:setCodec( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextDocumentWriter_setCodec( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentWriter:setDevice( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextDocumentWriter_setDevice( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentWriter:setFileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextDocumentWriter_setFileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentWriter:setFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextDocumentWriter_setFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentWriter:write( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTEXTDOCUMENT"
            RETURN Qt_QTextDocumentWriter_write( ::pPtr, ... )
         CASE "QTEXTDOCUMENTFRAGMENT"
            RETURN Qt_QTextDocumentWriter_write_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

