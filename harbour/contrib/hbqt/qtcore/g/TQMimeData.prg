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


FUNCTION QMimeData( ... )
   RETURN HB_QMimeData():new( ... )

FUNCTION QMimeDataFromPointer( ... )
   RETURN HB_QMimeData():fromPointer( ... )


CREATE CLASS QMimeData INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QMimeData

   METHOD  new( ... )

   METHOD  clear                         // (  )                                               -> NIL
   METHOD  colorData                     // (  )                                               -> oQVariant
   METHOD  data                          // ( cMimeType )                                      -> oQByteArray
   METHOD  formats                       // (  )                                               -> oQStringList
   METHOD  hasColor                      // (  )                                               -> lBool
   METHOD  hasFormat                     // ( cMimeType )                                      -> lBool
   METHOD  hasHtml                       // (  )                                               -> lBool
   METHOD  hasImage                      // (  )                                               -> lBool
   METHOD  hasText                       // (  )                                               -> lBool
   METHOD  hasUrls                       // (  )                                               -> lBool
   METHOD  html                          // (  )                                               -> cQString
   METHOD  imageData                     // (  )                                               -> oQVariant
   METHOD  removeFormat                  // ( cMimeType )                                      -> NIL
   METHOD  setColorData                  // ( oQVariant )                                      -> NIL
   METHOD  setData                       // ( cMimeType, oQByteArray )                         -> NIL
   METHOD  setHtml                       // ( cHtml )                                          -> NIL
   METHOD  setImageData                  // ( oQVariant )                                      -> NIL
   METHOD  setText                       // ( cText )                                          -> NIL
   METHOD  text                          // (  )                                               -> cQString
   METHOD  urls                          // (  )                                               -> oQList_QUrl>
   METHOD  hbUrlList                     // (  )                                               -> oQStringList

   ENDCLASS


METHOD QMimeData:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMimeData( ... )
   RETURN Self


METHOD QMimeData:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMimeData_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:colorData( ... )
   SWITCH PCount()
   CASE 0
      RETURN QVariantFromPointer( Qt_QMimeData_colorData( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:data( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QMimeData_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:formats( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QMimeData_formats( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:hasColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMimeData_hasColor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:hasFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMimeData_hasFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:hasHtml( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMimeData_hasHtml( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:hasImage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMimeData_hasImage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:hasText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMimeData_hasText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:hasUrls( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMimeData_hasUrls( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:html( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMimeData_html( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:imageData( ... )
   SWITCH PCount()
   CASE 0
      RETURN QVariantFromPointer( Qt_QMimeData_imageData( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:removeFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMimeData_removeFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:setColorData( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMimeData_setColorData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:setData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QMimeData_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:setHtml( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMimeData_setHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:setImageData( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMimeData_setImageData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QMimeData_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMimeData_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:urls( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QMimeData_urls( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMimeData:hbUrlList( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QMimeData_hbUrlList( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

