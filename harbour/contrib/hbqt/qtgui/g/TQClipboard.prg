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


FUNCTION QClipboard( ... )
   RETURN HB_QClipboard():new( ... )

FUNCTION QClipboardFromPointer( ... )
   RETURN HB_QClipboard():fromPointer( ... )


CREATE CLASS QClipboard INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QClipboard

   METHOD  new( ... )

   METHOD  clear                         // ( nMode )                                          -> NIL
   METHOD  image                         // ( nMode )                                          -> oQImage
   METHOD  ownsClipboard                 // (  )                                               -> lBool
   METHOD  ownsFindBuffer                // (  )                                               -> lBool
   METHOD  ownsSelection                 // (  )                                               -> lBool
   METHOD  pixmap                        // ( nMode )                                          -> oQPixmap
   METHOD  setImage                      // ( oQImage, nMode )                                 -> NIL
   METHOD  setMimeData                   // ( oQMimeData, nMode )                              -> NIL
   METHOD  setPixmap                     // ( oQPixmap, nMode )                                -> NIL
   METHOD  setText                       // ( cText, nMode )                                   -> NIL
   METHOD  supportsFindBuffer            // (  )                                               -> lBool
   METHOD  supportsSelection             // (  )                                               -> lBool
   METHOD  text                          // ( nMode )                                          -> cQString

   ENDCLASS


METHOD QClipboard:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QClipboard( ... )
   RETURN Self


METHOD QClipboard:clear( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QClipboard_clear( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QClipboard_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:image( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QImageFromPointer( Qt_QClipboard_image( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QImageFromPointer( Qt_QClipboard_image( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:ownsClipboard( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QClipboard_ownsClipboard( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:ownsFindBuffer( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QClipboard_ownsFindBuffer( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:ownsSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QClipboard_ownsSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:pixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPixmapFromPointer( Qt_QClipboard_pixmap( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QPixmapFromPointer( Qt_QClipboard_pixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:setImage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QClipboard_setImage( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QClipboard_setImage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:setMimeData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QClipboard_setMimeData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QClipboard_setMimeData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:setPixmap( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QClipboard_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QClipboard_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:setText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QClipboard_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QClipboard_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:supportsFindBuffer( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QClipboard_supportsFindBuffer( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:supportsSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QClipboard_supportsSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QClipboard:text( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QClipboard_text( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QClipboard_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

