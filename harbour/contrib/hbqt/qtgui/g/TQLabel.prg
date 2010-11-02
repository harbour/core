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


FUNCTION QLabel( ... )
   RETURN HB_QLabel():new( ... )

FUNCTION QLabelFromPointer( ... )
   RETURN HB_QLabel():fromPointer( ... )


CREATE CLASS QLabel INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QLabel

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  buddy                         // (  )                                               -> oQWidget
   METHOD  hasScaledContents             // (  )                                               -> lBool
   METHOD  indent                        // (  )                                               -> nInt
   METHOD  margin                        // (  )                                               -> nInt
   METHOD  movie                         // (  )                                               -> oQMovie
   METHOD  openExternalLinks             // (  )                                               -> lBool
   METHOD  picture                       // (  )                                               -> oQPicture
   METHOD  pixmap                        // (  )                                               -> oQPixmap
   METHOD  setAlignment                  // ( nQt::Alignment )                                 -> NIL
   METHOD  setBuddy                      // ( oQWidget )                                       -> NIL
   METHOD  setIndent                     // ( nInt )                                           -> NIL
   METHOD  setMargin                     // ( nInt )                                           -> NIL
   METHOD  setOpenExternalLinks          // ( lOpen )                                          -> NIL
   METHOD  setScaledContents             // ( lBool )                                          -> NIL
   METHOD  setTextFormat                 // ( nQt::TextFormat )                                -> NIL
   METHOD  setTextInteractionFlags       // ( nFlags )                                         -> NIL
   METHOD  setWordWrap                   // ( lOn )                                            -> NIL
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textFormat                    // (  )                                               -> nQt_TextFormat
   METHOD  textInteractionFlags          // (  )                                               -> nQt_TextInteractionFlags
   METHOD  wordWrap                      // (  )                                               -> lBool
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  setMovie                      // ( oQMovie )                                        -> NIL
   METHOD  setNum                        // ( nNum )                                           -> NIL
                                         // ( nNum )                                           -> NIL
   METHOD  setPicture                    // ( oQPicture )                                      -> NIL
   METHOD  setPixmap                     // ( oQPixmap )                                       -> NIL
   METHOD  setText                       // ( cQString )                                       -> NIL

   ENDCLASS


METHOD QLabel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLabel( ... )
   RETURN Self


METHOD QLabel:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:buddy( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QLabel_buddy( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:hasScaledContents( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_hasScaledContents( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:indent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_indent( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:margin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_margin( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:movie( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMovieFromPointer( Qt_QLabel_movie( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:openExternalLinks( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_openExternalLinks( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:picture( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPictureFromPointer( Qt_QLabel_picture( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:pixmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_QLabel_pixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setBuddy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setBuddy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setIndent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setIndent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setOpenExternalLinks( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setOpenExternalLinks( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setScaledContents( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setScaledContents( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setTextFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setTextFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setTextInteractionFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setTextInteractionFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setWordWrap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setWordWrap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:textFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_textFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:textInteractionFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_textInteractionFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:wordWrap( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_wordWrap( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setMovie( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setMovie( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setNum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setNum( ::pPtr, ... )
         // RETURN Qt_QLabel_setNum_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setPicture( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setPicture( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setPixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLabel:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

