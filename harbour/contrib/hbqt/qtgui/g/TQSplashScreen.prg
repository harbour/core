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


FUNCTION QSplashScreen( ... )
   RETURN HB_QSplashScreen():new( ... )

FUNCTION QSplashScreenFromPointer( ... )
   RETURN HB_QSplashScreen():fromPointer( ... )


CREATE CLASS QSplashScreen INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QSplashScreen

   METHOD  new( ... )

   METHOD  finish                        // ( oQWidget )                                       -> NIL
   METHOD  pixmap                        // (  )                                               -> oQPixmap
   METHOD  repaint                       // (  )                                               -> NIL
   METHOD  setPixmap                     // ( oQPixmap )                                       -> NIL
   METHOD  clearMessage                  // (  )                                               -> NIL
   METHOD  showMessage                   // ( cMessage, nAlignment, oQColor )                  -> NIL

   ENDCLASS


METHOD QSplashScreen:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSplashScreen( ... )
   RETURN Self


METHOD QSplashScreen:finish( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSplashScreen_finish( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplashScreen:pixmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPixmapFromPointer( Qt_QSplashScreen_pixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplashScreen:repaint( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSplashScreen_repaint( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplashScreen:setPixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSplashScreen_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplashScreen:clearMessage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSplashScreen_clearMessage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplashScreen:showMessage( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QSplashScreen_showMessage( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QSplashScreen_showMessage( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSplashScreen_showMessage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

