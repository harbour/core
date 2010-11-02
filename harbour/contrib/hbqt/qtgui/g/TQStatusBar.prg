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


FUNCTION QStatusBar( ... )
   RETURN HB_QStatusBar():new( ... )

FUNCTION QStatusBarFromPointer( ... )
   RETURN HB_QStatusBar():fromPointer( ... )


CREATE CLASS QStatusBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QStatusBar

   METHOD  new( ... )

   METHOD  addPermanentWidget            // ( oQWidget, nStretch )                             -> NIL
   METHOD  addWidget                     // ( oQWidget, nStretch )                             -> NIL
   METHOD  currentMessage                // (  )                                               -> cQString
   METHOD  insertPermanentWidget         // ( nIndex, oQWidget, nStretch )                     -> nInt
   METHOD  insertWidget                  // ( nIndex, oQWidget, nStretch )                     -> nInt
   METHOD  isSizeGripEnabled             // (  )                                               -> lBool
   METHOD  removeWidget                  // ( oQWidget )                                       -> NIL
   METHOD  setSizeGripEnabled            // ( lBool )                                          -> NIL
   METHOD  clearMessage                  // (  )                                               -> NIL
   METHOD  showMessage                   // ( cMessage, nTimeout )                             -> NIL

   ENDCLASS


METHOD QStatusBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStatusBar( ... )
   RETURN Self


METHOD QStatusBar:addPermanentWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStatusBar_addPermanentWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStatusBar_addPermanentWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStatusBar:addWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStatusBar_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStatusBar_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStatusBar:currentMessage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStatusBar_currentMessage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStatusBar:insertPermanentWidget( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QStatusBar_insertPermanentWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStatusBar_insertPermanentWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStatusBar:insertWidget( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QStatusBar_insertWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStatusBar_insertWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStatusBar:isSizeGripEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStatusBar_isSizeGripEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStatusBar:removeWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStatusBar_removeWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStatusBar:setSizeGripEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QStatusBar_setSizeGripEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStatusBar:clearMessage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStatusBar_clearMessage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStatusBar:showMessage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStatusBar_showMessage( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStatusBar_showMessage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

