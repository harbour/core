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


FUNCTION QScrollArea( ... )
   RETURN HB_QScrollArea():new( ... )

FUNCTION QScrollAreaFromPointer( ... )
   RETURN HB_QScrollArea():fromPointer( ... )


CREATE CLASS QScrollArea INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QScrollArea

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  ensureVisible                 // ( nX, nY, nXmargin, nYmargin )                     -> NIL
   METHOD  ensureWidgetVisible           // ( oQWidget, nXmargin, nYmargin )                   -> NIL
   METHOD  setAlignment                  // ( nQt::Alignment )                                 -> NIL
   METHOD  setWidget                     // ( oQWidget )                                       -> NIL
   METHOD  setWidgetResizable            // ( lResizable )                                     -> NIL
   METHOD  takeWidget                    // (  )                                               -> oQWidget
   METHOD  widget                        // (  )                                               -> oQWidget
   METHOD  widgetResizable               // (  )                                               -> lBool

   ENDCLASS


METHOD QScrollArea:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QScrollArea( ... )
   RETURN Self


METHOD QScrollArea:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QScrollArea_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QScrollArea:ensureVisible( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QScrollArea_ensureVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QScrollArea_ensureVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QScrollArea_ensureVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QScrollArea:ensureWidgetVisible( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QScrollArea_ensureWidgetVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QScrollArea_ensureWidgetVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QScrollArea_ensureWidgetVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QScrollArea:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QScrollArea_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QScrollArea:setWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QScrollArea_setWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QScrollArea:setWidgetResizable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QScrollArea_setWidgetResizable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QScrollArea:takeWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QScrollArea_takeWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QScrollArea:widget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QScrollArea_widget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QScrollArea:widgetResizable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QScrollArea_widgetResizable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

