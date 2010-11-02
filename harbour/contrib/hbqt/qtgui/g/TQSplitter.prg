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


FUNCTION QSplitter( ... )
   RETURN HB_QSplitter():new( ... )

FUNCTION QSplitterFromPointer( ... )
   RETURN HB_QSplitter():fromPointer( ... )


CREATE CLASS QSplitter INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QSplitter

   METHOD  new( ... )

   METHOD  addWidget                     // ( oQWidget )                                       -> NIL
   METHOD  childrenCollapsible           // (  )                                               -> lBool
   METHOD  count                         // (  )                                               -> nInt
   METHOD  getRange                      // ( nIndex, @nMin, @nMax )                           -> NIL
   METHOD  handleWidth                   // (  )                                               -> nInt
   METHOD  indexOf                       // ( oQWidget )                                       -> nInt
   METHOD  insertWidget                  // ( nIndex, oQWidget )                               -> NIL
   METHOD  isCollapsible                 // ( nIndex )                                         -> lBool
   METHOD  opaqueResize                  // (  )                                               -> lBool
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  refresh                       // (  )                                               -> NIL
   METHOD  restoreState                  // ( oQByteArray )                                    -> lBool
   METHOD  saveState                     // (  )                                               -> oQByteArray
   METHOD  setChildrenCollapsible        // ( lBool )                                          -> NIL
   METHOD  setCollapsible                // ( nIndex, lCollapse )                              -> NIL
   METHOD  setHandleWidth                // ( nInt )                                           -> NIL
   METHOD  setOpaqueResize               // ( lOpaque )                                        -> NIL
   METHOD  setOrientation                // ( nQt::Orientation )                               -> NIL
   METHOD  setStretchFactor              // ( nIndex, nStretch )                               -> NIL
   METHOD  sizes                         // (  )                                               -> oQList_int>
   METHOD  widget                        // ( nIndex )                                         -> oQWidget

   ENDCLASS


METHOD QSplitter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSplitter( ... )
   RETURN Self


METHOD QSplitter:addWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSplitter_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:childrenCollapsible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSplitter_childrenCollapsible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSplitter_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:getRange( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QSplitter_getRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:handleWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSplitter_handleWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:indexOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSplitter_indexOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:insertWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QSplitter_insertWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:isCollapsible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSplitter_isCollapsible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:opaqueResize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSplitter_opaqueResize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSplitter_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:refresh( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSplitter_refresh( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:restoreState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSplitter_restoreState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:saveState( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QSplitter_saveState( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:setChildrenCollapsible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QSplitter_setChildrenCollapsible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:setCollapsible( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QSplitter_setCollapsible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:setHandleWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSplitter_setHandleWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:setOpaqueResize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QSplitter_setOpaqueResize( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QSplitter_setOpaqueResize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:setOrientation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSplitter_setOrientation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:setStretchFactor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QSplitter_setStretchFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:sizes( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QSplitter_sizes( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSplitter:widget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QSplitter_widget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

