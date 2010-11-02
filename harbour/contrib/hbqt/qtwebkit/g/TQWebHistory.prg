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


REQUEST __HBQTWEBKIT


FUNCTION QWebHistory( ... )
   RETURN HB_QWebHistory():new( ... )

FUNCTION QWebHistoryFromPointer( ... )
   RETURN HB_QWebHistory():fromPointer( ... )


CREATE CLASS QWebHistory INHERIT HbQtObjectHandler FUNCTION HB_QWebHistory

   METHOD  new( ... )

   METHOD  back                          // (  )                                               -> NIL
   METHOD  backItem                      // (  )                                               -> oQWebHistoryItem
   METHOD  backItems                     // ( nMaxItems )                                      -> oQList_QWebHistoryItem>
   METHOD  canGoBack                     // (  )                                               -> lBool
   METHOD  canGoForward                  // (  )                                               -> lBool
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  count                         // (  )                                               -> nInt
   METHOD  currentItem                   // (  )                                               -> oQWebHistoryItem
   METHOD  currentItemIndex              // (  )                                               -> nInt
   METHOD  forward                       // (  )                                               -> NIL
   METHOD  forwardItem                   // (  )                                               -> oQWebHistoryItem
   METHOD  forwardItems                  // ( nMaxItems )                                      -> oQList_QWebHistoryItem>
   METHOD  goToItem                      // ( oQWebHistoryItem )                               -> NIL
   METHOD  itemAt                        // ( nI )                                             -> oQWebHistoryItem
   METHOD  items                         // (  )                                               -> oQList_QWebHistoryItem>
   METHOD  maximumItemCount              // (  )                                               -> nInt
   METHOD  setMaximumItemCount           // ( nCount )                                         -> NIL

   ENDCLASS


METHOD QWebHistory:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebHistory( ... )
   RETURN Self


METHOD QWebHistory:back( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHistory_back( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:backItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebHistoryItemFromPointer( Qt_QWebHistory_backItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:backItems( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QWebHistory_backItems( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:canGoBack( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHistory_canGoBack( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:canGoForward( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHistory_canGoForward( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHistory_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHistory_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:currentItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebHistoryItemFromPointer( Qt_QWebHistory_currentItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:currentItemIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHistory_currentItemIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:forward( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHistory_forward( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:forwardItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWebHistoryItemFromPointer( Qt_QWebHistory_forwardItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:forwardItems( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QWebHistory_forwardItems( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:goToItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWebHistory_goToItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:itemAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QWebHistoryItemFromPointer( Qt_QWebHistory_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:items( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QWebHistory_items( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:maximumItemCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebHistory_maximumItemCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebHistory:setMaximumItemCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebHistory_setMaximumItemCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

