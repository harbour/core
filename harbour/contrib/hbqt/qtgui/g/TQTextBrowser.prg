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


FUNCTION QTextBrowser( ... )
   RETURN HB_QTextBrowser():new( ... )

FUNCTION QTextBrowserFromPointer( ... )
   RETURN HB_QTextBrowser():fromPointer( ... )


CREATE CLASS QTextBrowser INHERIT HbQtObjectHandler, HB_QTextEdit FUNCTION HB_QTextBrowser

   METHOD  new( ... )

   METHOD  backwardHistoryCount          // (  )                                               -> nInt
   METHOD  clearHistory                  // (  )                                               -> NIL
   METHOD  forwardHistoryCount           // (  )                                               -> nInt
   METHOD  historyTitle                  // ( nI )                                             -> cQString
   METHOD  historyUrl                    // ( nI )                                             -> oQUrl
   METHOD  isBackwardAvailable           // (  )                                               -> lBool
   METHOD  isForwardAvailable            // (  )                                               -> lBool
   METHOD  loadResource                  // ( nType, oQUrl )                                   -> oQVariant
   METHOD  openExternalLinks             // (  )                                               -> lBool
   METHOD  openLinks                     // (  )                                               -> lBool
   METHOD  searchPaths                   // (  )                                               -> oQStringList
   METHOD  setOpenExternalLinks          // ( lOpen )                                          -> NIL
   METHOD  setOpenLinks                  // ( lOpen )                                          -> NIL
   METHOD  setSearchPaths                // ( oQStringList )                                   -> NIL
   METHOD  source                        // (  )                                               -> oQUrl
   METHOD  backward                      // (  )                                               -> NIL
   METHOD  forward                       // (  )                                               -> NIL
   METHOD  home                          // (  )                                               -> NIL
   METHOD  reload                        // (  )                                               -> NIL
   METHOD  setSource                     // ( oQUrl )                                          -> NIL

   ENDCLASS


METHOD QTextBrowser:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextBrowser( ... )
   RETURN Self


METHOD QTextBrowser:backwardHistoryCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_backwardHistoryCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:clearHistory( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_clearHistory( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:forwardHistoryCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_forwardHistoryCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:historyTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextBrowser_historyTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:historyUrl( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QUrlFromPointer( Qt_QTextBrowser_historyUrl( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:isBackwardAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_isBackwardAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:isForwardAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_isForwardAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:loadResource( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QTextBrowser_loadResource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:openExternalLinks( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_openExternalLinks( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:openLinks( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_openLinks( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:searchPaths( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QTextBrowser_searchPaths( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:setOpenExternalLinks( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextBrowser_setOpenExternalLinks( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:setOpenLinks( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextBrowser_setOpenLinks( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:setSearchPaths( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextBrowser_setSearchPaths( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:source( ... )
   SWITCH PCount()
   CASE 0
      RETURN QUrlFromPointer( Qt_QTextBrowser_source( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:backward( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_backward( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:forward( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_forward( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:home( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_home( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:reload( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextBrowser_reload( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextBrowser:setSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextBrowser_setSource( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

