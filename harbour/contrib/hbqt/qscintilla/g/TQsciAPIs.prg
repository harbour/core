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


REQUEST __HBQSCINTILLA


FUNCTION QsciAPIs( ... )
   RETURN HB_QsciAPIs():new( ... )

FUNCTION QsciAPIsFromPointer( ... )
   RETURN HB_QsciAPIs():fromPointer( ... )


CREATE CLASS QsciAPIs INHERIT HbQtObjectHandler, HB_QsciAbstractAPIs FUNCTION HB_QsciAPIs

   METHOD  new( ... )

   METHOD  add                           // ( cEntry )                                         -> NIL
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  load                          // ( cFname )                                         -> lBool
   METHOD  remove                        // ( cEntry )                                         -> NIL
   METHOD  prepare                       // (  )                                               -> NIL
   METHOD  cancelPreparation             // (  )                                               -> NIL
   METHOD  defaultPreparedName           // (  )                                               -> cQString
   METHOD  isPrepared                    // ( cFname )                                         -> lBool
   METHOD  loadPrepared                  // ( cFname )                                         -> lBool
   METHOD  savePrepared                  // ( cFname )                                         -> lBool
   METHOD  updateAutoCompletionList      // ( oQStringList, oQStringList )                     -> NIL
   METHOD  autoCompletionSelected        // ( cSel )                                           -> NIL
   METHOD  event                         // ( oQEvent )                                        -> lBool
   METHOD  installedAPIFiles             // (  )                                               -> oQStringList

   ENDCLASS


METHOD QsciAPIs:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciAPIs( ... )
   RETURN Self


METHOD QsciAPIs:add( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciAPIs_add( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciAPIs_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:load( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciAPIs_load( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:remove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciAPIs_remove( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:prepare( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciAPIs_prepare( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:cancelPreparation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciAPIs_cancelPreparation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:defaultPreparedName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciAPIs_defaultPreparedName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:isPrepared( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciAPIs_isPrepared( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciAPIs_isPrepared( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:loadPrepared( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciAPIs_loadPrepared( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciAPIs_loadPrepared( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:savePrepared( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciAPIs_savePrepared( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciAPIs_savePrepared( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:updateAutoCompletionList( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QsciAPIs_updateAutoCompletionList( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:autoCompletionSelected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciAPIs_autoCompletionSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:event( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciAPIs_event( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAPIs:installedAPIFiles( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QsciAPIs_installedAPIFiles( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

