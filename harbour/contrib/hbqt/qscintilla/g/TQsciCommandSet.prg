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


FUNCTION QsciCommandSet( ... )
   RETURN HB_QsciCommandSet():new( ... )

FUNCTION QsciCommandSetFromPointer( ... )
   RETURN HB_QsciCommandSet():fromPointer( ... )


CREATE CLASS QsciCommandSet INHERIT HbQtObjectHandler FUNCTION HB_QsciCommandSet

   METHOD  new( ... )

   METHOD  readSettings                  // ( oQSettings, cPrefix )                            -> lBool
   METHOD  writeSettings                 // ( oQSettings, cPrefix )                            -> lBool
   METHOD  commands                      // (  )                                               -> oQList_ QsciCommand * > &
   METHOD  clearKeys                     // (  )                                               -> NIL
   METHOD  clearAlternateKeys            // (  )                                               -> NIL

   ENDCLASS


METHOD QsciCommandSet:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciCommandSet( ... )
   RETURN Self


METHOD QsciCommandSet:readSettings( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QsciCommandSet_readSettings( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciCommandSet_readSettings( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciCommandSet:writeSettings( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QsciCommandSet_writeSettings( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QsciCommandSet_writeSettings( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciCommandSet:commands( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QsciCommandSet_commands( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciCommandSet:clearKeys( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciCommandSet_clearKeys( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciCommandSet:clearAlternateKeys( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciCommandSet_clearAlternateKeys( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

