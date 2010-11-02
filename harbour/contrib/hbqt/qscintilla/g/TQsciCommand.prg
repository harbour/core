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


FUNCTION QsciCommand( ... )
   RETURN HB_QsciCommand():new( ... )

FUNCTION QsciCommandFromPointer( ... )
   RETURN HB_QsciCommand():fromPointer( ... )


CREATE CLASS QsciCommand INHERIT HbQtObjectHandler FUNCTION HB_QsciCommand

   METHOD  new( ... )

   METHOD  setKey                        // ( nKey )                                           -> NIL
   METHOD  setAlternateKey               // ( nAltkey )                                        -> NIL
   METHOD  key                           // (  )                                               -> nInt
   METHOD  alternateKey                  // (  )                                               -> nInt
   METHOD  description                   // (  )                                               -> cQString
   METHOD  validKey                      // ( nKey )                                           -> lBool

   ENDCLASS


METHOD QsciCommand:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciCommand( ... )
   RETURN Self


METHOD QsciCommand:setKey( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciCommand_setKey( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciCommand:setAlternateKey( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciCommand_setAlternateKey( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciCommand:key( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciCommand_key( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciCommand:alternateKey( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciCommand_alternateKey( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciCommand:description( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciCommand_description( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciCommand:validKey( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciCommand_validKey( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

