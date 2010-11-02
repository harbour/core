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


FUNCTION QKeySequence( ... )
   RETURN HB_QKeySequence():new( ... )

FUNCTION QKeySequenceFromPointer( ... )
   RETURN HB_QKeySequence():fromPointer( ... )


CREATE CLASS QKeySequence INHERIT HbQtObjectHandler FUNCTION HB_QKeySequence

   METHOD  new( ... )

   METHOD  count                         // (  )                                               -> nUint
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  matches                       // ( oQKeySequence )                                  -> nSequenceMatch
   METHOD  toString                      // ( nFormat )                                        -> cQString
   METHOD  fromString                    // ( cStr, nFormat )                                  -> oQKeySequence
   METHOD  keyBindings                   // ( nKey )                                           -> oQList_QKeySequence>
   METHOD  mnemonic                      // ( cText )                                          -> oQKeySequence

   ENDCLASS


METHOD QKeySequence:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QKeySequence( ... )
   RETURN Self


METHOD QKeySequence:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QKeySequence_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeySequence:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QKeySequence_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeySequence:matches( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QKeySequence_matches( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeySequence:toString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QKeySequence_toString( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QKeySequence_toString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeySequence:fromString( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QKeySequenceFromPointer( Qt_QKeySequence_fromString( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QKeySequenceFromPointer( Qt_QKeySequence_fromString( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeySequence:keyBindings( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QKeySequence_keyBindings( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QKeySequence:mnemonic( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QKeySequenceFromPointer( Qt_QKeySequence_mnemonic( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

