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


REQUEST __HBQTCORE


FUNCTION HBQEvents( ... )
   RETURN HB_HBQEvents():new( ... )

FUNCTION HBQEventsFromPointer( ... )
   RETURN HB_HBQEvents():fromPointer( ... )


CREATE CLASS HBQEvents INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_HBQEvents

   METHOD  new( ... )

   METHOD  hbConnect                     // ( xObj, nEvent, xBlock )                           -> lBool
   METHOD  hbDisconnect                  // ( xObj, nEvent )                                   -> lBool
   METHOD  hbClear                       // (  )                                               -> lBool

   ENDCLASS


METHOD HBQEvents:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQEvents( ... )
   RETURN Self


METHOD HBQEvents:hbConnect( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE (  hb_pvalue( 1 ) != NIL ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. (  hb_pvalue( 3 ) != NIL )
         RETURN Qt_HBQEvents_hbConnect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQEvents:hbDisconnect( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE (  hb_pvalue( 1 ) != NIL ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_HBQEvents_hbDisconnect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQEvents:hbClear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQEvents_hbClear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

