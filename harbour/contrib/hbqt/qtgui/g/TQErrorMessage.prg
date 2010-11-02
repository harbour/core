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


FUNCTION QErrorMessage( ... )
   RETURN HB_QErrorMessage():new( ... )

FUNCTION QErrorMessageFromPointer( ... )
   RETURN HB_QErrorMessage():fromPointer( ... )


CREATE CLASS QErrorMessage INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QErrorMessage

   METHOD  new( ... )

   METHOD  showMessage                   // ( cMessage )                                       -> NIL
                                         // ( cMessage, cType )                                -> NIL

   ENDCLASS


METHOD QErrorMessage:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QErrorMessage( ... )
   RETURN Self


METHOD QErrorMessage:showMessage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QErrorMessage_showMessage_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QErrorMessage_showMessage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

