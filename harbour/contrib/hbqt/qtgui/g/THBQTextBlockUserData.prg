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


FUNCTION HBQTextBlockUserData( ... )
   RETURN HB_HBQTextBlockUserData():new( ... )

FUNCTION HBQTextBlockUserDataFromPointer( ... )
   RETURN HB_HBQTextBlockUserData():fromPointer( ... )


CREATE CLASS HBQTextBlockUserData INHERIT HbQtObjectHandler FUNCTION HB_HBQTextBlockUserData

   METHOD  new( ... )

   METHOD  hbSetState                    // ( nState )                                         -> nInt
   METHOD  hbState                       // (  )                                               -> nInt

   ENDCLASS


METHOD HBQTextBlockUserData:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQTextBlockUserData( ... )
   RETURN Self


METHOD HBQTextBlockUserData:hbSetState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQTextBlockUserData_hbSetState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQTextBlockUserData:hbState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQTextBlockUserData_hbState( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

