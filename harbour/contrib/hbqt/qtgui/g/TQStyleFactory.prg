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


FUNCTION QStyleFactory( ... )
   RETURN HB_QStyleFactory():new( ... )

FUNCTION QStyleFactoryFromPointer( ... )
   RETURN HB_QStyleFactory():fromPointer( ... )


CREATE CLASS QStyleFactory INHERIT HbQtObjectHandler FUNCTION HB_QStyleFactory

   METHOD  new( ... )

   METHOD  create                        // ( cKey )                                           -> oQStyle
   METHOD  keys                          // (  )                                               -> oQStringList

   ENDCLASS


METHOD QStyleFactory:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleFactory( ... )
   RETURN Self


METHOD QStyleFactory:create( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QStyleFromPointer( Qt_QStyleFactory_create( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStyleFactory:keys( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QStyleFactory_keys( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

