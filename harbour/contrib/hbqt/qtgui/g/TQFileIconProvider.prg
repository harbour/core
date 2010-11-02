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


FUNCTION QFileIconProvider( ... )
   RETURN HB_QFileIconProvider():new( ... )

FUNCTION QFileIconProviderFromPointer( ... )
   RETURN HB_QFileIconProvider():fromPointer( ... )


CREATE CLASS QFileIconProvider INHERIT HbQtObjectHandler FUNCTION HB_QFileIconProvider

   METHOD  new( ... )

   METHOD  icon                          // ( nType )                                          -> oQIcon
                                         // ( oQFileInfo )                                     -> oQIcon
   METHOD  type                          // ( oQFileInfo )                                     -> cQString

   ENDCLASS


METHOD QFileIconProvider:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFileIconProvider( ... )
   RETURN Self


METHOD QFileIconProvider:icon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QFileIconProvider_icon( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QIconFromPointer( Qt_QFileIconProvider_icon_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileIconProvider:type( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileIconProvider_type( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

