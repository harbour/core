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


FUNCTION QLatin1String( ... )
   RETURN HB_QLatin1String():new( ... )

FUNCTION QLatin1StringFromPointer( ... )
   RETURN HB_QLatin1String():fromPointer( ... )


CREATE CLASS QLatin1String INHERIT HbQtObjectHandler FUNCTION HB_QLatin1String

   METHOD  new( ... )

   METHOD  latin1                        // (  )                                               -> cChar

   ENDCLASS


METHOD QLatin1String:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLatin1String( ... )
   RETURN Self


METHOD QLatin1String:latin1( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLatin1String_latin1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

