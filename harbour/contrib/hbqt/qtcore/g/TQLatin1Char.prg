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


FUNCTION QLatin1Char( ... )
   RETURN HB_QLatin1Char():new( ... )

FUNCTION QLatin1CharFromPointer( ... )
   RETURN HB_QLatin1Char():fromPointer( ... )


CREATE CLASS QLatin1Char INHERIT HbQtObjectHandler FUNCTION HB_QLatin1Char

   METHOD  new( ... )

   METHOD  toLatin1                      // (  )                                               -> cChar
   METHOD  unicode                       // (  )                                               -> nUshort

   ENDCLASS


METHOD QLatin1Char:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLatin1Char( ... )
   RETURN Self


METHOD QLatin1Char:toLatin1( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLatin1Char_toLatin1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QLatin1Char:unicode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLatin1Char_unicode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

