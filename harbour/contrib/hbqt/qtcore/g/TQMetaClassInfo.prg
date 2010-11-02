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


FUNCTION QMetaClassInfo( ... )
   RETURN HB_QMetaClassInfo():new( ... )

FUNCTION QMetaClassInfoFromPointer( ... )
   RETURN HB_QMetaClassInfo():fromPointer( ... )


CREATE CLASS QMetaClassInfo INHERIT HbQtObjectHandler FUNCTION HB_QMetaClassInfo

   METHOD  new( ... )

   METHOD  name                          // (  )                                               -> cChar
   METHOD  value                         // (  )                                               -> cChar

   ENDCLASS


METHOD QMetaClassInfo:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMetaClassInfo( ... )
   RETURN Self


METHOD QMetaClassInfo:name( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaClassInfo_name( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMetaClassInfo:value( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMetaClassInfo_value( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

