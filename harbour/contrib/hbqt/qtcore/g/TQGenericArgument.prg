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


FUNCTION QGenericArgument( ... )
   RETURN HB_QGenericArgument():new( ... )

FUNCTION QGenericArgumentFromPointer( ... )
   RETURN HB_QGenericArgument():fromPointer( ... )


CREATE CLASS QGenericArgument INHERIT HbQtObjectHandler FUNCTION HB_QGenericArgument

   METHOD  new( ... )

   METHOD  data                          // (  )                                               -> NIL
   METHOD  name                          // (  )                                               -> cChar

   ENDCLASS


METHOD QGenericArgument:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGenericArgument( ... )
   RETURN Self


METHOD QGenericArgument:data( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGenericArgument_data( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGenericArgument:name( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGenericArgument_name( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

