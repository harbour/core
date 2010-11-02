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


FUNCTION QGenericReturnArgument( ... )
   RETURN HB_QGenericReturnArgument():new( ... )

FUNCTION QGenericReturnArgumentFromPointer( ... )
   RETURN HB_QGenericReturnArgument():fromPointer( ... )


CREATE CLASS QGenericReturnArgument INHERIT HbQtObjectHandler, HB_QGenericArgument FUNCTION HB_QGenericReturnArgument

   METHOD  new( ... )


   ENDCLASS


METHOD QGenericReturnArgument:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGenericReturnArgument( ... )
   RETURN Self

