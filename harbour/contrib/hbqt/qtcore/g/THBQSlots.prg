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


FUNCTION HBQSlots( ... )
   RETURN HB_HBQSlots():new( ... )

FUNCTION HBQSlotsFromPointer( ... )
   RETURN HB_HBQSlots():fromPointer( ... )


CREATE CLASS HBQSlots INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_HBQSlots

   METHOD  new( ... )


   ENDCLASS


METHOD HBQSlots:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQSlots( ... )
   RETURN Self

