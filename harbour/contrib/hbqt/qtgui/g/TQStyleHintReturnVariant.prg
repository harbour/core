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


FUNCTION QStyleHintReturnVariant( ... )
   RETURN HB_QStyleHintReturnVariant():new( ... )

FUNCTION QStyleHintReturnVariantFromPointer( ... )
   RETURN HB_QStyleHintReturnVariant():fromPointer( ... )


CREATE CLASS QStyleHintReturnVariant INHERIT HbQtObjectHandler, HB_QStyleHintReturn FUNCTION HB_QStyleHintReturnVariant

   METHOD  new( ... )


   ENDCLASS


METHOD QStyleHintReturnVariant:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStyleHintReturnVariant( ... )
   RETURN Self

