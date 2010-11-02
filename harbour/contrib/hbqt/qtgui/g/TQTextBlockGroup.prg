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


FUNCTION QTextBlockGroup( ... )
   RETURN HB_QTextBlockGroup():new( ... )

FUNCTION QTextBlockGroupFromPointer( ... )
   RETURN HB_QTextBlockGroup():fromPointer( ... )


CREATE CLASS QTextBlockGroup INHERIT HbQtObjectHandler, HB_QTextObject FUNCTION HB_QTextBlockGroup

   METHOD  new( ... )


   ENDCLASS


METHOD QTextBlockGroup:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextBlockGroup( ... )
   RETURN Self

