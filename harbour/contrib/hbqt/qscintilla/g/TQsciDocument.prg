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


REQUEST __HBQSCINTILLA


FUNCTION QsciDocument( ... )
   RETURN HB_QsciDocument():new( ... )

FUNCTION QsciDocumentFromPointer( ... )
   RETURN HB_QsciDocument():fromPointer( ... )


CREATE CLASS QsciDocument INHERIT HbQtObjectHandler FUNCTION HB_QsciDocument

   METHOD  new( ... )


   ENDCLASS


METHOD QsciDocument:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciDocument( ... )
   RETURN Self

