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


FUNCTION QTextObject( ... )
   RETURN HB_QTextObject():new( ... )

FUNCTION QTextObjectFromPointer( ... )
   RETURN HB_QTextObject():fromPointer( ... )


CREATE CLASS QTextObject INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QTextObject

   METHOD  new( ... )

   METHOD  document                      // (  )                                               -> oQTextDocument
   METHOD  format                        // (  )                                               -> oQTextFormat
   METHOD  formatIndex                   // (  )                                               -> nInt
   METHOD  objectIndex                   // (  )                                               -> nInt

   ENDCLASS


METHOD QTextObject:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextObject( ... )
   RETURN Self


METHOD QTextObject:document( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDocumentFromPointer( Qt_QTextObject_document( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextObject:format( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextFormatFromPointer( Qt_QTextObject_format( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextObject:formatIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextObject_formatIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextObject:objectIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextObject_objectIndex( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

