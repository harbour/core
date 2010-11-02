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


FUNCTION QSyntaxHighlighter( ... )
   RETURN HB_QSyntaxHighlighter():new( ... )

FUNCTION QSyntaxHighlighterFromPointer( ... )
   RETURN HB_QSyntaxHighlighter():fromPointer( ... )


CREATE CLASS QSyntaxHighlighter INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QSyntaxHighlighter

   METHOD  new( ... )

   METHOD  document                      // (  )                                               -> oQTextDocument
   METHOD  setDocument                   // ( oQTextDocument )                                 -> NIL
   METHOD  rehighlight                   // (  )                                               -> NIL

   ENDCLASS


METHOD QSyntaxHighlighter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSyntaxHighlighter( ... )
   RETURN Self


METHOD QSyntaxHighlighter:document( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDocumentFromPointer( Qt_QSyntaxHighlighter_document( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSyntaxHighlighter:setDocument( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSyntaxHighlighter_setDocument( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSyntaxHighlighter:rehighlight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSyntaxHighlighter_rehighlight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

