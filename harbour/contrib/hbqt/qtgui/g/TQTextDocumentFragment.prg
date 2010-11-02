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


FUNCTION QTextDocumentFragment( ... )
   RETURN HB_QTextDocumentFragment():new( ... )

FUNCTION QTextDocumentFragmentFromPointer( ... )
   RETURN HB_QTextDocumentFragment():fromPointer( ... )


CREATE CLASS QTextDocumentFragment INHERIT HbQtObjectHandler FUNCTION HB_QTextDocumentFragment

   METHOD  new( ... )

   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  toHtml                        // ( oQByteArray )                                    -> cQString
                                         // (  )                                               -> cQString
   METHOD  toPlainText                   // (  )                                               -> cQString
   METHOD  fromHtml                      // ( cText )                                          -> oQTextDocumentFragment
                                         // ( cText, oQTextDocument )                          -> oQTextDocumentFragment
   METHOD  fromPlainText                 // ( cPlainText )                                     -> oQTextDocumentFragment

   ENDCLASS


METHOD QTextDocumentFragment:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextDocumentFragment( ... )
   RETURN Self


METHOD QTextDocumentFragment:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextDocumentFragment_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentFragment:toHtml( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextDocumentFragment_toHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QTextDocumentFragment_toHtml_1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentFragment:toPlainText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextDocumentFragment_toPlainText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentFragment:fromHtml( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QTextDocumentFragmentFromPointer( Qt_QTextDocumentFragment_fromHtml_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QTextDocumentFragmentFromPointer( Qt_QTextDocumentFragment_fromHtml( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextDocumentFragment:fromPlainText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QTextDocumentFragmentFromPointer( Qt_QTextDocumentFragment_fromPlainText( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

