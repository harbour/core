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


FUNCTION QsciAbstractAPIs( ... )
   RETURN HB_QsciAbstractAPIs():new( ... )

FUNCTION QsciAbstractAPIsFromPointer( ... )
   RETURN HB_QsciAbstractAPIs():fromPointer( ... )


CREATE CLASS QsciAbstractAPIs INHERIT HbQtObjectHandler FUNCTION HB_QsciAbstractAPIs

   METHOD  new( ... )

   METHOD  lexer                         // (  )                                               -> oQsciLexer
   METHOD  updateAutoCompletionList      // ( oQStringList, oQStringList )                     -> NIL
   METHOD  autoCompletionSelected        // ( cSelection )                                     -> NIL

   ENDCLASS


METHOD QsciAbstractAPIs:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciAbstractAPIs( ... )
   RETURN Self


METHOD QsciAbstractAPIs:lexer( ... )
   SWITCH PCount()
   CASE 0
      RETURN QsciLexerFromPointer( Qt_QsciAbstractAPIs_lexer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAbstractAPIs:updateAutoCompletionList( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QsciAbstractAPIs_updateAutoCompletionList( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciAbstractAPIs:autoCompletionSelected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QsciAbstractAPIs_autoCompletionSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

