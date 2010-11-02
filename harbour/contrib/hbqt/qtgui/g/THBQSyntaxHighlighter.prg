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


FUNCTION HBQSyntaxHighlighter( ... )
   RETURN HB_HBQSyntaxHighlighter():new( ... )

FUNCTION HBQSyntaxHighlighterFromPointer( ... )
   RETURN HB_HBQSyntaxHighlighter():fromPointer( ... )


CREATE CLASS HBQSyntaxHighlighter INHERIT HbQtObjectHandler, HB_QSyntaxHighlighter FUNCTION HB_HBQSyntaxHighlighter

   METHOD  new( ... )

   METHOD  hbSetMultiLineCommentFormat   // ( oQTextCharFormat )                               -> NIL
   METHOD  hbSetSingleLineCommentFormat  // ( oQTextCharFormat )                               -> NIL
   METHOD  hbSetRule                     // ( cName, cPattern, oQTextCharFormat )              -> NIL
   METHOD  hbSetFormat                   // ( cName, oQTextCharFormat )                        -> NIL
   METHOD  hbSetFormatColumnSelection    // ( nStart, nCount, oQColor )                        -> NIL
   METHOD  hbSetRuleWithRegExp           // ( cName, oQRegExp, oQTextCharFormat )              -> NIL

   ENDCLASS


METHOD HBQSyntaxHighlighter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQSyntaxHighlighter( ... )
   RETURN Self


METHOD HBQSyntaxHighlighter:hbSetMultiLineCommentFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQSyntaxHighlighter_hbSetMultiLineCommentFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQSyntaxHighlighter:hbSetSingleLineCommentFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQSyntaxHighlighter_hbSetSingleLineCommentFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQSyntaxHighlighter:hbSetRule( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_HBQSyntaxHighlighter_hbSetRule( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQSyntaxHighlighter:hbSetFormat( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_HBQSyntaxHighlighter_hbSetFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQSyntaxHighlighter:hbSetFormatColumnSelection( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_HBQSyntaxHighlighter_hbSetFormatColumnSelection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD HBQSyntaxHighlighter:hbSetRuleWithRegExp( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_HBQSyntaxHighlighter_hbSetRuleWithRegExp( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

