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


FUNCTION QFontDialog( ... )
   RETURN HB_QFontDialog():new( ... )

FUNCTION QFontDialogFromPointer( ... )
   RETURN HB_QFontDialog():fromPointer( ... )


CREATE CLASS QFontDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QFontDialog

   METHOD  new( ... )

   METHOD  currentFont                   // (  )                                               -> oQFont
   METHOD  options                       // (  )                                               -> nFontDialogOptions
   METHOD  selectedFont                  // (  )                                               -> oQFont
   METHOD  setCurrentFont                // ( oQFont )                                         -> NIL
   METHOD  setOption                     // ( nOption, lOn )                                   -> NIL
   METHOD  setOptions                    // ( nOptions )                                       -> NIL
   METHOD  testOption                    // ( nOption )                                        -> lBool
   METHOD  getFont                       // ( @lOk, oQFont, oQWidget, cTitle, nOptions )       -> oQFont
                                         // ( @lOk, oQFont, oQWidget, cName )                  -> oQFont
                                         // ( @lOk, oQFont, oQWidget, cTitle )                 -> oQFont
                                         // ( @lOk, oQFont, oQWidget )                         -> oQFont
                                         // ( @lOk, oQWidget )                                 -> oQFont

   ENDCLASS


METHOD QFontDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFontDialog( ... )
   RETURN Self


METHOD QFontDialog:currentFont( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QFontDialog_currentFont( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDialog:options( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFontDialog_options( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDialog:selectedFont( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QFontDialog_selectedFont( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDialog:setCurrentFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFontDialog_setCurrentFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDialog:setOption( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QFontDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFontDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDialog:setOptions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFontDialog_setOptions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDialog:testOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFontDialog_testOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFontDialog:getFont( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isChar( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN QFontFromPointer( Qt_QFontDialog_getFont( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isChar( hb_pvalue( 4 ) )
         SWITCH __objGetClsName( hb_pvalue( 2 ) ) + __objGetClsName( hb_pvalue( 3 ) )
         CASE "QFONTQWIDGET"
            RETURN QFontFromPointer( Qt_QFontDialog_getFont_1( ::pPtr, ... ) )
         CASE "QFONTQWIDGET"
            RETURN QFontFromPointer( Qt_QFontDialog_getFont_2( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QFontFromPointer( Qt_QFontDialog_getFont_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 2 ) )
         CASE "QWIDGET"
            RETURN QFontFromPointer( Qt_QFontDialog_getFont_4( ::pPtr, ... ) )
         CASE "QFONT"
            RETURN QFontFromPointer( Qt_QFontDialog_getFont_3( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN QFontFromPointer( Qt_QFontDialog_getFont_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

