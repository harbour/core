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


FUNCTION QColorDialog( ... )
   RETURN HB_QColorDialog():new( ... )

FUNCTION QColorDialogFromPointer( ... )
   RETURN HB_QColorDialog():fromPointer( ... )


CREATE CLASS QColorDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QColorDialog

   METHOD  new( ... )

   METHOD  currentColor                  // (  )                                               -> oQColor
   METHOD  open                          // (  )                                               -> NIL
   METHOD  options                       // (  )                                               -> nColorDialogOptions
   METHOD  selectedColor                 // (  )                                               -> oQColor
   METHOD  setCurrentColor               // ( oQColor )                                        -> NIL
   METHOD  setOption                     // ( nOption, lOn )                                   -> NIL
   METHOD  setOptions                    // ( nOptions )                                       -> NIL
   METHOD  setVisible                    // ( lVisible )                                       -> NIL
   METHOD  testOption                    // ( nOption )                                        -> lBool
   METHOD  customColor                   // ( nIndex )                                         -> nQRgb
   METHOD  customCount                   // (  )                                               -> nInt
   METHOD  getColor                      // ( oQColor, oQWidget, cTitle, nOptions )            -> oQColor
                                         // ( oQColor, oQWidget )                              -> oQColor
   METHOD  setCustomColor                // ( nIndex, nColor )                                 -> NIL
   METHOD  setStandardColor              // ( nIndex, nColor )                                 -> NIL

   ENDCLASS


METHOD QColorDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QColorDialog( ... )
   RETURN Self


METHOD QColorDialog:currentColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QColorDialog_currentColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:open( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColorDialog_open( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:options( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColorDialog_options( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:selectedColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QColorDialog_selectedColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:setCurrentColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QColorDialog_setCurrentColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:setOption( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QColorDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColorDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:setOptions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColorDialog_setOptions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:setVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QColorDialog_setVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:testOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColorDialog_testOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:customColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QColorDialog_customColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:customCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QColorDialog_customCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:getColor( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QColorFromPointer( Qt_QColorDialog_getColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QColorFromPointer( Qt_QColorDialog_getColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QColorFromPointer( Qt_QColorDialog_getColor_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QColorDialog_getColor_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QColorFromPointer( Qt_QColorDialog_getColor_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:setCustomColor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QColorDialog_setCustomColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QColorDialog:setStandardColor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QColorDialog_setStandardColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

