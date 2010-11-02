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


FUNCTION QPalette( ... )
   RETURN HB_QPalette():new( ... )

FUNCTION QPaletteFromPointer( ... )
   RETURN HB_QPalette():fromPointer( ... )


CREATE CLASS QPalette INHERIT HbQtObjectHandler FUNCTION HB_QPalette

   METHOD  new( ... )

   METHOD  alternateBase                 // (  )                                               -> oQBrush
   METHOD  base                          // (  )                                               -> oQBrush
   METHOD  brightText                    // (  )                                               -> oQBrush
   METHOD  brush                         // ( nGroup, nRole )                                  -> oQBrush
                                         // ( nRole )                                          -> oQBrush
   METHOD  button                        // (  )                                               -> oQBrush
   METHOD  buttonText                    // (  )                                               -> oQBrush
   METHOD  cacheKey                      // (  )                                               -> nQint64
   METHOD  color                         // ( nGroup, nRole )                                  -> oQColor
                                         // ( nRole )                                          -> oQColor
   METHOD  currentColorGroup             // (  )                                               -> nColorGroup
   METHOD  dark                          // (  )                                               -> oQBrush
   METHOD  highlight                     // (  )                                               -> oQBrush
   METHOD  highlightedText               // (  )                                               -> oQBrush
   METHOD  isBrushSet                    // ( nCg, nCr )                                       -> lBool
   METHOD  isCopyOf                      // ( oQPalette )                                      -> lBool
   METHOD  isEqual                       // ( nCg1, nCg2 )                                     -> lBool
   METHOD  light                         // (  )                                               -> oQBrush
   METHOD  link                          // (  )                                               -> oQBrush
   METHOD  linkVisited                   // (  )                                               -> oQBrush
   METHOD  mid                           // (  )                                               -> oQBrush
   METHOD  midlight                      // (  )                                               -> oQBrush
   METHOD  resolve                       // ( oQPalette )                                      -> oQPalette
   METHOD  setBrush                      // ( nRole, oQBrush )                                 -> NIL
                                         // ( nGroup, nRole, oQBrush )                         -> NIL
   METHOD  setColor                      // ( nRole, oQColor )                                 -> NIL
                                         // ( nGroup, nRole, oQColor )                         -> NIL
   METHOD  setColorGroup                 // ( nCg, oQBrush, oQBrush, oQBrush, oQBrush, oQBrush, oQBrush, oQBrush, oQBrush, oQBrush ) -> NIL
   METHOD  setCurrentColorGroup          // ( nCg )                                            -> NIL
   METHOD  shadow                        // (  )                                               -> oQBrush
   METHOD  text                          // (  )                                               -> oQBrush
   METHOD  toolTipBase                   // (  )                                               -> oQBrush
   METHOD  toolTipText                   // (  )                                               -> oQBrush
   METHOD  window                        // (  )                                               -> oQBrush
   METHOD  windowText                    // (  )                                               -> oQBrush

   ENDCLASS


METHOD QPalette:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPalette( ... )
   RETURN Self


METHOD QPalette:alternateBase( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_alternateBase( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:base( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_base( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:brightText( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_brightText( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:brush( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QBrushFromPointer( Qt_QPalette_brush( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QBrushFromPointer( Qt_QPalette_brush_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:button( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_button( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:buttonText( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_buttonText( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:cacheKey( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPalette_cacheKey( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:color( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QColorFromPointer( Qt_QPalette_color( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QPalette_color_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:currentColorGroup( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPalette_currentColorGroup( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:dark( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_dark( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:highlight( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_highlight( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:highlightedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_highlightedText( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:isBrushSet( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPalette_isBrushSet( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:isCopyOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPalette_isCopyOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:isEqual( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPalette_isEqual( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:light( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_light( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:link( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_link( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:linkVisited( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_linkVisited( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:mid( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_mid( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:midlight( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_midlight( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:resolve( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPaletteFromPointer( Qt_QPalette_resolve( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:setBrush( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPalette_setBrush_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPalette_setBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:setColor( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPalette_setColor_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPalette_setColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:setColorGroup( ... )
   SWITCH PCount()
   CASE 10
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) ) .AND. hb_isObject( hb_pvalue( 6 ) ) .AND. hb_isObject( hb_pvalue( 7 ) ) .AND. hb_isObject( hb_pvalue( 8 ) ) .AND. hb_isObject( hb_pvalue( 9 ) ) .AND. hb_isObject( hb_pvalue( 10 ) )
         RETURN Qt_QPalette_setColorGroup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:setCurrentColorGroup( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPalette_setCurrentColorGroup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:shadow( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_shadow( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_text( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:toolTipBase( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_toolTipBase( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:toolTipText( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_toolTipText( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:window( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_window( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPalette:windowText( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QPalette_windowText( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

