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


FUNCTION QToolBar( ... )
   RETURN HB_QToolBar():new( ... )

FUNCTION QToolBarFromPointer( ... )
   RETURN HB_QToolBar():fromPointer( ... )


CREATE CLASS QToolBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QToolBar

   METHOD  new( ... )

   METHOD  actionAt                      // ( oQPoint )                                        -> oQAction
                                         // ( nX, nY )                                         -> oQAction
   METHOD  addAction                     // ( oQAction )                                       -> NIL
                                         // ( cText )                                          -> oQAction
                                         // ( coQIcon, cText )                                 -> oQAction
                                         // ( cText, oQObject, cMember )                       -> oQAction
                                         // ( coQIcon, cText, oQObject, cMember )              -> oQAction
   METHOD  addSeparator                  // (  )                                               -> oQAction
   METHOD  addWidget                     // ( oQWidget )                                       -> oQAction
   METHOD  allowedAreas                  // (  )                                               -> nQt_ToolBarAreas
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  iconSize                      // (  )                                               -> oQSize
   METHOD  insertSeparator               // ( oQAction )                                       -> oQAction
   METHOD  insertWidget                  // ( oQAction, oQWidget )                             -> oQAction
   METHOD  isAreaAllowed                 // ( nArea )                                          -> lBool
   METHOD  isFloatable                   // (  )                                               -> lBool
   METHOD  isFloating                    // (  )                                               -> lBool
   METHOD  isMovable                     // (  )                                               -> lBool
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  setAllowedAreas               // ( nAreas )                                         -> NIL
   METHOD  setFloatable                  // ( lFloatable )                                     -> NIL
   METHOD  setMovable                    // ( lMovable )                                       -> NIL
   METHOD  setOrientation                // ( nOrientation )                                   -> NIL
   METHOD  toggleViewAction              // (  )                                               -> oQAction
   METHOD  toolButtonStyle               // (  )                                               -> nQt_ToolButtonStyle
   METHOD  widgetForAction               // ( oQAction )                                       -> oQWidget
   METHOD  setIconSize                   // ( oQSize )                                         -> NIL
   METHOD  setToolButtonStyle            // ( nToolButtonStyle )                               -> NIL

   ENDCLASS


METHOD QToolBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QToolBar( ... )
   RETURN Self


METHOD QToolBar:actionAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QToolBar_actionAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QToolBar_actionAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:addAction( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isChar( hb_pvalue( 4 ) )
         RETURN QActionFromPointer( Qt_QToolBar_addAction_4( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QActionFromPointer( Qt_QToolBar_addAction_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QToolBar_addAction_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QToolBar_addAction_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_addAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:addSeparator( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QToolBar_addSeparator( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:addWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QToolBar_addWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:allowedAreas( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_allowedAreas( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:iconSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QToolBar_iconSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:insertSeparator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QToolBar_insertSeparator( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:insertWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QToolBar_insertWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:isAreaAllowed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_isAreaAllowed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:isFloatable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_isFloatable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:isFloating( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_isFloating( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:isMovable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_isMovable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setAllowedAreas( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setAllowedAreas( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setFloatable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setFloatable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setMovable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setMovable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setOrientation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setOrientation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:toggleViewAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QToolBar_toggleViewAction( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:toolButtonStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QToolBar_toolButtonStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:widgetForAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QToolBar_widgetForAction( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setIconSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setIconSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QToolBar:setToolButtonStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QToolBar_setToolButtonStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

