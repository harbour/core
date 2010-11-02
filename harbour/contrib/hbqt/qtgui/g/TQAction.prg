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


FUNCTION QAction( ... )
   RETURN HB_QAction():new( ... )

FUNCTION QActionFromPointer( ... )
   RETURN HB_QAction():fromPointer( ... )


CREATE CLASS QAction INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QAction

   METHOD  new( ... )

   METHOD  actionGroup                   // (  )                                               -> oQActionGroup
   METHOD  activate                      // ( nEvent )                                         -> NIL
   METHOD  associatedWidgets             // (  )                                               -> oQList_QWidget
   METHOD  autoRepeat                    // (  )                                               -> lBool
   METHOD  data                          // (  )                                               -> oQVariant
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  iconText                      // (  )                                               -> cQString
   METHOD  isCheckable                   // (  )                                               -> lBool
   METHOD  isChecked                     // (  )                                               -> lBool
   METHOD  isEnabled                     // (  )                                               -> lBool
   METHOD  isIconVisibleInMenu           // (  )                                               -> lBool
   METHOD  isSeparator                   // (  )                                               -> lBool
   METHOD  isVisible                     // (  )                                               -> lBool
   METHOD  menu                          // (  )                                               -> oQMenu
   METHOD  menuRole                      // (  )                                               -> nMenuRole
   METHOD  parentWidget                  // (  )                                               -> oQWidget
   METHOD  setActionGroup                // ( oQActionGroup )                                  -> NIL
   METHOD  setAutoRepeat                 // ( lBool )                                          -> NIL
   METHOD  setCheckable                  // ( lBool )                                          -> NIL
   METHOD  setData                       // ( oQVariant )                                      -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setIcon                       // ( coQIcon )                                        -> NIL
   METHOD  setIconText                   // ( cText )                                          -> NIL
   METHOD  setIconVisibleInMenu          // ( lVisible )                                       -> NIL
   METHOD  setMenu                       // ( oQMenu )                                         -> NIL
   METHOD  setMenuRole                   // ( nMenuRole )                                      -> NIL
   METHOD  setSeparator                  // ( lB )                                             -> NIL
   METHOD  setShortcut                   // ( oQKeySequence )                                  -> NIL
   METHOD  setShortcutContext            // ( nContext )                                       -> NIL
   METHOD  setShortcuts                  // ( nKey )                                           -> NIL
   METHOD  setStatusTip                  // ( cStatusTip )                                     -> NIL
   METHOD  setText                       // ( cText )                                          -> NIL
   METHOD  setToolTip                    // ( cTip )                                           -> NIL
   METHOD  setWhatsThis                  // ( cWhat )                                          -> NIL
   METHOD  shortcut                      // (  )                                               -> oQKeySequence
   METHOD  shortcutContext               // (  )                                               -> nQt_ShortcutContext
   METHOD  shortcuts                     // (  )                                               -> oQList_QKeySequence>
   METHOD  showStatusText                // ( oQWidget )                                       -> lBool
   METHOD  statusTip                     // (  )                                               -> cQString
   METHOD  text                          // (  )                                               -> cQString
   METHOD  toolTip                       // (  )                                               -> cQString
   METHOD  whatsThis                     // (  )                                               -> cQString
   METHOD  hover                         // (  )                                               -> NIL
   METHOD  setChecked                    // ( lBool )                                          -> NIL
   METHOD  setDisabled                   // ( lB )                                             -> NIL
   METHOD  setEnabled                    // ( lBool )                                          -> NIL
   METHOD  setVisible                    // ( lBool )                                          -> NIL
   METHOD  toggle                        // (  )                                               -> NIL
   METHOD  trigger                       // (  )                                               -> NIL

   ENDCLASS


METHOD QAction:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAction( ... )
   RETURN Self


METHOD QAction:actionGroup( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionGroupFromPointer( Qt_QAction_actionGroup( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:activate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAction_activate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:associatedWidgets( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QAction_associatedWidgets( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:autoRepeat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_autoRepeat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:data( ... )
   SWITCH PCount()
   CASE 0
      RETURN QVariantFromPointer( Qt_QAction_data( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QAction_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QAction_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:iconText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_iconText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:isCheckable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_isCheckable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:isChecked( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_isChecked( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:isEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_isEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:isIconVisibleInMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_isIconVisibleInMenu( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:isSeparator( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_isSeparator( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:isVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_isVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:menu( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMenuFromPointer( Qt_QAction_menu( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:menuRole( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_menuRole( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:parentWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QAction_parentWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setActionGroup( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setActionGroup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setAutoRepeat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setAutoRepeat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setCheckable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setCheckable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setData( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QAction_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setIconText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setIconText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setIconVisibleInMenu( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setIconVisibleInMenu( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setMenu( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setMenu( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setMenuRole( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setMenuRole( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setSeparator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setSeparator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setShortcut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setShortcut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setShortcutContext( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setShortcutContext( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setShortcuts( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setShortcuts( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setStatusTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setStatusTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setWhatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:shortcut( ... )
   SWITCH PCount()
   CASE 0
      RETURN QKeySequenceFromPointer( Qt_QAction_shortcut( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:shortcutContext( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_shortcutContext( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:shortcuts( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QAction_shortcuts( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:showStatusText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QAction_showStatusText( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QAction_showStatusText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:statusTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_statusTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:toolTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_toolTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:whatsThis( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_whatsThis( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:hover( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_hover( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setChecked( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setChecked( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setDisabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setDisabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:setVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAction_setVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:toggle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_toggle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAction:trigger( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAction_trigger( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

