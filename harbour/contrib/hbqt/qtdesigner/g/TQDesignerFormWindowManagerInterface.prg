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


REQUEST __HBQTDESIGNER


FUNCTION QDesignerFormWindowManagerInterface( ... )
   RETURN HB_QDesignerFormWindowManagerInterface():new( ... )

FUNCTION QDesignerFormWindowManagerInterfaceFromPointer( ... )
   RETURN HB_QDesignerFormWindowManagerInterface():fromPointer( ... )


CREATE CLASS QDesignerFormWindowManagerInterface INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QDesignerFormWindowManagerInterface

   METHOD  new( ... )

   METHOD  actionAdjustSize              // (  )                                               -> oQAction
   METHOD  actionBreakLayout             // (  )                                               -> oQAction
   METHOD  actionCopy                    // (  )                                               -> oQAction
   METHOD  actionCut                     // (  )                                               -> oQAction
   METHOD  actionDelete                  // (  )                                               -> oQAction
   METHOD  actionFormLayout              // (  )                                               -> oQAction
   METHOD  actionGridLayout              // (  )                                               -> oQAction
   METHOD  actionHorizontalLayout        // (  )                                               -> oQAction
   METHOD  actionLower                   // (  )                                               -> oQAction
   METHOD  actionPaste                   // (  )                                               -> oQAction
   METHOD  actionRaise                   // (  )                                               -> oQAction
   METHOD  actionRedo                    // (  )                                               -> oQAction
   METHOD  actionSelectAll               // (  )                                               -> oQAction
   METHOD  actionSimplifyLayout          // (  )                                               -> oQAction
   METHOD  actionSplitHorizontal         // (  )                                               -> oQAction
   METHOD  actionSplitVertical           // (  )                                               -> oQAction
   METHOD  actionUndo                    // (  )                                               -> oQAction
   METHOD  actionVerticalLayout          // (  )                                               -> oQAction
   METHOD  activeFormWindow              // (  )                                               -> oQDesignerFormWindowInterface
   METHOD  core                          // (  )                                               -> oQDesignerFormEditorInterface
   METHOD  createFormWindow              // ( oQWidget, nFlags )                               -> oQDesignerFormWindowInterface
   METHOD  formWindow                    // ( nIndex )                                         -> oQDesignerFormWindowInterface
   METHOD  formWindowCount               // (  )                                               -> nInt
   METHOD  addFormWindow                 // ( oQDesignerFormWindowInterface )                  -> NIL
   METHOD  removeFormWindow              // ( oQDesignerFormWindowInterface )                  -> NIL
   METHOD  setActiveFormWindow           // ( oQDesignerFormWindowInterface )                  -> NIL

   ENDCLASS


METHOD QDesignerFormWindowManagerInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerFormWindowManagerInterface( ... )
   RETURN Self


METHOD QDesignerFormWindowManagerInterface:actionAdjustSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionAdjustSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionBreakLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionBreakLayout( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionCopy( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionCopy( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionCut( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionCut( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionDelete( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionDelete( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionFormLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionFormLayout( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionGridLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionGridLayout( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionHorizontalLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionHorizontalLayout( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionLower( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionLower( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionPaste( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionPaste( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionRaise( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionRaise( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionRedo( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionRedo( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionSelectAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionSelectAll( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionSimplifyLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionSimplifyLayout( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionSplitHorizontal( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionSplitHorizontal( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionSplitVertical( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionSplitVertical( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionUndo( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionUndo( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:actionVerticalLayout( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QDesignerFormWindowManagerInterface_actionVerticalLayout( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:activeFormWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerFormWindowInterfaceFromPointer( Qt_QDesignerFormWindowManagerInterface_activeFormWindow( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:core( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerFormEditorInterfaceFromPointer( Qt_QDesignerFormWindowManagerInterface_core( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:createFormWindow( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QDesignerFormWindowInterfaceFromPointer( Qt_QDesignerFormWindowManagerInterface_createFormWindow( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QDesignerFormWindowInterfaceFromPointer( Qt_QDesignerFormWindowManagerInterface_createFormWindow( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QDesignerFormWindowInterfaceFromPointer( Qt_QDesignerFormWindowManagerInterface_createFormWindow( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:formWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QDesignerFormWindowInterfaceFromPointer( Qt_QDesignerFormWindowManagerInterface_formWindow( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:formWindowCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowManagerInterface_formWindowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:addFormWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowManagerInterface_addFormWindow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:removeFormWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowManagerInterface_removeFormWindow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowManagerInterface:setActiveFormWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowManagerInterface_setActiveFormWindow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

