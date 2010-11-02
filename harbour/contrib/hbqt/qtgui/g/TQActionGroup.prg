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


FUNCTION QActionGroup( ... )
   RETURN HB_QActionGroup():new( ... )

FUNCTION QActionGroupFromPointer( ... )
   RETURN HB_QActionGroup():fromPointer( ... )


CREATE CLASS QActionGroup INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QActionGroup

   METHOD  new( ... )

   METHOD  actions                       // (  )                                               -> oQList_QAction
   METHOD  addAction                     // ( oQAction )                                       -> oQAction
                                         // ( cText )                                          -> oQAction
                                         // ( coQIcon, cText )                                 -> oQAction
   METHOD  checkedAction                 // (  )                                               -> oQAction
   METHOD  isEnabled                     // (  )                                               -> lBool
   METHOD  isExclusive                   // (  )                                               -> lBool
   METHOD  isVisible                     // (  )                                               -> lBool
   METHOD  removeAction                  // ( oQAction )                                       -> NIL
   METHOD  setDisabled                   // ( lB )                                             -> NIL
   METHOD  setEnabled                    // ( lBool )                                          -> NIL
   METHOD  setExclusive                  // ( lBool )                                          -> NIL
   METHOD  setVisible                    // ( lBool )                                          -> NIL

   ENDCLASS


METHOD QActionGroup:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QActionGroup( ... )
   RETURN Self


METHOD QActionGroup:actions( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QActionGroup_actions( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QActionGroup:addAction( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QActionGroup_addAction_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QActionGroup_addAction_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QActionGroup_addAction( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QActionGroup:checkedAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QActionGroup_checkedAction( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QActionGroup:isEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QActionGroup_isEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QActionGroup:isExclusive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QActionGroup_isExclusive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QActionGroup:isVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QActionGroup_isVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QActionGroup:removeAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QActionGroup_removeAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QActionGroup:setDisabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QActionGroup_setDisabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QActionGroup:setEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QActionGroup_setEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QActionGroup:setExclusive( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QActionGroup_setExclusive( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QActionGroup:setVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QActionGroup_setVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

