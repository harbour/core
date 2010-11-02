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


FUNCTION QMenuBar( ... )
   RETURN HB_QMenuBar():new( ... )

FUNCTION QMenuBarFromPointer( ... )
   RETURN HB_QMenuBar():fromPointer( ... )


CREATE CLASS QMenuBar INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QMenuBar

   METHOD  new( ... )

   METHOD  activeAction                  // (  )                                               -> oQAction
   METHOD  addAction                     // ( cText )                                          -> oQAction
                                         // ( cText, oQObject, cMember )                       -> oQAction
                                         // ( oQAction )                                       -> NIL
   METHOD  addMenu                       // ( oQMenu )                                         -> oQAction
                                         // ( cTitle )                                         -> oQMenu
                                         // ( coQIcon, cTitle )                                -> oQMenu
   METHOD  addSeparator                  // (  )                                               -> oQAction
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  insertMenu                    // ( oQAction, oQMenu )                               -> oQAction
   METHOD  insertSeparator               // ( oQAction )                                       -> oQAction
   METHOD  isDefaultUp                   // (  )                                               -> lBool
   METHOD  setActiveAction               // ( oQAction )                                       -> NIL
   METHOD  setDefaultUp                  // ( lBool )                                          -> NIL

   ENDCLASS


METHOD QMenuBar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMenuBar( ... )
   RETURN Self


METHOD QMenuBar:activeAction( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QMenuBar_activeAction( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenuBar:addAction( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QActionFromPointer( Qt_QMenuBar_addAction_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QMenuBar_addAction( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMenuBar_addAction_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenuBar:addMenu( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QMenuFromPointer( Qt_QMenuBar_addMenu_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QMenuFromPointer( Qt_QMenuBar_addMenu_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QMenuBar_addMenu( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenuBar:addSeparator( ... )
   SWITCH PCount()
   CASE 0
      RETURN QActionFromPointer( Qt_QMenuBar_addSeparator( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenuBar:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMenuBar_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenuBar:insertMenu( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QMenuBar_insertMenu( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenuBar:insertSeparator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QMenuBar_insertSeparator( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenuBar:isDefaultUp( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMenuBar_isDefaultUp( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenuBar:setActiveAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QMenuBar_setActiveAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMenuBar:setDefaultUp( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QMenuBar_setDefaultUp( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

