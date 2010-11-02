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


FUNCTION QButtonGroup( ... )
   RETURN HB_QButtonGroup():new( ... )

FUNCTION QButtonGroupFromPointer( ... )
   RETURN HB_QButtonGroup():fromPointer( ... )


CREATE CLASS QButtonGroup INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QButtonGroup

   METHOD  new( ... )

   METHOD  addButton                     // ( oQAbstractButton )                               -> NIL
                                         // ( oQAbstractButton, nId )                          -> NIL
   METHOD  button                        // ( nId )                                            -> oQAbstractButton
   METHOD  buttons                       // (  )                                               -> oQList_QAbstractButton
   METHOD  checkedButton                 // (  )                                               -> oQAbstractButton
   METHOD  checkedId                     // (  )                                               -> nInt
   METHOD  exclusive                     // (  )                                               -> lBool
   METHOD  id                            // ( oQAbstractButton )                               -> nInt
   METHOD  removeButton                  // ( oQAbstractButton )                               -> NIL
   METHOD  setExclusive                  // ( lBool )                                          -> NIL
   METHOD  setId                         // ( oQAbstractButton, nId )                          -> NIL

   ENDCLASS


METHOD QButtonGroup:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QButtonGroup( ... )
   RETURN Self


METHOD QButtonGroup:addButton( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QButtonGroup_addButton_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QButtonGroup_addButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QButtonGroup:button( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QAbstractButtonFromPointer( Qt_QButtonGroup_button( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QButtonGroup:buttons( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QButtonGroup_buttons( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QButtonGroup:checkedButton( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractButtonFromPointer( Qt_QButtonGroup_checkedButton( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QButtonGroup:checkedId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QButtonGroup_checkedId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QButtonGroup:exclusive( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QButtonGroup_exclusive( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QButtonGroup:id( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QButtonGroup_id( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QButtonGroup:removeButton( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QButtonGroup_removeButton( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QButtonGroup:setExclusive( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QButtonGroup_setExclusive( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QButtonGroup:setId( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QButtonGroup_setId( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

