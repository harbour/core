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


FUNCTION QSizePolicy( ... )
   RETURN HB_QSizePolicy():new( ... )

FUNCTION QSizePolicyFromPointer( ... )
   RETURN HB_QSizePolicy():fromPointer( ... )


CREATE CLASS QSizePolicy INHERIT HbQtObjectHandler FUNCTION HB_QSizePolicy

   METHOD  new( ... )

   METHOD  controlType                   // (  )                                               -> nControlType
   METHOD  expandingDirections           // (  )                                               -> nQt_Orientations
   METHOD  hasHeightForWidth             // (  )                                               -> lBool
   METHOD  horizontalPolicy              // (  )                                               -> nPolicy
   METHOD  horizontalStretch             // (  )                                               -> nInt
   METHOD  setControlType                // ( nType )                                          -> NIL
   METHOD  setHeightForWidth             // ( lDependent )                                     -> NIL
   METHOD  setHorizontalPolicy           // ( nPolicy )                                        -> NIL
   METHOD  setHorizontalStretch          // ( nStretchFactor )                                 -> NIL
   METHOD  setVerticalPolicy             // ( nPolicy )                                        -> NIL
   METHOD  setVerticalStretch            // ( nStretchFactor )                                 -> NIL
   METHOD  transpose                     // (  )                                               -> NIL
   METHOD  verticalPolicy                // (  )                                               -> nPolicy
   METHOD  verticalStretch               // (  )                                               -> nInt

   ENDCLASS


METHOD QSizePolicy:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSizePolicy( ... )
   RETURN Self


METHOD QSizePolicy:controlType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizePolicy_controlType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:expandingDirections( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizePolicy_expandingDirections( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:hasHeightForWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizePolicy_hasHeightForWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:horizontalPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizePolicy_horizontalPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:horizontalStretch( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizePolicy_horizontalStretch( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:setControlType( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSizePolicy_setControlType( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:setHeightForWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QSizePolicy_setHeightForWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:setHorizontalPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSizePolicy_setHorizontalPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:setHorizontalStretch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSizePolicy_setHorizontalStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:setVerticalPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSizePolicy_setVerticalPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:setVerticalStretch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSizePolicy_setVerticalStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:transpose( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizePolicy_transpose( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:verticalPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizePolicy_verticalPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizePolicy:verticalStretch( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizePolicy_verticalStretch( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

