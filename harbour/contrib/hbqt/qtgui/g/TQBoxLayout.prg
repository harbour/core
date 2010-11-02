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


FUNCTION QBoxLayout( ... )
   RETURN HB_QBoxLayout():new( ... )

FUNCTION QBoxLayoutFromPointer( ... )
   RETURN HB_QBoxLayout():fromPointer( ... )


CREATE CLASS QBoxLayout INHERIT HbQtObjectHandler, HB_QLayout FUNCTION HB_QBoxLayout

   METHOD  new( ... )

   METHOD  addLayout                     // ( oQLayout, nStretch )                             -> NIL
   METHOD  addSpacerItem                 // ( oQSpacerItem )                                   -> NIL
   METHOD  addSpacing                    // ( nSize )                                          -> NIL
   METHOD  addStretch                    // ( nStretch )                                       -> NIL
   METHOD  addStrut                      // ( nSize )                                          -> NIL
   METHOD  addWidget                     // ( oQWidget, nStretch, nAlignment )                 -> NIL
   METHOD  direction                     // (  )                                               -> nDirection
   METHOD  insertLayout                  // ( nIndex, oQLayout, nStretch )                     -> NIL
   METHOD  insertSpacerItem              // ( nIndex, oQSpacerItem )                           -> NIL
   METHOD  insertSpacing                 // ( nIndex, nSize )                                  -> NIL
   METHOD  insertStretch                 // ( nIndex, nStretch )                               -> NIL
   METHOD  insertWidget                  // ( nIndex, oQWidget, nStretch, nAlignment )         -> NIL
   METHOD  invalidate                    // (  )                                               -> NIL
   METHOD  setDirection                  // ( nDirection )                                     -> NIL
   METHOD  setSpacing                    // ( nSpacing )                                       -> NIL
   METHOD  setStretch                    // ( nIndex, nStretch )                               -> NIL
   METHOD  setStretchFactor              // ( oQWidget, nStretch )                             -> lBool
                                         // ( oQLayout, nStretch )                             -> lBool
   METHOD  spacing                       // (  )                                               -> nInt
   METHOD  stretch                       // ( nIndex )                                         -> nInt

   ENDCLASS


METHOD QBoxLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QBoxLayout( ... )
   RETURN Self


METHOD QBoxLayout:addLayout( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QBoxLayout_addLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QBoxLayout_addLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:addSpacerItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QBoxLayout_addSpacerItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:addSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBoxLayout_addSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:addStretch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBoxLayout_addStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QBoxLayout_addStretch( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:addStrut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBoxLayout_addStrut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:addWidget( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QBoxLayout_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QBoxLayout_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QBoxLayout_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:direction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QBoxLayout_direction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:insertLayout( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QBoxLayout_insertLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QBoxLayout_insertLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:insertSpacerItem( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QBoxLayout_insertSpacerItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:insertSpacing( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QBoxLayout_insertSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:insertStretch( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QBoxLayout_insertStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBoxLayout_insertStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:insertWidget( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QBoxLayout_insertWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QBoxLayout_insertWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QBoxLayout_insertWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:invalidate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QBoxLayout_invalidate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:setDirection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBoxLayout_setDirection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:setSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBoxLayout_setSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:setStretch( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QBoxLayout_setStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:setStretchFactor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QWIDGET"
            RETURN Qt_QBoxLayout_setStretchFactor( ::pPtr, ... )
         CASE "QLAYOUT"
            RETURN Qt_QBoxLayout_setStretchFactor_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:spacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QBoxLayout_spacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QBoxLayout:stretch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QBoxLayout_stretch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

