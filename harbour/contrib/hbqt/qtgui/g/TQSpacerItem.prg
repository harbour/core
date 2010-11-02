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


FUNCTION QSpacerItem( ... )
   RETURN HB_QSpacerItem():new( ... )

FUNCTION QSpacerItemFromPointer( ... )
   RETURN HB_QSpacerItem():fromPointer( ... )


CREATE CLASS QSpacerItem INHERIT HbQtObjectHandler, HB_QLayoutItem FUNCTION HB_QSpacerItem

   METHOD  new( ... )

   METHOD  changeSize                    // ( nW, nH, nHPolicy, nVPolicy )                     -> NIL
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  spacerItem                    // (  )                                               -> oQSpacerItem

   ENDCLASS


METHOD QSpacerItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSpacerItem( ... )
   RETURN Self


METHOD QSpacerItem:changeSize( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QSpacerItem_changeSize( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QSpacerItem_changeSize( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QSpacerItem_changeSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpacerItem:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSpacerItem_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSpacerItem:spacerItem( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSpacerItemFromPointer( Qt_QSpacerItem_spacerItem( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

