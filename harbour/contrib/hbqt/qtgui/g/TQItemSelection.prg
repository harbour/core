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


FUNCTION QItemSelection( ... )
   RETURN HB_QItemSelection():new( ... )

FUNCTION QItemSelectionFromPointer( ... )
   RETURN HB_QItemSelection():fromPointer( ... )


CREATE CLASS QItemSelection INHERIT HbQtObjectHandler, HB_QList FUNCTION HB_QItemSelection

   METHOD  new( ... )

   METHOD  contains                      // ( oQModelIndex )                                   -> lBool
   METHOD  merge                         // ( oQItemSelection, nCommand )                      -> NIL
   METHOD  select                        // ( oQModelIndex, oQModelIndex )                     -> NIL

   ENDCLASS


METHOD QItemSelection:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QItemSelection( ... )
   RETURN Self


METHOD QItemSelection:contains( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QItemSelection_contains( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelection:merge( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QItemSelection_merge( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemSelection:select( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QItemSelection_select( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

