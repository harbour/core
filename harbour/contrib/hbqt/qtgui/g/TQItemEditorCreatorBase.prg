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


FUNCTION QItemEditorCreatorBase( ... )
   RETURN HB_QItemEditorCreatorBase():new( ... )

FUNCTION QItemEditorCreatorBaseFromPointer( ... )
   RETURN HB_QItemEditorCreatorBase():fromPointer( ... )


CREATE CLASS QItemEditorCreatorBase INHERIT HbQtObjectHandler FUNCTION HB_QItemEditorCreatorBase

   METHOD  new( ... )

   METHOD  createWidget                  // ( oQWidget )                                       -> oQWidget
   METHOD  valuePropertyName             // (  )                                               -> oQByteArray

   ENDCLASS


METHOD QItemEditorCreatorBase:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QItemEditorCreatorBase( ... )
   RETURN Self


METHOD QItemEditorCreatorBase:createWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QItemEditorCreatorBase_createWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemEditorCreatorBase:valuePropertyName( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QItemEditorCreatorBase_valuePropertyName( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

