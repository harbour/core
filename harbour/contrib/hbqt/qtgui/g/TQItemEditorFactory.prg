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


FUNCTION QItemEditorFactory( ... )
   RETURN HB_QItemEditorFactory():new( ... )

FUNCTION QItemEditorFactoryFromPointer( ... )
   RETURN HB_QItemEditorFactory():fromPointer( ... )


CREATE CLASS QItemEditorFactory INHERIT HbQtObjectHandler FUNCTION HB_QItemEditorFactory

   METHOD  new( ... )

   METHOD  createEditor                  // ( nType, oQWidget )                                -> oQWidget
   METHOD  registerEditor                // ( nType, oQItemEditorCreatorBase )                 -> NIL
   METHOD  valuePropertyName             // ( nType )                                          -> oQByteArray
   METHOD  setDefaultFactory             // ( oQItemEditorFactory )                            -> NIL

   ENDCLASS


METHOD QItemEditorFactory:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QItemEditorFactory( ... )
   RETURN Self


METHOD QItemEditorFactory:createEditor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QItemEditorFactory_createEditor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemEditorFactory:registerEditor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QItemEditorFactory_registerEditor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemEditorFactory:valuePropertyName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QItemEditorFactory_valuePropertyName( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QItemEditorFactory:setDefaultFactory( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QItemEditorFactory_setDefaultFactory( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

