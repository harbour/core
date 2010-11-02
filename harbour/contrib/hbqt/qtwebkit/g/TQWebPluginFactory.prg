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


REQUEST __HBQTWEBKIT


FUNCTION QWebPluginFactory( ... )
   RETURN HB_QWebPluginFactory():new( ... )

FUNCTION QWebPluginFactoryFromPointer( ... )
   RETURN HB_QWebPluginFactory():fromPointer( ... )


CREATE CLASS QWebPluginFactory INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QWebPluginFactory

   METHOD  new( ... )

   METHOD  create                        // ( cMimeType, oQUrl, oQStringList, oQStringList )   -> oQObject
   METHOD  refreshPlugins                // (  )                                               -> NIL
   METHOD  supportsExtension             // ( nExtension )                                     -> lBool

   ENDCLASS


METHOD QWebPluginFactory:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWebPluginFactory( ... )
   RETURN Self


METHOD QWebPluginFactory:create( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) )
         RETURN QObjectFromPointer( Qt_QWebPluginFactory_create( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebPluginFactory:refreshPlugins( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWebPluginFactory_refreshPlugins( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QWebPluginFactory:supportsExtension( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWebPluginFactory_supportsExtension( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

