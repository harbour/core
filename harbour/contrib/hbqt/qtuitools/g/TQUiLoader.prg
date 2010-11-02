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


REQUEST __HBQTUITOOLS


FUNCTION QUiLoader( ... )
   RETURN HB_QUiLoader():new( ... )

FUNCTION QUiLoaderFromPointer( ... )
   RETURN HB_QUiLoader():fromPointer( ... )


CREATE CLASS QUiLoader INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QUiLoader

   METHOD  new( ... )

   METHOD  addPluginPath                 // ( cPath )                                          -> NIL
   METHOD  availableLayouts              // (  )                                               -> oQStringList
   METHOD  availableWidgets              // (  )                                               -> oQStringList
   METHOD  clearPluginPaths              // (  )                                               -> NIL
   METHOD  createAction                  // ( oQObject, cName )                                -> oQAction
   METHOD  createActionGroup             // ( oQObject, cName )                                -> oQActionGroup
   METHOD  createLayout                  // ( cClassName, oQObject, cName )                    -> oQLayout
   METHOD  createWidget                  // ( cClassName, oQWidget, cName )                    -> oQWidget
   METHOD  isLanguageChangeEnabled       // (  )                                               -> lBool
   METHOD  load                          // ( oQIODevice, oQWidget )                           -> oQWidget
   METHOD  pluginPaths                   // (  )                                               -> oQStringList
   METHOD  setLanguageChangeEnabled      // ( lEnabled )                                       -> NIL
   METHOD  setWorkingDirectory           // ( oQDir )                                          -> NIL
   METHOD  workingDirectory              // (  )                                               -> oQDir

   ENDCLASS


METHOD QUiLoader:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QUiLoader( ... )
   RETURN Self


METHOD QUiLoader:addPluginPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUiLoader_addPluginPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:availableLayouts( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QUiLoader_availableLayouts( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:availableWidgets( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QUiLoader_availableWidgets( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:clearPluginPaths( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUiLoader_clearPluginPaths( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:createAction( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QUiLoader_createAction( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QUiLoader_createAction( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QActionFromPointer( Qt_QUiLoader_createAction( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:createActionGroup( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QActionGroupFromPointer( Qt_QUiLoader_createActionGroup( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionGroupFromPointer( Qt_QUiLoader_createActionGroup( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QActionGroupFromPointer( Qt_QUiLoader_createActionGroup( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:createLayout( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QLayoutFromPointer( Qt_QUiLoader_createLayout( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QLayoutFromPointer( Qt_QUiLoader_createLayout( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QLayoutFromPointer( Qt_QUiLoader_createLayout( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:createWidget( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QWidgetFromPointer( Qt_QUiLoader_createWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QUiLoader_createWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QUiLoader_createWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:isLanguageChangeEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUiLoader_isLanguageChangeEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:load( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QUiLoader_load( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QUiLoader_load( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:pluginPaths( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QUiLoader_pluginPaths( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:setLanguageChangeEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QUiLoader_setLanguageChangeEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:setWorkingDirectory( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUiLoader_setWorkingDirectory( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:workingDirectory( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QUiLoader_workingDirectory( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

