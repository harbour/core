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


REQUEST __HBQTDESIGNER


FUNCTION QDesignerFormWindowInterface( ... )
   RETURN HB_QDesignerFormWindowInterface():new( ... )

FUNCTION QDesignerFormWindowInterfaceFromPointer( ... )
   RETURN HB_QDesignerFormWindowInterface():fromPointer( ... )


CREATE CLASS QDesignerFormWindowInterface INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDesignerFormWindowInterface

   METHOD  new( ... )

   METHOD  addResourceFile               // ( cPath )                                          -> NIL
   METHOD  author                        // (  )                                               -> cQString
   METHOD  comment                       // (  )                                               -> cQString
   METHOD  contents                      // (  )                                               -> cQString
   METHOD  core                          // (  )                                               -> oQDesignerFormEditorInterface
   METHOD  cursor                        // (  )                                               -> oQDesignerFormWindowCursorInterface
   METHOD  emitSelectionChanged          // (  )                                               -> NIL
   METHOD  exportMacro                   // (  )                                               -> cQString
   METHOD  features                      // (  )                                               -> nFeature
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  grid                          // (  )                                               -> oQPoint
   METHOD  hasFeature                    // ( nFeature )                                       -> lBool
   METHOD  includeHints                  // (  )                                               -> oQStringList
   METHOD  isDirty                       // (  )                                               -> lBool
   METHOD  isManaged                     // ( oQWidget )                                       -> lBool
   METHOD  layoutDefault                 // ( @nMargin, @nSpacing )                            -> NIL
   METHOD  mainContainer                 // (  )                                               -> oQWidget
   METHOD  pixmapFunction                // (  )                                               -> cQString
   METHOD  removeResourceFile            // ( cPath )                                          -> NIL
   METHOD  resourceFiles                 // (  )                                               -> oQStringList
   METHOD  setAuthor                     // ( cAuthor )                                        -> NIL
   METHOD  setComment                    // ( cComment )                                       -> NIL
   METHOD  setContents                   // ( oQIODevice )                                     -> NIL
   METHOD  setExportMacro                // ( cExportMacro )                                   -> NIL
   METHOD  setIncludeHints               // ( oQStringList )                                   -> NIL
   METHOD  setLayoutDefault              // ( nMargin, nSpacing )                              -> NIL
   METHOD  setMainContainer              // ( oQWidget )                                       -> NIL
   METHOD  setPixmapFunction             // ( cPixmapFunction )                                -> NIL
   METHOD  findFormWindow                // ( oQWidget )                                       -> oQDesignerFormWindowInterface
                                         // ( oQObject )                                       -> oQDesignerFormWindowInterface
   METHOD  clearSelection                // ( lUpdate )                                        -> NIL
   METHOD  manageWidget                  // ( oQWidget )                                       -> NIL
   METHOD  selectWidget                  // ( oQWidget, lSelect )                              -> NIL
                                         // ( cContents )                                      -> NIL
   METHOD  setDirty                      // ( lDirty )                                         -> NIL
   METHOD  setFeatures                   // ( nFeatures )                                      -> NIL
   METHOD  setFileName                   // ( cFileName )                                      -> NIL
   METHOD  setGrid                       // ( oQPoint )                                        -> NIL
   METHOD  unmanageWidget                // ( oQWidget )                                       -> NIL

   ENDCLASS


METHOD QDesignerFormWindowInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerFormWindowInterface( ... )
   RETURN Self


METHOD QDesignerFormWindowInterface:addResourceFile( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_addResourceFile( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:author( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_author( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:comment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_comment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:contents( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_contents( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:core( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerFormEditorInterfaceFromPointer( Qt_QDesignerFormWindowInterface_core( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:cursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerFormWindowCursorInterfaceFromPointer( Qt_QDesignerFormWindowInterface_cursor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:emitSelectionChanged( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_emitSelectionChanged( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:exportMacro( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_exportMacro( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:features( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_features( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:grid( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QDesignerFormWindowInterface_grid( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:hasFeature( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_hasFeature( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:includeHints( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QDesignerFormWindowInterface_includeHints( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:isDirty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_isDirty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:isManaged( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_isManaged( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:layoutDefault( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDesignerFormWindowInterface_layoutDefault( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:mainContainer( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QDesignerFormWindowInterface_mainContainer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:pixmapFunction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_pixmapFunction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:removeResourceFile( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_removeResourceFile( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:resourceFiles( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QDesignerFormWindowInterface_resourceFiles( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setAuthor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setAuthor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setComment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setComment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setContents( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setContents_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setContents( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setExportMacro( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setExportMacro( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setIncludeHints( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setIncludeHints( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setLayoutDefault( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDesignerFormWindowInterface_setLayoutDefault( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setMainContainer( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setMainContainer( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setPixmapFunction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setPixmapFunction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:findFormWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QWIDGET"
            RETURN QDesignerFormWindowInterfaceFromPointer( Qt_QDesignerFormWindowInterface_findFormWindow( ::pPtr, ... ) )
         CASE "QOBJECT"
            RETURN QDesignerFormWindowInterfaceFromPointer( Qt_QDesignerFormWindowInterface_findFormWindow_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:clearSelection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_clearSelection( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_clearSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:manageWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_manageWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:selectWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QDesignerFormWindowInterface_selectWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_selectWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setDirty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setDirty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setFeatures( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setFeatures( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setFileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setFileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:setGrid( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setGrid( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QDesignerFormWindowInterface:unmanageWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_unmanageWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

