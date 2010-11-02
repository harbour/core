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


FUNCTION QCompleter( ... )
   RETURN HB_QCompleter():new( ... )

FUNCTION QCompleterFromPointer( ... )
   RETURN HB_QCompleter():fromPointer( ... )


CREATE CLASS QCompleter INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QCompleter

   METHOD  new( ... )

   METHOD  caseSensitivity               // (  )                                               -> nQt_CaseSensitivity
   METHOD  completionColumn              // (  )                                               -> nInt
   METHOD  completionCount               // (  )                                               -> nInt
   METHOD  completionMode                // (  )                                               -> nCompletionMode
   METHOD  completionModel               // (  )                                               -> oQAbstractItemModel
   METHOD  completionPrefix              // (  )                                               -> cQString
   METHOD  completionRole                // (  )                                               -> nInt
   METHOD  currentCompletion             // (  )                                               -> cQString
   METHOD  currentIndex                  // (  )                                               -> oQModelIndex
   METHOD  currentRow                    // (  )                                               -> nInt
   METHOD  model                         // (  )                                               -> oQAbstractItemModel
   METHOD  modelSorting                  // (  )                                               -> nModelSorting
   METHOD  pathFromIndex                 // ( oQModelIndex )                                   -> cQString
   METHOD  popup                         // (  )                                               -> oQAbstractItemView
   METHOD  setCaseSensitivity            // ( nCaseSensitivity )                               -> NIL
   METHOD  setCompletionColumn           // ( nColumn )                                        -> NIL
   METHOD  setCompletionMode             // ( nMode )                                          -> NIL
   METHOD  setCompletionRole             // ( nRole )                                          -> NIL
   METHOD  setCurrentRow                 // ( nRow )                                           -> lBool
   METHOD  setModel                      // ( oQAbstractItemModel )                            -> NIL
   METHOD  setModelSorting               // ( nSorting )                                       -> NIL
   METHOD  setPopup                      // ( oQAbstractItemView )                             -> NIL
   METHOD  setWidget                     // ( oQWidget )                                       -> NIL
   METHOD  splitPath                     // ( cPath )                                          -> oQStringList
   METHOD  widget                        // (  )                                               -> oQWidget
   METHOD  wrapAround                    // (  )                                               -> lBool
   METHOD  complete                      // ( oQRect )                                         -> NIL
   METHOD  setCompletionPrefix           // ( cPrefix )                                        -> NIL
   METHOD  setWrapAround                 // ( lWrap )                                          -> NIL

   ENDCLASS


METHOD QCompleter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QCompleter( ... )
   RETURN Self


METHOD QCompleter:caseSensitivity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCompleter_caseSensitivity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:completionColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCompleter_completionColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:completionCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCompleter_completionCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:completionMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCompleter_completionMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:completionModel( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractItemModelFromPointer( Qt_QCompleter_completionModel( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:completionPrefix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCompleter_completionPrefix( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:completionRole( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCompleter_completionRole( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:currentCompletion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCompleter_currentCompletion( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:currentIndex( ... )
   SWITCH PCount()
   CASE 0
      RETURN QModelIndexFromPointer( Qt_QCompleter_currentIndex( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:currentRow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCompleter_currentRow( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:model( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractItemModelFromPointer( Qt_QCompleter_model( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:modelSorting( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCompleter_modelSorting( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:pathFromIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_pathFromIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:popup( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractItemViewFromPointer( Qt_QCompleter_popup( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:setCaseSensitivity( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_setCaseSensitivity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:setCompletionColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_setCompletionColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:setCompletionMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_setCompletionMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:setCompletionRole( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_setCompletionRole( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:setCurrentRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_setCurrentRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:setModel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_setModel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:setModelSorting( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_setModelSorting( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:setPopup( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_setPopup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:setWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_setWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:splitPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QCompleter_splitPath( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:widget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QCompleter_widget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:wrapAround( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCompleter_wrapAround( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:complete( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_complete( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QCompleter_complete( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:setCompletionPrefix( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_setCompletionPrefix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCompleter:setWrapAround( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QCompleter_setWrapAround( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

