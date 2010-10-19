/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


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

