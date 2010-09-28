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


FUNCTION QDesignerFormEditorInterface( ... )
   RETURN HB_QDesignerFormEditorInterface():new( ... )


CREATE CLASS QDesignerFormEditorInterface INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QDesignerFormEditorInterface

   METHOD  new( ... )

   METHOD  actionEditor()
   METHOD  formWindowManager()
   METHOD  objectInspector()
   METHOD  propertyEditor()
   METHOD  setActionEditor( pActionEditor )
   METHOD  setObjectInspector( pObjectInspector )
   METHOD  setPropertyEditor( pPropertyEditor )
   METHOD  setWidgetBox( pWidgetBox )
   METHOD  topLevel()
   METHOD  widgetBox()

   ENDCLASS


METHOD QDesignerFormEditorInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerFormEditorInterface( ... )
   RETURN Self


METHOD QDesignerFormEditorInterface:actionEditor()
   RETURN HB_QDesignerActionEditorInterface():from( Qt_QDesignerFormEditorInterface_actionEditor( ::pPtr ) )


METHOD QDesignerFormEditorInterface:formWindowManager()
   RETURN HB_QDesignerFormWindowManagerInterface():from( Qt_QDesignerFormEditorInterface_formWindowManager( ::pPtr ) )


METHOD QDesignerFormEditorInterface:objectInspector()
   RETURN HB_QDesignerObjectInspectorInterface():from( Qt_QDesignerFormEditorInterface_objectInspector( ::pPtr ) )


METHOD QDesignerFormEditorInterface:propertyEditor()
   RETURN HB_QDesignerPropertyEditorInterface():from( Qt_QDesignerFormEditorInterface_propertyEditor( ::pPtr ) )


METHOD QDesignerFormEditorInterface:setActionEditor( pActionEditor )
   RETURN Qt_QDesignerFormEditorInterface_setActionEditor( ::pPtr, hbqt_ptr( pActionEditor ) )


METHOD QDesignerFormEditorInterface:setObjectInspector( pObjectInspector )
   RETURN Qt_QDesignerFormEditorInterface_setObjectInspector( ::pPtr, hbqt_ptr( pObjectInspector ) )


METHOD QDesignerFormEditorInterface:setPropertyEditor( pPropertyEditor )
   RETURN Qt_QDesignerFormEditorInterface_setPropertyEditor( ::pPtr, hbqt_ptr( pPropertyEditor ) )


METHOD QDesignerFormEditorInterface:setWidgetBox( pWidgetBox )
   RETURN Qt_QDesignerFormEditorInterface_setWidgetBox( ::pPtr, hbqt_ptr( pWidgetBox ) )


METHOD QDesignerFormEditorInterface:topLevel()
   RETURN HB_QWidget():from( Qt_QDesignerFormEditorInterface_topLevel( ::pPtr ) )


METHOD QDesignerFormEditorInterface:widgetBox()
   RETURN HB_QDesignerWidgetBoxInterface():from( Qt_QDesignerFormEditorInterface_widgetBox( ::pPtr ) )

