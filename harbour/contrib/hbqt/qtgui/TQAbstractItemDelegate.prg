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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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


#include "hbclass.ch"


CREATE CLASS QAbstractItemDelegate INHERIT QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  createEditor( pParent, pOption, pIndex )
   METHOD  editorEvent( pEvent, pModel, pOption, pIndex )
   METHOD  paint( pPainter, pOption, pIndex )
   METHOD  setEditorData( pEditor, pIndex )
   METHOD  setModelData( pEditor, pModel, pIndex )
   METHOD  sizeHint( pOption, pIndex )
   METHOD  updateEditorGeometry( pEditor, pOption, pIndex )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD QAbstractItemDelegate:New( pParent )
   ::pParent := pParent
   ::pPtr := Qt_QAbstractItemDelegate( pParent )
   RETURN Self


METHOD QAbstractItemDelegate:Configure( xObject )
   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF
   RETURN Self


METHOD QAbstractItemDelegate:createEditor( pParent, pOption, pIndex )
   RETURN Qt_QAbstractItemDelegate_createEditor( ::pPtr, pParent, pOption, pIndex )


METHOD QAbstractItemDelegate:editorEvent( pEvent, pModel, pOption, pIndex )
   RETURN Qt_QAbstractItemDelegate_editorEvent( ::pPtr, pEvent, pModel, pOption, pIndex )


METHOD QAbstractItemDelegate:paint( pPainter, pOption, pIndex )
   RETURN Qt_QAbstractItemDelegate_paint( ::pPtr, pPainter, pOption, pIndex )


METHOD QAbstractItemDelegate:setEditorData( pEditor, pIndex )
   RETURN Qt_QAbstractItemDelegate_setEditorData( ::pPtr, pEditor, pIndex )


METHOD QAbstractItemDelegate:setModelData( pEditor, pModel, pIndex )
   RETURN Qt_QAbstractItemDelegate_setModelData( ::pPtr, pEditor, pModel, pIndex )


METHOD QAbstractItemDelegate:sizeHint( pOption, pIndex )
   RETURN Qt_QAbstractItemDelegate_sizeHint( ::pPtr, pOption, pIndex )


METHOD QAbstractItemDelegate:updateEditorGeometry( pEditor, pOption, pIndex )
   RETURN Qt_QAbstractItemDelegate_updateEditorGeometry( ::pPtr, pEditor, pOption, pIndex )

