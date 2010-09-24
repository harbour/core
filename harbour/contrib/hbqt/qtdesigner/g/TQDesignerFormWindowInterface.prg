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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QDesignerFormWindowInterface( ... )
   RETURN HB_QDesignerFormWindowInterface():new( ... )


CREATE CLASS QDesignerFormWindowInterface INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDesignerFormWindowInterface

   METHOD  new( ... )

   METHOD  addResourceFile( cPath )
   METHOD  author()
   METHOD  comment()
   METHOD  contents()
   METHOD  core()
   METHOD  cursor()
   METHOD  emitSelectionChanged()
   METHOD  exportMacro()
   METHOD  features()
   METHOD  fileName()
   METHOD  grid()
   METHOD  hasFeature( nFeature )
   METHOD  includeHints()
   METHOD  isDirty()
   METHOD  isManaged( pWidget )
   METHOD  layoutDefault( nMargin, nSpacing )
   METHOD  mainContainer()
   METHOD  pixmapFunction()
   METHOD  removeResourceFile( cPath )
   METHOD  resourceFiles()
   METHOD  setAuthor( cAuthor )
   METHOD  setComment( cComment )
   METHOD  setContents( ... )
   METHOD  setExportMacro( cExportMacro )
   METHOD  setIncludeHints( pIncludeHints )
   METHOD  setLayoutDefault( nMargin, nSpacing )
   METHOD  setMainContainer( pMainContainer )
   METHOD  setPixmapFunction( cPixmapFunction )
   METHOD  findFormWindow( ... )
   METHOD  clearSelection( lUpdate )
   METHOD  manageWidget( pWidget )
   METHOD  selectWidget( pWidget, lSelect )
   METHOD  setDirty( lDirty )
   METHOD  setFeatures( nFeatures )
   METHOD  setFileName( cFileName )
   METHOD  setGrid( pGrid )
   METHOD  unmanageWidget( pWidget )

   ENDCLASS


METHOD QDesignerFormWindowInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerFormWindowInterface( ... )
   RETURN Self


METHOD QDesignerFormWindowInterface:addResourceFile( cPath )
   RETURN Qt_QDesignerFormWindowInterface_addResourceFile( ::pPtr, cPath )


METHOD QDesignerFormWindowInterface:author()
   RETURN Qt_QDesignerFormWindowInterface_author( ::pPtr )


METHOD QDesignerFormWindowInterface:comment()
   RETURN Qt_QDesignerFormWindowInterface_comment( ::pPtr )


METHOD QDesignerFormWindowInterface:contents()
   RETURN Qt_QDesignerFormWindowInterface_contents( ::pPtr )


METHOD QDesignerFormWindowInterface:core()
   RETURN Qt_QDesignerFormWindowInterface_core( ::pPtr )


METHOD QDesignerFormWindowInterface:cursor()
   RETURN Qt_QDesignerFormWindowInterface_cursor( ::pPtr )


METHOD QDesignerFormWindowInterface:emitSelectionChanged()
   RETURN Qt_QDesignerFormWindowInterface_emitSelectionChanged( ::pPtr )


METHOD QDesignerFormWindowInterface:exportMacro()
   RETURN Qt_QDesignerFormWindowInterface_exportMacro( ::pPtr )


METHOD QDesignerFormWindowInterface:features()
   RETURN Qt_QDesignerFormWindowInterface_features( ::pPtr )


METHOD QDesignerFormWindowInterface:fileName()
   RETURN Qt_QDesignerFormWindowInterface_fileName( ::pPtr )


METHOD QDesignerFormWindowInterface:grid()
   RETURN Qt_QDesignerFormWindowInterface_grid( ::pPtr )


METHOD QDesignerFormWindowInterface:hasFeature( nFeature )
   RETURN Qt_QDesignerFormWindowInterface_hasFeature( ::pPtr, nFeature )


METHOD QDesignerFormWindowInterface:includeHints()
   RETURN Qt_QDesignerFormWindowInterface_includeHints( ::pPtr )


METHOD QDesignerFormWindowInterface:isDirty()
   RETURN Qt_QDesignerFormWindowInterface_isDirty( ::pPtr )


METHOD QDesignerFormWindowInterface:isManaged( pWidget )
   RETURN Qt_QDesignerFormWindowInterface_isManaged( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QDesignerFormWindowInterface:layoutDefault( nMargin, nSpacing )
   RETURN Qt_QDesignerFormWindowInterface_layoutDefault( ::pPtr, nMargin, nSpacing )


METHOD QDesignerFormWindowInterface:mainContainer()
   RETURN Qt_QDesignerFormWindowInterface_mainContainer( ::pPtr )


METHOD QDesignerFormWindowInterface:pixmapFunction()
   RETURN Qt_QDesignerFormWindowInterface_pixmapFunction( ::pPtr )


METHOD QDesignerFormWindowInterface:removeResourceFile( cPath )
   RETURN Qt_QDesignerFormWindowInterface_removeResourceFile( ::pPtr, cPath )


METHOD QDesignerFormWindowInterface:resourceFiles()
   RETURN Qt_QDesignerFormWindowInterface_resourceFiles( ::pPtr )


METHOD QDesignerFormWindowInterface:setAuthor( cAuthor )
   RETURN Qt_QDesignerFormWindowInterface_setAuthor( ::pPtr, cAuthor )


METHOD QDesignerFormWindowInterface:setComment( cComment )
   RETURN Qt_QDesignerFormWindowInterface_setComment( ::pPtr, cComment )


METHOD QDesignerFormWindowInterface:setContents( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // virtual void setContents ( const QString & contents ) = 0
                // C c QString
         RETURN Qt_QDesignerFormWindowInterface_setContents_1( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO"
                // virtual void setContents ( QIODevice * device ) = 0
                // PO p QIODevice
         RETURN Qt_QDesignerFormWindowInterface_setContents( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QDesignerFormWindowInterface:setExportMacro( cExportMacro )
   RETURN Qt_QDesignerFormWindowInterface_setExportMacro( ::pPtr, cExportMacro )


METHOD QDesignerFormWindowInterface:setIncludeHints( pIncludeHints )
   RETURN Qt_QDesignerFormWindowInterface_setIncludeHints( ::pPtr, hbqt_ptr( pIncludeHints ) )


METHOD QDesignerFormWindowInterface:setLayoutDefault( nMargin, nSpacing )
   RETURN Qt_QDesignerFormWindowInterface_setLayoutDefault( ::pPtr, nMargin, nSpacing )


METHOD QDesignerFormWindowInterface:setMainContainer( pMainContainer )
   RETURN Qt_QDesignerFormWindowInterface_setMainContainer( ::pPtr, hbqt_ptr( pMainContainer ) )


METHOD QDesignerFormWindowInterface:setPixmapFunction( cPixmapFunction )
   RETURN Qt_QDesignerFormWindowInterface_setPixmapFunction( ::pPtr, cPixmapFunction )


METHOD QDesignerFormWindowInterface:findFormWindow( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QDesignerFormWindowInterface * findFormWindow ( QWidget * widget )
                // PO p QWidget
         RETURN QDesignerFormWindowInterface():from( Qt_QDesignerFormWindowInterface_findFormWindow( ::pPtr, ... ) )
                // QDesignerFormWindowInterface * findFormWindow ( QObject * object )
                // PO p QObject
         // RETURN QDesignerFormWindowInterface():from( Qt_QDesignerFormWindowInterface_findFormWindow_1( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QDesignerFormWindowInterface:clearSelection( lUpdate )
   RETURN Qt_QDesignerFormWindowInterface_clearSelection( ::pPtr, lUpdate )


METHOD QDesignerFormWindowInterface:manageWidget( pWidget )
   RETURN Qt_QDesignerFormWindowInterface_manageWidget( ::pPtr, hbqt_ptr( pWidget ) )


METHOD QDesignerFormWindowInterface:selectWidget( pWidget, lSelect )
   RETURN Qt_QDesignerFormWindowInterface_selectWidget( ::pPtr, hbqt_ptr( pWidget ), lSelect )


METHOD QDesignerFormWindowInterface:setDirty( lDirty )
   RETURN Qt_QDesignerFormWindowInterface_setDirty( ::pPtr, lDirty )


METHOD QDesignerFormWindowInterface:setFeatures( nFeatures )
   RETURN Qt_QDesignerFormWindowInterface_setFeatures( ::pPtr, nFeatures )


METHOD QDesignerFormWindowInterface:setFileName( cFileName )
   RETURN Qt_QDesignerFormWindowInterface_setFileName( ::pPtr, cFileName )


METHOD QDesignerFormWindowInterface:setGrid( pGrid )
   RETURN Qt_QDesignerFormWindowInterface_setGrid( ::pPtr, hbqt_ptr( pGrid ) )


METHOD QDesignerFormWindowInterface:unmanageWidget( pWidget )
   RETURN Qt_QDesignerFormWindowInterface_unmanageWidget( ::pPtr, hbqt_ptr( pWidget ) )

