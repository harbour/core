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

#include "hbqt.h"
#include "hbqtdesigner_garbage.h"
#include "hbqtdesigner.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtDesigner/QDesignerFormEditorInterface>


/*
 * QDesignerFormEditorInterface ( QObject * parent = 0 )
 * virtual ~QDesignerFormEditorInterface ()
 *
 */

typedef struct
{
   QPointer< QDesignerFormEditorInterface > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QDesignerFormEditorInterface;

QT_G_FUNC( hbqt_gcRelease_QDesignerFormEditorInterface )
{
   QDesignerFormEditorInterface  * ph = NULL ;
   QGC_POINTER_QDesignerFormEditorInterface * p = ( QGC_POINTER_QDesignerFormEditorInterface * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QDesignerFormEditorInterface   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QDesignerFormEditorInterface   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QDesignerFormEditorInterface          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QDesignerFormEditorInterface    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QDesignerFormEditorInterface    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDesignerFormEditorInterface( void * pObj, bool bNew )
{
   QGC_POINTER_QDesignerFormEditorInterface * p = ( QGC_POINTER_QDesignerFormEditorInterface * ) hb_gcAllocate( sizeof( QGC_POINTER_QDesignerFormEditorInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesignerFormEditorInterface >( ( QDesignerFormEditorInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerFormEditorInterface;
   p->type = HBQT_TYPE_QDesignerFormEditorInterface;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDesignerFormEditorInterface  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDesignerFormEditorInterface", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE )
{
   QDesignerFormEditorInterface * pObj = NULL;

   if( HB_ISPOINTER( 1 ) )
   {
      pObj = new QDesignerFormEditorInterface( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = new QDesignerFormEditorInterface() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( void * ) pObj, true ) );
}

/*
 * QDesignerActionEditorInterface * actionEditor () const
 */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_ACTIONEDITOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerActionEditorInterface( ( p )->actionEditor(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMEDITORINTERFACE_ACTIONEDITOR FP=hb_retptrGC( hbqt_gcAllocate_QDesignerActionEditorInterface( ( p )->actionEditor(), false ) ); p is NULL" ) );
   }
}

/*
 * QDesignerFormWindowManagerInterface * formWindowManager () const
 */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_FORMWINDOWMANAGER )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowManagerInterface( ( p )->formWindowManager(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMEDITORINTERFACE_FORMWINDOWMANAGER FP=hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowManagerInterface( ( p )->formWindowManager(), false ) ); p is NULL" ) );
   }
}

/*
 * QDesignerObjectInspectorInterface * objectInspector () const
 */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_OBJECTINSPECTOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerObjectInspectorInterface( ( p )->objectInspector(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMEDITORINTERFACE_OBJECTINSPECTOR FP=hb_retptrGC( hbqt_gcAllocate_QDesignerObjectInspectorInterface( ( p )->objectInspector(), false ) ); p is NULL" ) );
   }
}

/*
 * QDesignerPropertyEditorInterface * propertyEditor () const
 */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_PROPERTYEDITOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerPropertyEditorInterface( ( p )->propertyEditor(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMEDITORINTERFACE_PROPERTYEDITOR FP=hb_retptrGC( hbqt_gcAllocate_QDesignerPropertyEditorInterface( ( p )->propertyEditor(), false ) ); p is NULL" ) );
   }
}

/*
 * void setActionEditor ( QDesignerActionEditorInterface * actionEditor )
 */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_SETACTIONEDITOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      ( p )->setActionEditor( hbqt_par_QDesignerActionEditorInterface( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMEDITORINTERFACE_SETACTIONEDITOR FP=( p )->setActionEditor( hbqt_par_QDesignerActionEditorInterface( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setObjectInspector ( QDesignerObjectInspectorInterface * objectInspector )
 */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_SETOBJECTINSPECTOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      ( p )->setObjectInspector( hbqt_par_QDesignerObjectInspectorInterface( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMEDITORINTERFACE_SETOBJECTINSPECTOR FP=( p )->setObjectInspector( hbqt_par_QDesignerObjectInspectorInterface( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPropertyEditor ( QDesignerPropertyEditorInterface * propertyEditor )
 */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_SETPROPERTYEDITOR )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      ( p )->setPropertyEditor( hbqt_par_QDesignerPropertyEditorInterface( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMEDITORINTERFACE_SETPROPERTYEDITOR FP=( p )->setPropertyEditor( hbqt_par_QDesignerPropertyEditorInterface( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWidgetBox ( QDesignerWidgetBoxInterface * widgetBox )
 */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_SETWIDGETBOX )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      ( p )->setWidgetBox( hbqt_par_QDesignerWidgetBoxInterface( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMEDITORINTERFACE_SETWIDGETBOX FP=( p )->setWidgetBox( hbqt_par_QDesignerWidgetBoxInterface( 2 ) ); p is NULL" ) );
   }
}

/*
 * QWidget * topLevel () const
 */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_TOPLEVEL )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->topLevel(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMEDITORINTERFACE_TOPLEVEL FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->topLevel(), false ) ); p is NULL" ) );
   }
}

/*
 * QDesignerWidgetBoxInterface * widgetBox () const
 */
HB_FUNC( QT_QDESIGNERFORMEDITORINTERFACE_WIDGETBOX )
{
   QDesignerFormEditorInterface * p = hbqt_par_QDesignerFormEditorInterface( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesignerWidgetBoxInterface( ( p )->widgetBox(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDESIGNERFORMEDITORINTERFACE_WIDGETBOX FP=hb_retptrGC( hbqt_gcAllocate_QDesignerWidgetBoxInterface( ( p )->widgetBox(), false ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
