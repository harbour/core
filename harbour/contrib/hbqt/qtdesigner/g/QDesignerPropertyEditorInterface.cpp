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

#include "hbqtcore.h"
#include "hbqtdesigner.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtDesigner/QDesignerPropertyEditorInterface>


/*
 * QDesignerPropertyEditorInterface ( QWidget * parent, Qt::WindowFlags flags = 0 )
 * ~QDesignerPropertyEditorInterface ()
 *
 */

typedef struct
{
   QPointer< QDesignerPropertyEditorInterface > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDesignerPropertyEditorInterface;

HBQT_GC_FUNC( hbqt_gcRelease_QDesignerPropertyEditorInterface )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDesignerPropertyEditorInterface( void * pObj, bool bNew )
{
   HBQT_GC_T_QDesignerPropertyEditorInterface * p = ( HBQT_GC_T_QDesignerPropertyEditorInterface * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDesignerPropertyEditorInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesignerPropertyEditorInterface >( ( QDesignerPropertyEditorInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerPropertyEditorInterface;
   p->type = HBQT_TYPE_QDesignerPropertyEditorInterface;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDesignerPropertyEditorInterface  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDesignerPropertyEditorInterface", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE )
{
   //hb_retptr( new QDesignerPropertyEditorInterface() );
}

/*
 * virtual QDesignerFormEditorInterface * core () const
 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_CORE )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( p )->core(), false ) );
   }
}

/*
 * virtual QString currentPropertyName () const = 0
 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_CURRENTPROPERTYNAME )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->currentPropertyName().toUtf8().data() );
   }
}

/*
 * virtual bool isReadOnly () const = 0
 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_ISREADONLY )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
   {
      hb_retl( ( p )->isReadOnly() );
   }
}

/*
 * virtual QObject * object () const = 0
 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_OBJECT )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QObject( ( p )->object(), false ) );
   }
}

/*
 * virtual void setObject ( QObject * object ) = 0
 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_SETOBJECT )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
   {
      ( p )->setObject( hbqt_par_QObject( 2 ) );
   }
}

/*
 * virtual void setPropertyValue ( const QString & name, const QVariant & value, bool changed = true ) = 0
 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_SETPROPERTYVALUE )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPropertyValue( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QVariant( 3 ), hb_parl( 4 ) );
      hb_strfree( pText );
   }
}

/*
 * virtual void setReadOnly ( bool readOnly ) = 0
 */
HB_FUNC( QT_QDESIGNERPROPERTYEDITORINTERFACE_SETREADONLY )
{
   QDesignerPropertyEditorInterface * p = hbqt_par_QDesignerPropertyEditorInterface( 1 );
   if( p )
   {
      ( p )->setReadOnly( hb_parl( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
