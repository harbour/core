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
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum CompletionMode { PopupCompletion, InlineCompletion, UnfilteredPopupCompletion }
 *  enum ModelSorting { UnsortedModel, CaseSensitivelySortedModel, CaseInsensitivelySortedModel }
 */

#include <QtCore/QPointer>

#include <QtGui/QCompleter>
#include <QtCore/QStringList>

/* QCompleter ( QObject * parent = 0 )
 * QCompleter ( QAbstractItemModel * model, QObject * parent = 0 )
 * QCompleter ( const QStringList & list, QObject * parent = 0 )
 * ~QCompleter ()
 */

typedef struct
{
   QPointer< QCompleter > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QCompleter;

QT_G_FUNC( hbqt_gcRelease_QCompleter )
{
   QCompleter  * ph = NULL ;
   QGC_POINTER_QCompleter * p = ( QGC_POINTER_QCompleter * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QCompleter   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QCompleter   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QCompleter          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QCompleter    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QCompleter    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QCompleter( void * pObj, bool bNew )
{
   QGC_POINTER_QCompleter * p = ( QGC_POINTER_QCompleter * ) hb_gcAllocate( sizeof( QGC_POINTER_QCompleter ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QCompleter >( ( QCompleter * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QCompleter;
   p->type = HBQT_TYPE_QCompleter;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QCompleter  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QCompleter", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QCOMPLETER )
{
   QCompleter * pObj = NULL;

   pObj = new QCompleter() ;

   hb_retptrGC( hbqt_gcAllocate_QCompleter( ( void * ) pObj, true ) );
}

/*
 * Qt::CaseSensitivity caseSensitivity () const
 */
HB_FUNC( QT_QCOMPLETER_CASESENSITIVITY )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retni( ( Qt::CaseSensitivity ) ( p )->caseSensitivity() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_CASESENSITIVITY FP=hb_retni( ( Qt::CaseSensitivity ) ( p )->caseSensitivity() ); p is NULL" ) );
   }
}

/*
 * int completionColumn () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONCOLUMN )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retni( ( p )->completionColumn() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_COMPLETIONCOLUMN FP=hb_retni( ( p )->completionColumn() ); p is NULL" ) );
   }
}

/*
 * int completionCount () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONCOUNT )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retni( ( p )->completionCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_COMPLETIONCOUNT FP=hb_retni( ( p )->completionCount() ); p is NULL" ) );
   }
}

/*
 * CompletionMode completionMode () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONMODE )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retni( ( QCompleter::CompletionMode ) ( p )->completionMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_COMPLETIONMODE FP=hb_retni( ( QCompleter::CompletionMode ) ( p )->completionMode() ); p is NULL" ) );
   }
}

/*
 * QAbstractItemModel * completionModel () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONMODEL )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->completionModel(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_COMPLETIONMODEL FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->completionModel(), false ) ); p is NULL" ) );
   }
}

/*
 * QString completionPrefix () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONPREFIX )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retc( ( p )->completionPrefix().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_COMPLETIONPREFIX FP=hb_retc( ( p )->completionPrefix().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int completionRole () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONROLE )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retni( ( p )->completionRole() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_COMPLETIONROLE FP=hb_retni( ( p )->completionRole() ); p is NULL" ) );
   }
}

/*
 * QString currentCompletion () const
 */
HB_FUNC( QT_QCOMPLETER_CURRENTCOMPLETION )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retc( ( p )->currentCompletion().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_CURRENTCOMPLETION FP=hb_retc( ( p )->currentCompletion().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QModelIndex currentIndex () const
 */
HB_FUNC( QT_QCOMPLETER_CURRENTINDEX )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->currentIndex() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_CURRENTINDEX FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->currentIndex() ), true ) ); p is NULL" ) );
   }
}

/*
 * int currentRow () const
 */
HB_FUNC( QT_QCOMPLETER_CURRENTROW )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retni( ( p )->currentRow() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_CURRENTROW FP=hb_retni( ( p )->currentRow() ); p is NULL" ) );
   }
}

/*
 * QAbstractItemModel * model () const
 */
HB_FUNC( QT_QCOMPLETER_MODEL )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->model(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_MODEL FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->model(), false ) ); p is NULL" ) );
   }
}

/*
 * ModelSorting modelSorting () const
 */
HB_FUNC( QT_QCOMPLETER_MODELSORTING )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retni( ( QCompleter::ModelSorting ) ( p )->modelSorting() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_MODELSORTING FP=hb_retni( ( QCompleter::ModelSorting ) ( p )->modelSorting() ); p is NULL" ) );
   }
}

/*
 * virtual QString pathFromIndex ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QCOMPLETER_PATHFROMINDEX )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retc( ( p )->pathFromIndex( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_PATHFROMINDEX FP=hb_retc( ( p )->pathFromIndex( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QAbstractItemView * popup () const
 */
HB_FUNC( QT_QCOMPLETER_POPUP )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemView( ( p )->popup(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_POPUP FP=hb_retptrGC( hbqt_gcAllocate_QAbstractItemView( ( p )->popup(), false ) ); p is NULL" ) );
   }
}

/*
 * void setCaseSensitivity ( Qt::CaseSensitivity caseSensitivity )
 */
HB_FUNC( QT_QCOMPLETER_SETCASESENSITIVITY )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      ( p )->setCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SETCASESENSITIVITY FP=( p )->setCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCompletionColumn ( int column )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONCOLUMN )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      ( p )->setCompletionColumn( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SETCOMPLETIONCOLUMN FP=( p )->setCompletionColumn( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCompletionMode ( CompletionMode mode )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONMODE )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      ( p )->setCompletionMode( ( QCompleter::CompletionMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SETCOMPLETIONMODE FP=( p )->setCompletionMode( ( QCompleter::CompletionMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCompletionRole ( int role )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONROLE )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      ( p )->setCompletionRole( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SETCOMPLETIONROLE FP=( p )->setCompletionRole( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool setCurrentRow ( int row )
 */
HB_FUNC( QT_QCOMPLETER_SETCURRENTROW )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retl( ( p )->setCurrentRow( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SETCURRENTROW FP=hb_retl( ( p )->setCurrentRow( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setModel ( QAbstractItemModel * model )
 */
HB_FUNC( QT_QCOMPLETER_SETMODEL )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      ( p )->setModel( hbqt_par_QAbstractItemModel( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SETMODEL FP=( p )->setModel( hbqt_par_QAbstractItemModel( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setModelSorting ( ModelSorting sorting )
 */
HB_FUNC( QT_QCOMPLETER_SETMODELSORTING )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      ( p )->setModelSorting( ( QCompleter::ModelSorting ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SETMODELSORTING FP=( p )->setModelSorting( ( QCompleter::ModelSorting ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPopup ( QAbstractItemView * popup )
 */
HB_FUNC( QT_QCOMPLETER_SETPOPUP )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      ( p )->setPopup( hbqt_par_QAbstractItemView( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SETPOPUP FP=( p )->setPopup( hbqt_par_QAbstractItemView( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWidget ( QWidget * widget )
 */
HB_FUNC( QT_QCOMPLETER_SETWIDGET )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      ( p )->setWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SETWIDGET FP=( p )->setWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual QStringList splitPath ( const QString & path ) const
 */
HB_FUNC( QT_QCOMPLETER_SPLITPATH )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->splitPath( QCompleter::tr( hb_parc( 2 ) ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SPLITPATH FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->splitPath( QCompleter::tr( hb_parc( 2 ) ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QWidget * widget () const
 */
HB_FUNC( QT_QCOMPLETER_WIDGET )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_WIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) ); p is NULL" ) );
   }
}

/*
 * bool wrapAround () const
 */
HB_FUNC( QT_QCOMPLETER_WRAPAROUND )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      hb_retl( ( p )->wrapAround() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_WRAPAROUND FP=hb_retl( ( p )->wrapAround() ); p is NULL" ) );
   }
}

/*
 * void complete ( const QRect & rect = QRect() )
 */
HB_FUNC( QT_QCOMPLETER_COMPLETE )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      ( p )->complete( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRect( 2 ) : QRect() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_COMPLETE FP=( p )->complete( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRect( 2 ) : QRect() ) ); p is NULL" ) );
   }
}

/*
 * void setCompletionPrefix ( const QString & prefix )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONPREFIX )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      ( p )->setCompletionPrefix( QCompleter::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SETCOMPLETIONPREFIX FP=( p )->setCompletionPrefix( QCompleter::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setWrapAround ( bool wrap )
 */
HB_FUNC( QT_QCOMPLETER_SETWRAPAROUND )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
      ( p )->setWrapAround( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QCOMPLETER_SETWRAPAROUND FP=( p )->setWrapAround( hb_parl( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
