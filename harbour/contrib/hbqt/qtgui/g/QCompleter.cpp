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

/*
 *  Constructed[ 29/29 [ 100.00% ] ]
 *
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QCompleter;

HBQT_GC_FUNC( hbqt_gcRelease_QCompleter )
{
   QCompleter  * ph = NULL ;
   HBQT_GC_T_QCompleter * p = ( HBQT_GC_T_QCompleter * ) Cargo;

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
   HBQT_GC_T_QCompleter * p = ( HBQT_GC_T_QCompleter * ) hb_gcAllocate( sizeof( HBQT_GC_T_QCompleter ), hbqt_gcFuncs() );

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
   {
      hb_retni( ( Qt::CaseSensitivity ) ( p )->caseSensitivity() );
   }
}

/*
 * int completionColumn () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONCOLUMN )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retni( ( p )->completionColumn() );
   }
}

/*
 * int completionCount () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONCOUNT )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retni( ( p )->completionCount() );
   }
}

/*
 * CompletionMode completionMode () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONMODE )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retni( ( QCompleter::CompletionMode ) ( p )->completionMode() );
   }
}

/*
 * QAbstractItemModel * completionModel () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONMODEL )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->completionModel(), false ) );
   }
}

/*
 * QString completionPrefix () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONPREFIX )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->completionPrefix().toUtf8().data() );
   }
}

/*
 * int completionRole () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONROLE )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retni( ( p )->completionRole() );
   }
}

/*
 * QString currentCompletion () const
 */
HB_FUNC( QT_QCOMPLETER_CURRENTCOMPLETION )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->currentCompletion().toUtf8().data() );
   }
}

/*
 * QModelIndex currentIndex () const
 */
HB_FUNC( QT_QCOMPLETER_CURRENTINDEX )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->currentIndex() ), true ) );
   }
}

/*
 * int currentRow () const
 */
HB_FUNC( QT_QCOMPLETER_CURRENTROW )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retni( ( p )->currentRow() );
   }
}

/*
 * QAbstractItemModel * model () const
 */
HB_FUNC( QT_QCOMPLETER_MODEL )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->model(), false ) );
   }
}

/*
 * ModelSorting modelSorting () const
 */
HB_FUNC( QT_QCOMPLETER_MODELSORTING )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retni( ( QCompleter::ModelSorting ) ( p )->modelSorting() );
   }
}

/*
 * virtual QString pathFromIndex ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QCOMPLETER_PATHFROMINDEX )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->pathFromIndex( *hbqt_par_QModelIndex( 2 ) ).toUtf8().data() );
   }
}

/*
 * QAbstractItemView * popup () const
 */
HB_FUNC( QT_QCOMPLETER_POPUP )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemView( ( p )->popup(), false ) );
   }
}

/*
 * void setCaseSensitivity ( Qt::CaseSensitivity caseSensitivity )
 */
HB_FUNC( QT_QCOMPLETER_SETCASESENSITIVITY )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      ( p )->setCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) );
   }
}

/*
 * void setCompletionColumn ( int column )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONCOLUMN )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      ( p )->setCompletionColumn( hb_parni( 2 ) );
   }
}

/*
 * void setCompletionMode ( CompletionMode mode )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONMODE )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      ( p )->setCompletionMode( ( QCompleter::CompletionMode ) hb_parni( 2 ) );
   }
}

/*
 * void setCompletionRole ( int role )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONROLE )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      ( p )->setCompletionRole( hb_parni( 2 ) );
   }
}

/*
 * bool setCurrentRow ( int row )
 */
HB_FUNC( QT_QCOMPLETER_SETCURRENTROW )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retl( ( p )->setCurrentRow( hb_parni( 2 ) ) );
   }
}

/*
 * void setModel ( QAbstractItemModel * model )
 */
HB_FUNC( QT_QCOMPLETER_SETMODEL )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      ( p )->setModel( hbqt_par_QAbstractItemModel( 2 ) );
   }
}

/*
 * void setModelSorting ( ModelSorting sorting )
 */
HB_FUNC( QT_QCOMPLETER_SETMODELSORTING )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      ( p )->setModelSorting( ( QCompleter::ModelSorting ) hb_parni( 2 ) );
   }
}

/*
 * void setPopup ( QAbstractItemView * popup )
 */
HB_FUNC( QT_QCOMPLETER_SETPOPUP )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      ( p )->setPopup( hbqt_par_QAbstractItemView( 2 ) );
   }
}

/*
 * void setWidget ( QWidget * widget )
 */
HB_FUNC( QT_QCOMPLETER_SETWIDGET )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      ( p )->setWidget( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * virtual QStringList splitPath ( const QString & path ) const
 */
HB_FUNC( QT_QCOMPLETER_SPLITPATH )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->splitPath( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QWidget * widget () const
 */
HB_FUNC( QT_QCOMPLETER_WIDGET )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
   }
}

/*
 * bool wrapAround () const
 */
HB_FUNC( QT_QCOMPLETER_WRAPAROUND )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      hb_retl( ( p )->wrapAround() );
   }
}

/*
 * void complete ( const QRect & rect = QRect() )
 */
HB_FUNC( QT_QCOMPLETER_COMPLETE )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      ( p )->complete( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRect( 2 ) : QRect() ) );
   }
}

/*
 * void setCompletionPrefix ( const QString & prefix )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONPREFIX )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      void * pText;
      ( p )->setCompletionPrefix( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setWrapAround ( bool wrap )
 */
HB_FUNC( QT_QCOMPLETER_SETWRAPAROUND )
{
   QCompleter * p = hbqt_par_QCompleter( 1 );
   if( p )
   {
      ( p )->setWrapAround( hb_parl( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
