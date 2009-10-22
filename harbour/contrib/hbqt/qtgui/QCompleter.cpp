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

#include "hbapi.h"
#include "../hbqt.h"

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

QT_G_FUNC( release_QCompleter )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QCompleter" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         delete ( ( QCompleter * ) ph );
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "  Object Name Missing: QCompleter" );  OutputDebugString( str );
#endif
      }
   }
}

HB_FUNC( QT_QCOMPLETER )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAlloc( sizeof( QGC_POINTER ), Q_release );
   QPointer< QCompleter > pObj = NULL;

   pObj = new QCompleter() ;

   p->ph = pObj;
   p->func = release_QCompleter;

   hb_retptrGC( p );
}
/*
 * Qt::CaseSensitivity caseSensitivity () const
 */
HB_FUNC( QT_QCOMPLETER_CASESENSITIVITY )
{
   hb_retni( ( Qt::CaseSensitivity ) hbqt_par_QCompleter( 1 )->caseSensitivity() );
}

/*
 * int completionColumn () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONCOLUMN )
{
   hb_retni( hbqt_par_QCompleter( 1 )->completionColumn() );
}

/*
 * int completionCount () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONCOUNT )
{
   hb_retni( hbqt_par_QCompleter( 1 )->completionCount() );
}

/*
 * CompletionMode completionMode () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONMODE )
{
   hb_retni( ( QCompleter::CompletionMode ) hbqt_par_QCompleter( 1 )->completionMode() );
}

/*
 * QAbstractItemModel * completionModel () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONMODEL )
{
   hb_retptr( ( QAbstractItemModel* ) hbqt_par_QCompleter( 1 )->completionModel() );
}

/*
 * QString completionPrefix () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONPREFIX )
{
   hb_retc( hbqt_par_QCompleter( 1 )->completionPrefix().toAscii().data() );
}

/*
 * int completionRole () const
 */
HB_FUNC( QT_QCOMPLETER_COMPLETIONROLE )
{
   hb_retni( hbqt_par_QCompleter( 1 )->completionRole() );
}

/*
 * QString currentCompletion () const
 */
HB_FUNC( QT_QCOMPLETER_CURRENTCOMPLETION )
{
   hb_retc( hbqt_par_QCompleter( 1 )->currentCompletion().toAscii().data() );
}

/*
 * QModelIndex currentIndex () const
 */
HB_FUNC( QT_QCOMPLETER_CURRENTINDEX )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QModelIndex( hbqt_par_QCompleter( 1 )->currentIndex() ), release_QModelIndex ) );
}

/*
 * int currentRow () const
 */
HB_FUNC( QT_QCOMPLETER_CURRENTROW )
{
   hb_retni( hbqt_par_QCompleter( 1 )->currentRow() );
}

/*
 * QAbstractItemModel * model () const
 */
HB_FUNC( QT_QCOMPLETER_MODEL )
{
   hb_retptr( ( QAbstractItemModel* ) hbqt_par_QCompleter( 1 )->model() );
}

/*
 * ModelSorting modelSorting () const
 */
HB_FUNC( QT_QCOMPLETER_MODELSORTING )
{
   hb_retni( ( QCompleter::ModelSorting ) hbqt_par_QCompleter( 1 )->modelSorting() );
}

/*
 * virtual QString pathFromIndex ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QCOMPLETER_PATHFROMINDEX )
{
   hb_retc( hbqt_par_QCompleter( 1 )->pathFromIndex( *hbqt_par_QModelIndex( 2 ) ).toAscii().data() );
}

/*
 * QAbstractItemView * popup () const
 */
HB_FUNC( QT_QCOMPLETER_POPUP )
{
   hb_retptr( ( QAbstractItemView* ) hbqt_par_QCompleter( 1 )->popup() );
}

/*
 * void setCaseSensitivity ( Qt::CaseSensitivity caseSensitivity )
 */
HB_FUNC( QT_QCOMPLETER_SETCASESENSITIVITY )
{
   hbqt_par_QCompleter( 1 )->setCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) );
}

/*
 * void setCompletionColumn ( int column )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONCOLUMN )
{
   hbqt_par_QCompleter( 1 )->setCompletionColumn( hb_parni( 2 ) );
}

/*
 * void setCompletionMode ( CompletionMode mode )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONMODE )
{
   hbqt_par_QCompleter( 1 )->setCompletionMode( ( QCompleter::CompletionMode ) hb_parni( 2 ) );
}

/*
 * void setCompletionRole ( int role )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONROLE )
{
   hbqt_par_QCompleter( 1 )->setCompletionRole( hb_parni( 2 ) );
}

/*
 * bool setCurrentRow ( int row )
 */
HB_FUNC( QT_QCOMPLETER_SETCURRENTROW )
{
   hb_retl( hbqt_par_QCompleter( 1 )->setCurrentRow( hb_parni( 2 ) ) );
}

/*
 * void setModel ( QAbstractItemModel * model )
 */
HB_FUNC( QT_QCOMPLETER_SETMODEL )
{
   hbqt_par_QCompleter( 1 )->setModel( hbqt_par_QAbstractItemModel( 2 ) );
}

/*
 * void setModelSorting ( ModelSorting sorting )
 */
HB_FUNC( QT_QCOMPLETER_SETMODELSORTING )
{
   hbqt_par_QCompleter( 1 )->setModelSorting( ( QCompleter::ModelSorting ) hb_parni( 2 ) );
}

/*
 * void setPopup ( QAbstractItemView * popup )
 */
HB_FUNC( QT_QCOMPLETER_SETPOPUP )
{
   hbqt_par_QCompleter( 1 )->setPopup( hbqt_par_QAbstractItemView( 2 ) );
}

/*
 * void setWidget ( QWidget * widget )
 */
HB_FUNC( QT_QCOMPLETER_SETWIDGET )
{
   hbqt_par_QCompleter( 1 )->setWidget( hbqt_par_QWidget( 2 ) );
}

/*
 * virtual QStringList splitPath ( const QString & path ) const
 */
HB_FUNC( QT_QCOMPLETER_SPLITPATH )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QStringList( hbqt_par_QCompleter( 1 )->splitPath( hbqt_par_QString( 2 ) ) ), release_QStringList ) );
}

/*
 * QWidget * widget () const
 */
HB_FUNC( QT_QCOMPLETER_WIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QCompleter( 1 )->widget() );
}

/*
 * bool wrapAround () const
 */
HB_FUNC( QT_QCOMPLETER_WRAPAROUND )
{
   hb_retl( hbqt_par_QCompleter( 1 )->wrapAround() );
}

/*
 * void complete ( const QRect & rect = QRect() )
 */
HB_FUNC( QT_QCOMPLETER_COMPLETE )
{
   hbqt_par_QCompleter( 1 )->complete( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRect( 2 ) : QRect() ) );
}

/*
 * void setCompletionPrefix ( const QString & prefix )
 */
HB_FUNC( QT_QCOMPLETER_SETCOMPLETIONPREFIX )
{
   hbqt_par_QCompleter( 1 )->setCompletionPrefix( hbqt_par_QString( 2 ) );
}

/*
 * void setWrapAround ( bool wrap )
 */
HB_FUNC( QT_QCOMPLETER_SETWRAPAROUND )
{
   hbqt_par_QCompleter( 1 )->setWrapAround( hb_parl( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
