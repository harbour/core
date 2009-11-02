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

#include <QtCore/QPointer>

#include <QtGui/QStringListModel>


/*
 * QStringListModel ( QObject * parent = 0 )
 * QStringListModel ( const QStringList & strings, QObject * parent = 0 )
 */

QT_G_FUNC( release_QStringListModel )
{
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "release_QStringListModel            %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QStringListModel * ) ph )->~QStringListModel();
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "  Object Name Missing: QStringListModel" );  OutputDebugString( str );
#endif
      }
   }
   else
   {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "! ph____QStringListModel" );  OutputDebugString( str );
#endif
   }
}

HB_FUNC( QT_QSTRINGLISTMODEL )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   QPointer< QStringListModel > pObj = NULL;
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:  new QStringListModel            %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif

   pObj = ( QStringListModel* ) new QStringListModel() ;

#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:                                  %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   p->ph = pObj;
   p->func = release_QStringListModel;

   hb_retptrGC( p );
}
/*
 * virtual QVariant data ( const QModelIndex & index, int role ) const
 */
HB_FUNC( QT_QSTRINGLISTMODEL_DATA )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QVariant( hbqt_par_QStringListModel( 1 )->data( *hbqt_par_QModelIndex( 2 ), hb_parni( 3 ) ) ), release_QVariant ) );
}

/*
 * virtual Qt::ItemFlags flags ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QSTRINGLISTMODEL_FLAGS )
{
   hb_retni( ( Qt::ItemFlags ) hbqt_par_QStringListModel( 1 )->flags( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * virtual bool insertRows ( int row, int count, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QSTRINGLISTMODEL_INSERTROWS )
{
   hb_retl( hbqt_par_QStringListModel( 1 )->insertRows( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
}

/*
 * virtual bool removeRows ( int row, int count, const QModelIndex & parent = QModelIndex() )
 */
HB_FUNC( QT_QSTRINGLISTMODEL_REMOVEROWS )
{
   hb_retl( hbqt_par_QStringListModel( 1 )->removeRows( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) );
}

/*
 * virtual int rowCount ( const QModelIndex & parent = QModelIndex() ) const
 */
HB_FUNC( QT_QSTRINGLISTMODEL_ROWCOUNT )
{
   hb_retni( hbqt_par_QStringListModel( 1 )->rowCount( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QModelIndex( 2 ) : QModelIndex() ) ) );
}

/*
 * virtual bool setData ( const QModelIndex & index, const QVariant & value, int role = Qt::EditRole )
 */
HB_FUNC( QT_QSTRINGLISTMODEL_SETDATA )
{
   hb_retl( hbqt_par_QStringListModel( 1 )->setData( *hbqt_par_QModelIndex( 2 ), *hbqt_par_QVariant( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : Qt::EditRole ) ) );
}

/*
 * void setStringList ( const QStringList & strings )
 */
HB_FUNC( QT_QSTRINGLISTMODEL_SETSTRINGLIST )
{
   hbqt_par_QStringListModel( 1 )->setStringList( *hbqt_par_QStringList( 2 ) );
}

/*
 * QStringList stringList () const
 */
HB_FUNC( QT_QSTRINGLISTMODEL_STRINGLIST )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QStringList( hbqt_par_QStringListModel( 1 )->stringList() ), release_QStringList ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
