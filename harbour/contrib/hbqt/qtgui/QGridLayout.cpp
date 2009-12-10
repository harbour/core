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

#include <QtGui/QGridLayout>


/*
 * QGridLayout ( QWidget * parent )
 * QGridLayout ()
 * ~QGridLayout ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QGridLayout > pq;
} QGC_POINTER_QGridLayout;

QT_G_FUNC( release_QGridLayout )
{
   QGC_POINTER_QGridLayout * p = ( QGC_POINTER_QGridLayout * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QGridLayout                  p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QGridLayout                 ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QGridLayout * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QGridLayout * ) p->ph )->~QGridLayout();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QGridLayout * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QGridLayout                 Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QGridLayout                 Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QGridLayout                 Object Allready deleted!" ) );
   }
}

void * gcAllocate_QGridLayout( void * pObj )
{
   QGC_POINTER_QGridLayout * p = ( QGC_POINTER_QGridLayout * ) hb_gcAllocate( sizeof( QGC_POINTER_QGridLayout ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QGridLayout;
   new( & p->pq ) QPointer< QGridLayout >( ( QGridLayout * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QGridLayout                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QGRIDLAYOUT )
{
   void * pObj = NULL;

   pObj = new QGridLayout( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( gcAllocate_QGridLayout( pObj ) );
}
/*
 * void addItem ( QLayoutItem * item, int row, int column, int rowSpan = 1, int columnSpan = 1, Qt::Alignment alignment = 0 )
 */
HB_FUNC( QT_QGRIDLAYOUT_ADDITEM )
{
   hbqt_par_QGridLayout( 1 )->addItem( hbqt_par_QLayoutItem( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( HB_ISNUM( 5 ) ? hb_parni( 5 ) : 1 ), ( HB_ISNUM( 6 ) ? hb_parni( 6 ) : 1 ), ( Qt::Alignment ) hb_parni( 7 ) );
}

/*
 * void addLayout ( QLayout * layout, int row, int column, Qt::Alignment alignment = 0 )
 */
HB_FUNC( QT_QGRIDLAYOUT_ADDLAYOUT )
{
   hbqt_par_QGridLayout( 1 )->addLayout( hbqt_par_QLayout( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( Qt::Alignment ) hb_parni( 5 ) );
}

/*
 * void addLayout ( QLayout * layout, int row, int column, int rowSpan, int columnSpan, Qt::Alignment alignment = 0 )
 */
HB_FUNC( QT_QGRIDLAYOUT_ADDLAYOUT_1 )
{
   hbqt_par_QGridLayout( 1 )->addLayout( hbqt_par_QLayout( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), ( Qt::Alignment ) hb_parni( 7 ) );
}

/*
 * void addWidget ( QWidget * widget, int row, int column, Qt::Alignment alignment = 0 )
 */
HB_FUNC( QT_QGRIDLAYOUT_ADDWIDGET )
{
   hbqt_par_QGridLayout( 1 )->addWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( Qt::Alignment ) hb_parni( 5 ) );
}

/*
 * void addWidget ( QWidget * widget, int fromRow, int fromColumn, int rowSpan, int columnSpan, Qt::Alignment alignment = 0 )
 */
HB_FUNC( QT_QGRIDLAYOUT_ADDWIDGET_1 )
{
   hbqt_par_QGridLayout( 1 )->addWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), ( Qt::Alignment ) hb_parni( 7 ) );
}

/*
 * QRect cellRect ( int row, int column ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_CELLRECT )
{
   hb_retptrGC( gcAllocate_QRect( new QRect( hbqt_par_QGridLayout( 1 )->cellRect( hb_parni( 2 ), hb_parni( 3 ) ) ) ) );
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QGRIDLAYOUT_COLUMNCOUNT )
{
   hb_retni( hbqt_par_QGridLayout( 1 )->columnCount() );
}

/*
 * int columnMinimumWidth ( int column ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_COLUMNMINIMUMWIDTH )
{
   hb_retni( hbqt_par_QGridLayout( 1 )->columnMinimumWidth( hb_parni( 2 ) ) );
}

/*
 * int columnStretch ( int column ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_COLUMNSTRETCH )
{
   hb_retni( hbqt_par_QGridLayout( 1 )->columnStretch( hb_parni( 2 ) ) );
}

/*
 * void getItemPosition ( int index, int * row, int * column, int * rowSpan, int * columnSpan )
 */
HB_FUNC( QT_QGRIDLAYOUT_GETITEMPOSITION )
{
   int iRow = 0;
   int iColumn = 0;
   int iRowSpan = 0;
   int iColumnSpan = 0;

   hbqt_par_QGridLayout( 1 )->getItemPosition( hb_parni( 2 ), &iRow, &iColumn, &iRowSpan, &iColumnSpan );

   hb_storni( iRow, 3 );
   hb_storni( iColumn, 4 );
   hb_storni( iRowSpan, 5 );
   hb_storni( iColumnSpan, 6 );
}

/*
 * int horizontalSpacing () const
 */
HB_FUNC( QT_QGRIDLAYOUT_HORIZONTALSPACING )
{
   hb_retni( hbqt_par_QGridLayout( 1 )->horizontalSpacing() );
}

/*
 * QLayoutItem * itemAtPosition ( int row, int column ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_ITEMATPOSITION )
{
   hb_retptr( ( QLayoutItem* ) hbqt_par_QGridLayout( 1 )->itemAtPosition( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * Qt::Corner originCorner () const
 */
HB_FUNC( QT_QGRIDLAYOUT_ORIGINCORNER )
{
   hb_retni( ( Qt::Corner ) hbqt_par_QGridLayout( 1 )->originCorner() );
}

/*
 * int rowCount () const
 */
HB_FUNC( QT_QGRIDLAYOUT_ROWCOUNT )
{
   hb_retni( hbqt_par_QGridLayout( 1 )->rowCount() );
}

/*
 * int rowMinimumHeight ( int row ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_ROWMINIMUMHEIGHT )
{
   hb_retni( hbqt_par_QGridLayout( 1 )->rowMinimumHeight( hb_parni( 2 ) ) );
}

/*
 * int rowStretch ( int row ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_ROWSTRETCH )
{
   hb_retni( hbqt_par_QGridLayout( 1 )->rowStretch( hb_parni( 2 ) ) );
}

/*
 * void setColumnMinimumWidth ( int column, int minSize )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETCOLUMNMINIMUMWIDTH )
{
   hbqt_par_QGridLayout( 1 )->setColumnMinimumWidth( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setColumnStretch ( int column, int stretch )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETCOLUMNSTRETCH )
{
   hbqt_par_QGridLayout( 1 )->setColumnStretch( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setHorizontalSpacing ( int spacing )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETHORIZONTALSPACING )
{
   hbqt_par_QGridLayout( 1 )->setHorizontalSpacing( hb_parni( 2 ) );
}

/*
 * void setOriginCorner ( Qt::Corner corner )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETORIGINCORNER )
{
   hbqt_par_QGridLayout( 1 )->setOriginCorner( ( Qt::Corner ) hb_parni( 2 ) );
}

/*
 * void setRowMinimumHeight ( int row, int minSize )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETROWMINIMUMHEIGHT )
{
   hbqt_par_QGridLayout( 1 )->setRowMinimumHeight( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setRowStretch ( int row, int stretch )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETROWSTRETCH )
{
   hbqt_par_QGridLayout( 1 )->setRowStretch( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setSpacing ( int spacing )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETSPACING )
{
   hbqt_par_QGridLayout( 1 )->setSpacing( hb_parni( 2 ) );
}

/*
 * void setVerticalSpacing ( int spacing )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETVERTICALSPACING )
{
   hbqt_par_QGridLayout( 1 )->setVerticalSpacing( hb_parni( 2 ) );
}

/*
 * int spacing () const
 */
HB_FUNC( QT_QGRIDLAYOUT_SPACING )
{
   hb_retni( hbqt_par_QGridLayout( 1 )->spacing() );
}

/*
 * int verticalSpacing () const
 */
HB_FUNC( QT_QGRIDLAYOUT_VERTICALSPACING )
{
   hb_retni( hbqt_par_QGridLayout( 1 )->verticalSpacing() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
