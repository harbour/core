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
 *  enum FieldGrowthPolicy { FieldsStayAtSizeHint, ExpandingFieldsGrow, AllNonFixedFieldsGrow }
 *  enum ItemRole { LabelRole, FieldRole, SpanningRole }
 *  enum RowWrapPolicy { DontWrapRows, WrapLongRows, WrapAllRows }
 */

#include <QtCore/QPointer>

#include <QtGui/QFormLayout>


/*
 * QFormLayout ( QWidget * parent = 0 )
 * ~QFormLayout ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QFormLayout > pq;
} QGC_POINTER_QFormLayout;

QT_G_FUNC( hbqt_gcRelease_QFormLayout )
{
   QGC_POINTER_QFormLayout * p = ( QGC_POINTER_QFormLayout * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QFormLayout                  p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QFormLayout                 ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QFormLayout * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QFormLayout * ) p->ph )->~QFormLayout();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QFormLayout * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QFormLayout                 Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO hbqt_gcRelease_QFormLayout                 Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QFormLayout                 Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QFormLayout( void * pObj )
{
   QGC_POINTER_QFormLayout * p = ( QGC_POINTER_QFormLayout * ) hb_gcAllocate( sizeof( QGC_POINTER_QFormLayout ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QFormLayout;
   new( & p->pq ) QPointer< QFormLayout >( ( QFormLayout * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QFormLayout                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QFORMLAYOUT )
{
   void * pObj = NULL;

   pObj = ( QFormLayout * ) new QFormLayout( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QFormLayout( pObj ) );
}
/*
 * void addRow ( QWidget * label, QWidget * field )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW )
{
   hbqt_par_QFormLayout( 1 )->addRow( hbqt_par_QWidget( 2 ), hbqt_par_QWidget( 3 ) );
}

/*
 * void addRow ( QWidget * label, QLayout * field )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_1 )
{
   hbqt_par_QFormLayout( 1 )->addRow( hbqt_par_QWidget( 2 ), hbqt_par_QLayout( 3 ) );
}

/*
 * void addRow ( QWidget * widget )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_2 )
{
   hbqt_par_QFormLayout( 1 )->addRow( hbqt_par_QWidget( 2 ) );
}

/*
 * void addRow ( const QString & labelText, QWidget * field )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_3 )
{
   hbqt_par_QFormLayout( 1 )->addRow( QFormLayout::tr( hb_parc( 2 ) ), hbqt_par_QWidget( 3 ) );
}

/*
 * void addRow ( const QString & labelText, QLayout * field )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_4 )
{
   hbqt_par_QFormLayout( 1 )->addRow( QFormLayout::tr( hb_parc( 2 ) ), hbqt_par_QLayout( 3 ) );
}

/*
 * void addRow ( QLayout * layout )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_5 )
{
   hbqt_par_QFormLayout( 1 )->addRow( hbqt_par_QLayout( 2 ) );
}

/*
 * FieldGrowthPolicy fieldGrowthPolicy () const
 */
HB_FUNC( QT_QFORMLAYOUT_FIELDGROWTHPOLICY )
{
   hb_retni( ( QFormLayout::FieldGrowthPolicy ) hbqt_par_QFormLayout( 1 )->fieldGrowthPolicy() );
}

/*
 * Qt::Alignment formAlignment () const
 */
HB_FUNC( QT_QFORMLAYOUT_FORMALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QFormLayout( 1 )->formAlignment() );
}

/*
 * void getItemPosition ( int index, int * rowPtr, ItemRole * rolePtr ) const
 */
HB_FUNC( QT_QFORMLAYOUT_GETITEMPOSITION )
{
   int iRowPtr = 0;
   QFormLayout::ItemRole iRolePtr;

   hbqt_par_QFormLayout( 1 )->getItemPosition( hb_parni( 2 ), &iRowPtr, &iRolePtr );

   hb_storni( iRowPtr, 3 );
   hb_storni( iRolePtr, 4 );
}

/*
 * void getLayoutPosition ( QLayout * layout, int * rowPtr, ItemRole * rolePtr ) const
 */
HB_FUNC( QT_QFORMLAYOUT_GETLAYOUTPOSITION )
{
   int iRowPtr = 0;
   QFormLayout::ItemRole iRolePtr;

   hbqt_par_QFormLayout( 1 )->getLayoutPosition( hbqt_par_QLayout( 2 ), &iRowPtr, &iRolePtr );

   hb_storni( iRowPtr, 3 );
   hb_storni( iRolePtr, 4 );
}

/*
 * void getWidgetPosition ( QWidget * widget, int * rowPtr, ItemRole * rolePtr ) const
 */
HB_FUNC( QT_QFORMLAYOUT_GETWIDGETPOSITION )
{
   int iRowPtr = 0;
   QFormLayout::ItemRole iRolePtr;

   hbqt_par_QFormLayout( 1 )->getWidgetPosition( hbqt_par_QWidget( 2 ), &iRowPtr, &iRolePtr );

   hb_storni( iRowPtr, 3 );
   hb_storni( iRolePtr, 4 );
}

/*
 * int horizontalSpacing () const
 */
HB_FUNC( QT_QFORMLAYOUT_HORIZONTALSPACING )
{
   hb_retni( hbqt_par_QFormLayout( 1 )->horizontalSpacing() );
}

/*
 * void insertRow ( int row, QWidget * label, QWidget * field )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW )
{
   hbqt_par_QFormLayout( 1 )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hbqt_par_QWidget( 4 ) );
}

/*
 * void insertRow ( int row, QWidget * label, QLayout * field )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_1 )
{
   hbqt_par_QFormLayout( 1 )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hbqt_par_QLayout( 4 ) );
}

/*
 * void insertRow ( int row, QWidget * widget )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_2 )
{
   hbqt_par_QFormLayout( 1 )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ) );
}

/*
 * void insertRow ( int row, const QString & labelText, QWidget * field )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_3 )
{
   hbqt_par_QFormLayout( 1 )->insertRow( hb_parni( 2 ), QFormLayout::tr( hb_parc( 3 ) ), hbqt_par_QWidget( 4 ) );
}

/*
 * void insertRow ( int row, const QString & labelText, QLayout * field )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_4 )
{
   hbqt_par_QFormLayout( 1 )->insertRow( hb_parni( 2 ), QFormLayout::tr( hb_parc( 3 ) ), hbqt_par_QLayout( 4 ) );
}

/*
 * void insertRow ( int row, QLayout * layout )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_5 )
{
   hbqt_par_QFormLayout( 1 )->insertRow( hb_parni( 2 ), hbqt_par_QLayout( 3 ) );
}

/*
 * QLayoutItem * itemAt ( int row, ItemRole role ) const
 */
HB_FUNC( QT_QFORMLAYOUT_ITEMAT )
{
   hb_retptr( ( QLayoutItem* ) hbqt_par_QFormLayout( 1 )->itemAt( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ) ) );
}

/*
 * Qt::Alignment labelAlignment () const
 */
HB_FUNC( QT_QFORMLAYOUT_LABELALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QFormLayout( 1 )->labelAlignment() );
}

/*
 * QWidget * labelForField ( QWidget * field ) const
 */
HB_FUNC( QT_QFORMLAYOUT_LABELFORFIELD )
{
   hb_retptr( ( QWidget* ) hbqt_par_QFormLayout( 1 )->labelForField( hbqt_par_QWidget( 2 ) ) );
}

/*
 * QWidget * labelForField ( QLayout * field ) const
 */
HB_FUNC( QT_QFORMLAYOUT_LABELFORFIELD_1 )
{
   hb_retptr( ( QWidget* ) hbqt_par_QFormLayout( 1 )->labelForField( hbqt_par_QLayout( 2 ) ) );
}

/*
 * int rowCount () const
 */
HB_FUNC( QT_QFORMLAYOUT_ROWCOUNT )
{
   hb_retni( hbqt_par_QFormLayout( 1 )->rowCount() );
}

/*
 * RowWrapPolicy rowWrapPolicy () const
 */
HB_FUNC( QT_QFORMLAYOUT_ROWWRAPPOLICY )
{
   hb_retni( ( QFormLayout::RowWrapPolicy ) hbqt_par_QFormLayout( 1 )->rowWrapPolicy() );
}

/*
 * void setFieldGrowthPolicy ( FieldGrowthPolicy policy )
 */
HB_FUNC( QT_QFORMLAYOUT_SETFIELDGROWTHPOLICY )
{
   hbqt_par_QFormLayout( 1 )->setFieldGrowthPolicy( ( QFormLayout::FieldGrowthPolicy ) hb_parni( 2 ) );
}

/*
 * void setFormAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QFORMLAYOUT_SETFORMALIGNMENT )
{
   hbqt_par_QFormLayout( 1 )->setFormAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
 * void setHorizontalSpacing ( int spacing )
 */
HB_FUNC( QT_QFORMLAYOUT_SETHORIZONTALSPACING )
{
   hbqt_par_QFormLayout( 1 )->setHorizontalSpacing( hb_parni( 2 ) );
}

/*
 * void setItem ( int row, ItemRole role, QLayoutItem * item )
 */
HB_FUNC( QT_QFORMLAYOUT_SETITEM )
{
   hbqt_par_QFormLayout( 1 )->setItem( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QLayoutItem( 4 ) );
}

/*
 * void setLabelAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QFORMLAYOUT_SETLABELALIGNMENT )
{
   hbqt_par_QFormLayout( 1 )->setLabelAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
 * void setLayout ( int row, ItemRole role, QLayout * layout )
 */
HB_FUNC( QT_QFORMLAYOUT_SETLAYOUT )
{
   hbqt_par_QFormLayout( 1 )->setLayout( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QLayout( 4 ) );
}

/*
 * void setRowWrapPolicy ( RowWrapPolicy policy )
 */
HB_FUNC( QT_QFORMLAYOUT_SETROWWRAPPOLICY )
{
   hbqt_par_QFormLayout( 1 )->setRowWrapPolicy( ( QFormLayout::RowWrapPolicy ) hb_parni( 2 ) );
}

/*
 * void setSpacing ( int spacing )
 */
HB_FUNC( QT_QFORMLAYOUT_SETSPACING )
{
   hbqt_par_QFormLayout( 1 )->setSpacing( hb_parni( 2 ) );
}

/*
 * void setVerticalSpacing ( int spacing )
 */
HB_FUNC( QT_QFORMLAYOUT_SETVERTICALSPACING )
{
   hbqt_par_QFormLayout( 1 )->setVerticalSpacing( hb_parni( 2 ) );
}

/*
 * void setWidget ( int row, ItemRole role, QWidget * widget )
 */
HB_FUNC( QT_QFORMLAYOUT_SETWIDGET )
{
   hbqt_par_QFormLayout( 1 )->setWidget( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
}

/*
 * int spacing () const
 */
HB_FUNC( QT_QFORMLAYOUT_SPACING )
{
   hb_retni( hbqt_par_QFormLayout( 1 )->spacing() );
}

/*
 * int verticalSpacing () const
 */
HB_FUNC( QT_QFORMLAYOUT_VERTICALSPACING )
{
   hb_retni( hbqt_par_QFormLayout( 1 )->verticalSpacing() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
