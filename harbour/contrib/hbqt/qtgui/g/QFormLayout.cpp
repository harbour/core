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
   QPointer< QFormLayout > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFormLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QFormLayout )
{
   QFormLayout  * ph = NULL ;
   HBQT_GC_T_QFormLayout * p = ( HBQT_GC_T_QFormLayout * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFormLayout   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFormLayout   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QFormLayout          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFormLayout    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFormLayout    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFormLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QFormLayout * p = ( HBQT_GC_T_QFormLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QFormLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFormLayout >( ( QFormLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFormLayout;
   p->type = HBQT_TYPE_QFormLayout;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFormLayout  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFormLayout", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFORMLAYOUT )
{
   QFormLayout * pObj = NULL;

   pObj = ( QFormLayout * ) new QFormLayout( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QFormLayout( ( void * ) pObj, true ) );
}

/*
 * void addRow ( QWidget * label, QWidget * field )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->addRow( hbqt_par_QWidget( 2 ), hbqt_par_QWidget( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_ADDROW FP=( p )->addRow( hbqt_par_QWidget( 2 ), hbqt_par_QWidget( 3 ) ); p is NULL" ) );
   }
}

/*
 * void addRow ( QWidget * label, QLayout * field )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_1 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->addRow( hbqt_par_QWidget( 2 ), hbqt_par_QLayout( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_ADDROW_1 FP=( p )->addRow( hbqt_par_QWidget( 2 ), hbqt_par_QLayout( 3 ) ); p is NULL" ) );
   }
}

/*
 * void addRow ( QWidget * widget )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_2 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->addRow( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_ADDROW_2 FP=( p )->addRow( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void addRow ( const QString & labelText, QWidget * field )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_3 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->addRow( QFormLayout::tr( hb_parc( 2 ) ), hbqt_par_QWidget( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_ADDROW_3 FP=( p )->addRow( QFormLayout::tr( hb_parc( 2 ) ), hbqt_par_QWidget( 3 ) ); p is NULL" ) );
   }
}

/*
 * void addRow ( const QString & labelText, QLayout * field )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_4 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->addRow( QFormLayout::tr( hb_parc( 2 ) ), hbqt_par_QLayout( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_ADDROW_4 FP=( p )->addRow( QFormLayout::tr( hb_parc( 2 ) ), hbqt_par_QLayout( 3 ) ); p is NULL" ) );
   }
}

/*
 * void addRow ( QLayout * layout )
 */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_5 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->addRow( hbqt_par_QLayout( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_ADDROW_5 FP=( p )->addRow( hbqt_par_QLayout( 2 ) ); p is NULL" ) );
   }
}

/*
 * FieldGrowthPolicy fieldGrowthPolicy () const
 */
HB_FUNC( QT_QFORMLAYOUT_FIELDGROWTHPOLICY )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( QFormLayout::FieldGrowthPolicy ) ( p )->fieldGrowthPolicy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_FIELDGROWTHPOLICY FP=hb_retni( ( QFormLayout::FieldGrowthPolicy ) ( p )->fieldGrowthPolicy() ); p is NULL" ) );
   }
}

/*
 * Qt::Alignment formAlignment () const
 */
HB_FUNC( QT_QFORMLAYOUT_FORMALIGNMENT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->formAlignment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_FORMALIGNMENT FP=hb_retni( ( Qt::Alignment ) ( p )->formAlignment() ); p is NULL" ) );
   }
}

/*
 * void getItemPosition ( int index, int * rowPtr, ItemRole * rolePtr ) const
 */
HB_FUNC( QT_QFORMLAYOUT_GETITEMPOSITION )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   int iRowPtr = 0;
   QFormLayout::ItemRole iRolePtr = ( QFormLayout::ItemRole ) 0;

   if( p )
      ( p )->getItemPosition( hb_parni( 2 ), &iRowPtr, &iRolePtr );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_GETITEMPOSITION FP=( p )->getItemPosition( hb_parni( 2 ), &iRowPtr, &iRolePtr ); p is NULL" ) );
   }

   hb_storni( iRowPtr, 3 );
   hb_storni( iRolePtr, 4 );
}

/*
 * void getLayoutPosition ( QLayout * layout, int * rowPtr, ItemRole * rolePtr ) const
 */
HB_FUNC( QT_QFORMLAYOUT_GETLAYOUTPOSITION )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   int iRowPtr = 0;
   QFormLayout::ItemRole iRolePtr = ( QFormLayout::ItemRole ) 0;

   if( p )
      ( p )->getLayoutPosition( hbqt_par_QLayout( 2 ), &iRowPtr, &iRolePtr );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_GETLAYOUTPOSITION FP=( p )->getLayoutPosition( hbqt_par_QLayout( 2 ), &iRowPtr, &iRolePtr ); p is NULL" ) );
   }

   hb_storni( iRowPtr, 3 );
   hb_storni( iRolePtr, 4 );
}

/*
 * void getWidgetPosition ( QWidget * widget, int * rowPtr, ItemRole * rolePtr ) const
 */
HB_FUNC( QT_QFORMLAYOUT_GETWIDGETPOSITION )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   int iRowPtr = 0;
   QFormLayout::ItemRole iRolePtr = ( QFormLayout::ItemRole ) 0;

   if( p )
      ( p )->getWidgetPosition( hbqt_par_QWidget( 2 ), &iRowPtr, &iRolePtr );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_GETWIDGETPOSITION FP=( p )->getWidgetPosition( hbqt_par_QWidget( 2 ), &iRowPtr, &iRolePtr ); p is NULL" ) );
   }

   hb_storni( iRowPtr, 3 );
   hb_storni( iRolePtr, 4 );
}

/*
 * int horizontalSpacing () const
 */
HB_FUNC( QT_QFORMLAYOUT_HORIZONTALSPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( p )->horizontalSpacing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_HORIZONTALSPACING FP=hb_retni( ( p )->horizontalSpacing() ); p is NULL" ) );
   }
}

/*
 * void insertRow ( int row, QWidget * label, QWidget * field )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hbqt_par_QWidget( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_INSERTROW FP=( p )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hbqt_par_QWidget( 4 ) ); p is NULL" ) );
   }
}

/*
 * void insertRow ( int row, QWidget * label, QLayout * field )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_1 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hbqt_par_QLayout( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_INSERTROW_1 FP=( p )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hbqt_par_QLayout( 4 ) ); p is NULL" ) );
   }
}

/*
 * void insertRow ( int row, QWidget * widget )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_2 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_INSERTROW_2 FP=( p )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ) ); p is NULL" ) );
   }
}

/*
 * void insertRow ( int row, const QString & labelText, QWidget * field )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_3 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), QFormLayout::tr( hb_parc( 3 ) ), hbqt_par_QWidget( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_INSERTROW_3 FP=( p )->insertRow( hb_parni( 2 ), QFormLayout::tr( hb_parc( 3 ) ), hbqt_par_QWidget( 4 ) ); p is NULL" ) );
   }
}

/*
 * void insertRow ( int row, const QString & labelText, QLayout * field )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_4 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), QFormLayout::tr( hb_parc( 3 ) ), hbqt_par_QLayout( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_INSERTROW_4 FP=( p )->insertRow( hb_parni( 2 ), QFormLayout::tr( hb_parc( 3 ) ), hbqt_par_QLayout( 4 ) ); p is NULL" ) );
   }
}

/*
 * void insertRow ( int row, QLayout * layout )
 */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_5 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QLayout( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_INSERTROW_5 FP=( p )->insertRow( hb_parni( 2 ), hbqt_par_QLayout( 3 ) ); p is NULL" ) );
   }
}

/*
 * QLayoutItem * itemAt ( int row, ItemRole role ) const
 */
HB_FUNC( QT_QFORMLAYOUT_ITEMAT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLayoutItem( ( p )->itemAt( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_ITEMAT FP=hb_retptrGC( hbqt_gcAllocate_QLayoutItem( ( p )->itemAt( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * Qt::Alignment labelAlignment () const
 */
HB_FUNC( QT_QFORMLAYOUT_LABELALIGNMENT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->labelAlignment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_LABELALIGNMENT FP=hb_retni( ( Qt::Alignment ) ( p )->labelAlignment() ); p is NULL" ) );
   }
}

/*
 * QWidget * labelForField ( QWidget * field ) const
 */
HB_FUNC( QT_QFORMLAYOUT_LABELFORFIELD )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->labelForField( hbqt_par_QWidget( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_LABELFORFIELD FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->labelForField( hbqt_par_QWidget( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * labelForField ( QLayout * field ) const
 */
HB_FUNC( QT_QFORMLAYOUT_LABELFORFIELD_1 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->labelForField( hbqt_par_QLayout( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_LABELFORFIELD_1 FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->labelForField( hbqt_par_QLayout( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * int rowCount () const
 */
HB_FUNC( QT_QFORMLAYOUT_ROWCOUNT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( p )->rowCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_ROWCOUNT FP=hb_retni( ( p )->rowCount() ); p is NULL" ) );
   }
}

/*
 * RowWrapPolicy rowWrapPolicy () const
 */
HB_FUNC( QT_QFORMLAYOUT_ROWWRAPPOLICY )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( QFormLayout::RowWrapPolicy ) ( p )->rowWrapPolicy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_ROWWRAPPOLICY FP=hb_retni( ( QFormLayout::RowWrapPolicy ) ( p )->rowWrapPolicy() ); p is NULL" ) );
   }
}

/*
 * void setFieldGrowthPolicy ( FieldGrowthPolicy policy )
 */
HB_FUNC( QT_QFORMLAYOUT_SETFIELDGROWTHPOLICY )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setFieldGrowthPolicy( ( QFormLayout::FieldGrowthPolicy ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_SETFIELDGROWTHPOLICY FP=( p )->setFieldGrowthPolicy( ( QFormLayout::FieldGrowthPolicy ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFormAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QFORMLAYOUT_SETFORMALIGNMENT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setFormAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_SETFORMALIGNMENT FP=( p )->setFormAlignment( ( Qt::Alignment ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHorizontalSpacing ( int spacing )
 */
HB_FUNC( QT_QFORMLAYOUT_SETHORIZONTALSPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setHorizontalSpacing( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_SETHORIZONTALSPACING FP=( p )->setHorizontalSpacing( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setItem ( int row, ItemRole role, QLayoutItem * item )
 */
HB_FUNC( QT_QFORMLAYOUT_SETITEM )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setItem( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QLayoutItem( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_SETITEM FP=( p )->setItem( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QLayoutItem( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setLabelAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QFORMLAYOUT_SETLABELALIGNMENT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setLabelAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_SETLABELALIGNMENT FP=( p )->setLabelAlignment( ( Qt::Alignment ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLayout ( int row, ItemRole role, QLayout * layout )
 */
HB_FUNC( QT_QFORMLAYOUT_SETLAYOUT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setLayout( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QLayout( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_SETLAYOUT FP=( p )->setLayout( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QLayout( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setRowWrapPolicy ( RowWrapPolicy policy )
 */
HB_FUNC( QT_QFORMLAYOUT_SETROWWRAPPOLICY )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setRowWrapPolicy( ( QFormLayout::RowWrapPolicy ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_SETROWWRAPPOLICY FP=( p )->setRowWrapPolicy( ( QFormLayout::RowWrapPolicy ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSpacing ( int spacing )
 */
HB_FUNC( QT_QFORMLAYOUT_SETSPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setSpacing( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_SETSPACING FP=( p )->setSpacing( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setVerticalSpacing ( int spacing )
 */
HB_FUNC( QT_QFORMLAYOUT_SETVERTICALSPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setVerticalSpacing( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_SETVERTICALSPACING FP=( p )->setVerticalSpacing( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWidget ( int row, ItemRole role, QWidget * widget )
 */
HB_FUNC( QT_QFORMLAYOUT_SETWIDGET )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setWidget( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_SETWIDGET FP=( p )->setWidget( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QWidget( 4 ) ); p is NULL" ) );
   }
}

/*
 * int spacing () const
 */
HB_FUNC( QT_QFORMLAYOUT_SPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( p )->spacing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_SPACING FP=hb_retni( ( p )->spacing() ); p is NULL" ) );
   }
}

/*
 * int verticalSpacing () const
 */
HB_FUNC( QT_QFORMLAYOUT_VERTICALSPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( p )->verticalSpacing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFORMLAYOUT_VERTICALSPACING FP=hb_retni( ( p )->verticalSpacing() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
