/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  enum FieldGrowthPolicy { FieldsStayAtSizeHint, ExpandingFieldsGrow, AllNonFixedFieldsGrow }
 *  enum ItemRole { LabelRole, FieldRole, SpanningRole }
 *  enum RowWrapPolicy { DontWrapRows, WrapLongRows, WrapAllRows }
 */

/*
 *  Constructed[ 33/33 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //void addRow ( QWidget * label, QLayout * field )
 *  //void addRow ( const QString & labelText, QLayout * field )
 *  //void addRow ( QLayout * layout )
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
   HBQT_GC_T_QFormLayout * p = ( HBQT_GC_T_QFormLayout * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QFormLayout * ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QFormLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QFormLayout * p = ( HBQT_GC_T_QFormLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QFormLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFormLayout >( ( QFormLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFormLayout;
   p->type = HBQT_TYPE_QFormLayout;

   return p;
}

HB_FUNC( QT_QFORMLAYOUT )
{
   QFormLayout * pObj = NULL;

   pObj = new QFormLayout( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QFormLayout( ( void * ) pObj, true ) );
}

/* void addRow ( QWidget * label, QWidget * field ) */
HB_FUNC( QT_QFORMLAYOUT_ADDROW )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->addRow( hbqt_par_QWidget( 2 ), hbqt_par_QWidget( 3 ) );
}

/* void addRow ( QWidget * widget ) */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_1 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->addRow( hbqt_par_QWidget( 2 ) );
}

/* void addRow ( const QString & labelText, QWidget * field ) */
HB_FUNC( QT_QFORMLAYOUT_ADDROW_2 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
   {
      void * pText;
      ( p )->addRow( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QWidget( 3 ) );
      hb_strfree( pText );
   }
}

/* FieldGrowthPolicy fieldGrowthPolicy () const */
HB_FUNC( QT_QFORMLAYOUT_FIELDGROWTHPOLICY )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( QFormLayout::FieldGrowthPolicy ) ( p )->fieldGrowthPolicy() );
}

/* Qt::Alignment formAlignment () const */
HB_FUNC( QT_QFORMLAYOUT_FORMALIGNMENT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->formAlignment() );
}

/* void getItemPosition ( int index, int * rowPtr, ItemRole * rolePtr ) const */
HB_FUNC( QT_QFORMLAYOUT_GETITEMPOSITION )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   int iRowPtr = 0;
   QFormLayout::ItemRole iRolePtr = ( QFormLayout::ItemRole ) 0;

   if( p )
      ( p )->getItemPosition( hb_parni( 2 ), &iRowPtr, &iRolePtr );

   hb_storni( iRowPtr, 3 );
   hb_storni( iRolePtr, 4 );
}

/* void getLayoutPosition ( QLayout * layout, int * rowPtr, ItemRole * rolePtr ) const */
HB_FUNC( QT_QFORMLAYOUT_GETLAYOUTPOSITION )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   int iRowPtr = 0;
   QFormLayout::ItemRole iRolePtr = ( QFormLayout::ItemRole ) 0;

   if( p )
      ( p )->getLayoutPosition( hbqt_par_QLayout( 2 ), &iRowPtr, &iRolePtr );

   hb_storni( iRowPtr, 3 );
   hb_storni( iRolePtr, 4 );
}

/* void getWidgetPosition ( QWidget * widget, int * rowPtr, ItemRole * rolePtr ) const */
HB_FUNC( QT_QFORMLAYOUT_GETWIDGETPOSITION )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   int iRowPtr = 0;
   QFormLayout::ItemRole iRolePtr = ( QFormLayout::ItemRole ) 0;

   if( p )
      ( p )->getWidgetPosition( hbqt_par_QWidget( 2 ), &iRowPtr, &iRolePtr );

   hb_storni( iRowPtr, 3 );
   hb_storni( iRolePtr, 4 );
}

/* int horizontalSpacing () const */
HB_FUNC( QT_QFORMLAYOUT_HORIZONTALSPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( p )->horizontalSpacing() );
}

/* void insertRow ( int row, QWidget * label, QWidget * field ) */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hbqt_par_QWidget( 4 ) );
}

/* void insertRow ( int row, QWidget * label, QLayout * field ) */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_1 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hbqt_par_QLayout( 4 ) );
}

/* void insertRow ( int row, QWidget * widget ) */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_2 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QWidget( 3 ) );
}

/* void insertRow ( int row, const QString & labelText, QWidget * field ) */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_3 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertRow( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hbqt_par_QWidget( 4 ) );
      hb_strfree( pText );
   }
}

/* void insertRow ( int row, const QString & labelText, QLayout * field ) */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_4 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertRow( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hbqt_par_QLayout( 4 ) );
      hb_strfree( pText );
   }
}

/* void insertRow ( int row, QLayout * layout ) */
HB_FUNC( QT_QFORMLAYOUT_INSERTROW_5 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QLayout( 3 ) );
}

/* QLayoutItem * itemAt ( int row, ItemRole role ) const */
HB_FUNC( QT_QFORMLAYOUT_ITEMAT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLayoutItem( ( p )->itemAt( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ) ), false ) );
}

/* Qt::Alignment labelAlignment () const */
HB_FUNC( QT_QFORMLAYOUT_LABELALIGNMENT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->labelAlignment() );
}

/* QWidget * labelForField ( QWidget * field ) const */
HB_FUNC( QT_QFORMLAYOUT_LABELFORFIELD )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->labelForField( hbqt_par_QWidget( 2 ) ), false ) );
}

/* QWidget * labelForField ( QLayout * field ) const */
HB_FUNC( QT_QFORMLAYOUT_LABELFORFIELD_1 )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->labelForField( hbqt_par_QLayout( 2 ) ), false ) );
}

/* int rowCount () const */
HB_FUNC( QT_QFORMLAYOUT_ROWCOUNT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( p )->rowCount() );
}

/* RowWrapPolicy rowWrapPolicy () const */
HB_FUNC( QT_QFORMLAYOUT_ROWWRAPPOLICY )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( QFormLayout::RowWrapPolicy ) ( p )->rowWrapPolicy() );
}

/* void setFieldGrowthPolicy ( FieldGrowthPolicy policy ) */
HB_FUNC( QT_QFORMLAYOUT_SETFIELDGROWTHPOLICY )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setFieldGrowthPolicy( ( QFormLayout::FieldGrowthPolicy ) hb_parni( 2 ) );
}

/* void setFormAlignment ( Qt::Alignment alignment ) */
HB_FUNC( QT_QFORMLAYOUT_SETFORMALIGNMENT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setFormAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/* void setHorizontalSpacing ( int spacing ) */
HB_FUNC( QT_QFORMLAYOUT_SETHORIZONTALSPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setHorizontalSpacing( hb_parni( 2 ) );
}

/* void setItem ( int row, ItemRole role, QLayoutItem * item ) */
HB_FUNC( QT_QFORMLAYOUT_SETITEM )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setItem( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QLayoutItem( 4 ) );
}

/* void setLabelAlignment ( Qt::Alignment alignment ) */
HB_FUNC( QT_QFORMLAYOUT_SETLABELALIGNMENT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setLabelAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/* void setLayout ( int row, ItemRole role, QLayout * layout ) */
HB_FUNC( QT_QFORMLAYOUT_SETLAYOUT )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setLayout( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QLayout( 4 ) );
}

/* void setRowWrapPolicy ( RowWrapPolicy policy ) */
HB_FUNC( QT_QFORMLAYOUT_SETROWWRAPPOLICY )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setRowWrapPolicy( ( QFormLayout::RowWrapPolicy ) hb_parni( 2 ) );
}

/* void setSpacing ( int spacing ) */
HB_FUNC( QT_QFORMLAYOUT_SETSPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setSpacing( hb_parni( 2 ) );
}

/* void setVerticalSpacing ( int spacing ) */
HB_FUNC( QT_QFORMLAYOUT_SETVERTICALSPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setVerticalSpacing( hb_parni( 2 ) );
}

/* void setWidget ( int row, ItemRole role, QWidget * widget ) */
HB_FUNC( QT_QFORMLAYOUT_SETWIDGET )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      ( p )->setWidget( hb_parni( 2 ), ( QFormLayout::ItemRole ) hb_parni( 3 ), hbqt_par_QWidget( 4 ) );
}

/* int spacing () const */
HB_FUNC( QT_QFORMLAYOUT_SPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( p )->spacing() );
}

/* int verticalSpacing () const */
HB_FUNC( QT_QFORMLAYOUT_VERTICALSPACING )
{
   QFormLayout * p = hbqt_par_QFormLayout( 1 );
   if( p )
      hb_retni( ( p )->verticalSpacing() );
}


#endif /* #if QT_VERSION >= 0x040500 */
