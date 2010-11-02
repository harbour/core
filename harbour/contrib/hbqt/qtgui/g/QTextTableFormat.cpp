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
 *  Constructed[ 11/13 [ 84.62% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QVector<QTextLength> columnWidthConstraints () const
 *  void setColumnWidthConstraints ( const QVector<QTextLength> & constraints )
 */

#include <QtCore/QPointer>

#include <QtGui/QTextTableFormat>


/* QTextTableFormat ()
 */

typedef struct
{
   QTextTableFormat * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextTableFormat;

HBQT_GC_FUNC( hbqt_gcRelease_QTextTableFormat )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextTableFormat * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextTableFormat( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextTableFormat * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextTableFormat;
   p->type = HBQT_TYPE_QTextTableFormat;

   return p;
}

HB_FUNC( QT_QTEXTTABLEFORMAT )
{
   QTextTableFormat * pObj = NULL;

   pObj = new QTextTableFormat() ;

   hb_retptrGC( hbqt_gcAllocate_QTextTableFormat( ( void * ) pObj, true ) );
}

/* Qt::Alignment alignment () const */
HB_FUNC( QT_QTEXTTABLEFORMAT_ALIGNMENT )
{
   QTextTableFormat * p = hbqt_par_QTextTableFormat( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
}

/* qreal cellPadding () const */
HB_FUNC( QT_QTEXTTABLEFORMAT_CELLPADDING )
{
   QTextTableFormat * p = hbqt_par_QTextTableFormat( 1 );
   if( p )
      hb_retnd( ( p )->cellPadding() );
}

/* qreal cellSpacing () const */
HB_FUNC( QT_QTEXTTABLEFORMAT_CELLSPACING )
{
   QTextTableFormat * p = hbqt_par_QTextTableFormat( 1 );
   if( p )
      hb_retnd( ( p )->cellSpacing() );
}

/* void clearColumnWidthConstraints () */
HB_FUNC( QT_QTEXTTABLEFORMAT_CLEARCOLUMNWIDTHCONSTRAINTS )
{
   QTextTableFormat * p = hbqt_par_QTextTableFormat( 1 );
   if( p )
      ( p )->clearColumnWidthConstraints();
}

/* int columns () const */
HB_FUNC( QT_QTEXTTABLEFORMAT_COLUMNS )
{
   QTextTableFormat * p = hbqt_par_QTextTableFormat( 1 );
   if( p )
      hb_retni( ( p )->columns() );
}

/* int headerRowCount () const */
HB_FUNC( QT_QTEXTTABLEFORMAT_HEADERROWCOUNT )
{
   QTextTableFormat * p = hbqt_par_QTextTableFormat( 1 );
   if( p )
      hb_retni( ( p )->headerRowCount() );
}

/* bool isValid () const */
HB_FUNC( QT_QTEXTTABLEFORMAT_ISVALID )
{
   QTextTableFormat * p = hbqt_par_QTextTableFormat( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* void setAlignment ( Qt::Alignment alignment ) */
HB_FUNC( QT_QTEXTTABLEFORMAT_SETALIGNMENT )
{
   QTextTableFormat * p = hbqt_par_QTextTableFormat( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/* void setCellPadding ( qreal padding ) */
HB_FUNC( QT_QTEXTTABLEFORMAT_SETCELLPADDING )
{
   QTextTableFormat * p = hbqt_par_QTextTableFormat( 1 );
   if( p )
      ( p )->setCellPadding( hb_parnd( 2 ) );
}

/* void setCellSpacing ( qreal spacing ) */
HB_FUNC( QT_QTEXTTABLEFORMAT_SETCELLSPACING )
{
   QTextTableFormat * p = hbqt_par_QTextTableFormat( 1 );
   if( p )
      ( p )->setCellSpacing( hb_parnd( 2 ) );
}

/* void setHeaderRowCount ( int count ) */
HB_FUNC( QT_QTEXTTABLEFORMAT_SETHEADERROWCOUNT )
{
   QTextTableFormat * p = hbqt_par_QTextTableFormat( 1 );
   if( p )
      ( p )->setHeaderRowCount( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
