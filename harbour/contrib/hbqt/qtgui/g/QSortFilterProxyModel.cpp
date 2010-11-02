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
 *  Constructed[ 24/24 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSortFilterProxyModel>


/*
 * QSortFilterProxyModel ( QObject * parent = 0 )
 * ~QSortFilterProxyModel ()
 */

typedef struct
{
   QPointer< QSortFilterProxyModel > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSortFilterProxyModel;

HBQT_GC_FUNC( hbqt_gcRelease_QSortFilterProxyModel )
{
   HBQT_GC_T_QSortFilterProxyModel * p = ( HBQT_GC_T_QSortFilterProxyModel * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QSortFilterProxyModel * ph = p->ph;
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

void * hbqt_gcAllocate_QSortFilterProxyModel( void * pObj, bool bNew )
{
   HBQT_GC_T_QSortFilterProxyModel * p = ( HBQT_GC_T_QSortFilterProxyModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSortFilterProxyModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSortFilterProxyModel >( ( QSortFilterProxyModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSortFilterProxyModel;
   p->type = HBQT_TYPE_QSortFilterProxyModel;

   return p;
}

HB_FUNC( QT_QSORTFILTERPROXYMODEL )
{
   QSortFilterProxyModel * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QSortFilterProxyModel( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = new QSortFilterProxyModel() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QSortFilterProxyModel( ( void * ) pObj, true ) );
}

/* bool dynamicSortFilter () const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_DYNAMICSORTFILTER )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retl( ( p )->dynamicSortFilter() );
}

/* Qt::CaseSensitivity filterCaseSensitivity () const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_FILTERCASESENSITIVITY )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retni( ( Qt::CaseSensitivity ) ( p )->filterCaseSensitivity() );
}

/* int filterKeyColumn () const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_FILTERKEYCOLUMN )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retni( ( p )->filterKeyColumn() );
}

/* QRegExp filterRegExp () const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_FILTERREGEXP )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegExp( new QRegExp( ( p )->filterRegExp() ), true ) );
}

/* int filterRole () const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_FILTERROLE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retni( ( p )->filterRole() );
}

/* bool isSortLocaleAware () const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_ISSORTLOCALEAWARE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retl( ( p )->isSortLocaleAware() );
}

/* virtual QModelIndex mapFromSource ( const QModelIndex & sourceIndex ) const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_MAPFROMSOURCE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mapFromSource( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
}

/* virtual QModelIndex mapToSource ( const QModelIndex & proxyIndex ) const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_MAPTOSOURCE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mapToSource( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
}

/* void setDynamicSortFilter ( bool enable ) */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETDYNAMICSORTFILTER )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      ( p )->setDynamicSortFilter( hb_parl( 2 ) );
}

/* void setFilterCaseSensitivity ( Qt::CaseSensitivity cs ) */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETFILTERCASESENSITIVITY )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      ( p )->setFilterCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) );
}

/* void setFilterKeyColumn ( int column ) */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETFILTERKEYCOLUMN )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      ( p )->setFilterKeyColumn( hb_parni( 2 ) );
}

/* void setFilterRegExp ( const QRegExp & regExp ) */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETFILTERREGEXP )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      ( p )->setFilterRegExp( *hbqt_par_QRegExp( 2 ) );
}

/* void setFilterRole ( int role ) */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETFILTERROLE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      ( p )->setFilterRole( hb_parni( 2 ) );
}

/* void setSortCaseSensitivity ( Qt::CaseSensitivity cs ) */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETSORTCASESENSITIVITY )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      ( p )->setSortCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) );
}

/* void setSortLocaleAware ( bool on ) */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETSORTLOCALEAWARE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      ( p )->setSortLocaleAware( hb_parl( 2 ) );
}

/* void setSortRole ( int role ) */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETSORTROLE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      ( p )->setSortRole( hb_parni( 2 ) );
}

/* Qt::CaseSensitivity sortCaseSensitivity () const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SORTCASESENSITIVITY )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retni( ( Qt::CaseSensitivity ) ( p )->sortCaseSensitivity() );
}

/* int sortColumn () const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SORTCOLUMN )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retni( ( p )->sortColumn() );
}

/* Qt::SortOrder sortOrder () const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SORTORDER )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retni( ( Qt::SortOrder ) ( p )->sortOrder() );
}

/* int sortRole () const */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SORTROLE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      hb_retni( ( p )->sortRole() );
}

/* void invalidate () */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_INVALIDATE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
      ( p )->invalidate();
}

/* void setFilterFixedString ( const QString & pattern ) */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETFILTERFIXEDSTRING )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFilterFixedString( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setFilterRegExp ( const QString & pattern ) */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETFILTERREGEXP_1 )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFilterRegExp( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setFilterWildcard ( const QString & pattern ) */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETFILTERWILDCARD )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFilterWildcard( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
