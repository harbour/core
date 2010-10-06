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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

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
   QSortFilterProxyModel  * ph = NULL ;
   HBQT_GC_T_QSortFilterProxyModel * p = ( HBQT_GC_T_QSortFilterProxyModel * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSortFilterProxyModel   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSortFilterProxyModel   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QSortFilterProxyModel          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QSortFilterProxyModel    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QSortFilterProxyModel    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSortFilterProxyModel( void * pObj, bool bNew )
{
   HBQT_GC_T_QSortFilterProxyModel * p = ( HBQT_GC_T_QSortFilterProxyModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSortFilterProxyModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSortFilterProxyModel >( ( QSortFilterProxyModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSortFilterProxyModel;
   p->type = HBQT_TYPE_QSortFilterProxyModel;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QSortFilterProxyModel  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QSortFilterProxyModel", pObj ) );
   }
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

/*
 * bool dynamicSortFilter () const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_DYNAMICSORTFILTER )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retl( ( p )->dynamicSortFilter() );
   }
}

/*
 * Qt::CaseSensitivity filterCaseSensitivity () const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_FILTERCASESENSITIVITY )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retni( ( Qt::CaseSensitivity ) ( p )->filterCaseSensitivity() );
   }
}

/*
 * int filterKeyColumn () const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_FILTERKEYCOLUMN )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retni( ( p )->filterKeyColumn() );
   }
}

/*
 * QRegExp filterRegExp () const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_FILTERREGEXP )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRegExp( new QRegExp( ( p )->filterRegExp() ), true ) );
   }
}

/*
 * int filterRole () const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_FILTERROLE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retni( ( p )->filterRole() );
   }
}

/*
 * bool isSortLocaleAware () const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_ISSORTLOCALEAWARE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retl( ( p )->isSortLocaleAware() );
   }
}

/*
 * virtual QModelIndex mapFromSource ( const QModelIndex & sourceIndex ) const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_MAPFROMSOURCE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mapFromSource( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   }
}

/*
 * virtual QModelIndex mapToSource ( const QModelIndex & proxyIndex ) const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_MAPTOSOURCE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mapToSource( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
   }
}

/*
 * void setDynamicSortFilter ( bool enable )
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETDYNAMICSORTFILTER )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      ( p )->setDynamicSortFilter( hb_parl( 2 ) );
   }
}

/*
 * void setFilterCaseSensitivity ( Qt::CaseSensitivity cs )
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETFILTERCASESENSITIVITY )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      ( p )->setFilterCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) );
   }
}

/*
 * void setFilterKeyColumn ( int column )
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETFILTERKEYCOLUMN )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      ( p )->setFilterKeyColumn( hb_parni( 2 ) );
   }
}

/*
 * void setFilterRegExp ( const QRegExp & regExp )
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETFILTERREGEXP )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      ( p )->setFilterRegExp( *hbqt_par_QRegExp( 2 ) );
   }
}

/*
 * void setFilterRole ( int role )
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETFILTERROLE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      ( p )->setFilterRole( hb_parni( 2 ) );
   }
}

/*
 * void setSortCaseSensitivity ( Qt::CaseSensitivity cs )
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETSORTCASESENSITIVITY )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      ( p )->setSortCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) );
   }
}

/*
 * void setSortLocaleAware ( bool on )
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETSORTLOCALEAWARE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      ( p )->setSortLocaleAware( hb_parl( 2 ) );
   }
}

/*
 * void setSortRole ( int role )
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SETSORTROLE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      ( p )->setSortRole( hb_parni( 2 ) );
   }
}

/*
 * Qt::CaseSensitivity sortCaseSensitivity () const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SORTCASESENSITIVITY )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retni( ( Qt::CaseSensitivity ) ( p )->sortCaseSensitivity() );
   }
}

/*
 * int sortColumn () const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SORTCOLUMN )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retni( ( p )->sortColumn() );
   }
}

/*
 * Qt::SortOrder sortOrder () const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SORTORDER )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retni( ( Qt::SortOrder ) ( p )->sortOrder() );
   }
}

/*
 * int sortRole () const
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_SORTROLE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      hb_retni( ( p )->sortRole() );
   }
}

/*
 * void invalidate ()
 */
HB_FUNC( QT_QSORTFILTERPROXYMODEL_INVALIDATE )
{
   QSortFilterProxyModel * p = hbqt_par_QSortFilterProxyModel( 1 );
   if( p )
   {
      ( p )->invalidate();
   }
}

/*
 * void setFilterFixedString ( const QString & pattern )
 */
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

/*
 * void setFilterRegExp ( const QString & pattern )
 */
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

/*
 * void setFilterWildcard ( const QString & pattern )
 */
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


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
