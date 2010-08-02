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
 *  enum WritingSystem { Any, Latin, Greek, Cyrillic, ..., Runic }
 */

#include <QtCore/QPointer>

#include <QtCore/QStringList>
#include <QtGui/QFontDatabase>


/*
 * QFontDatabase ()
 */

typedef struct
{
   QFontDatabase * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QFontDatabase;

QT_G_FUNC( hbqt_gcRelease_QFontDatabase )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QFontDatabase   /.\\", p->ph ) );
         delete ( ( QFontDatabase * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QFontDatabase   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFontDatabase    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFontDatabase    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFontDatabase( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QFontDatabase * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontDatabase;
   p->type = HBQT_TYPE_QFontDatabase;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFontDatabase", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFontDatabase", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFONTDATABASE )
{
   QFontDatabase * pObj = NULL;

   pObj = new QFontDatabase() ;

   hb_retptrGC( hbqt_gcAllocate_QFontDatabase( ( void * ) pObj, true ) );
}

/*
 * bool bold ( const QString & family, const QString & style ) const
 */
HB_FUNC( QT_QFONTDATABASE_BOLD )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retl( ( p )->bold( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_BOLD FP=hb_retl( ( p )->bold( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QStringList families ( WritingSystem writingSystem = Any ) const
 */
HB_FUNC( QT_QFONTDATABASE_FAMILIES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->families( ( HB_ISNUM( 2 ) ? ( QFontDatabase::WritingSystem ) hb_parni( 2 ) : ( QFontDatabase::WritingSystem ) QFontDatabase::Any ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_FAMILIES FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->families( ( HB_ISNUM( 2 ) ? ( QFontDatabase::WritingSystem ) hb_parni( 2 ) : ( QFontDatabase::WritingSystem ) QFontDatabase::Any ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QFont font ( const QString & family, const QString & style, int pointSize ) const
 */
HB_FUNC( QT_QFONTDATABASE_FONT )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ), hb_parni( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ), hb_parni( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isBitmapScalable ( const QString & family, const QString & style = QString() ) const
 */
HB_FUNC( QT_QFONTDATABASE_ISBITMAPSCALABLE )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retl( ( p )->isBitmapScalable( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_ISBITMAPSCALABLE FP=hb_retl( ( p )->isBitmapScalable( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isFixedPitch ( const QString & family, const QString & style = QString() ) const
 */
HB_FUNC( QT_QFONTDATABASE_ISFIXEDPITCH )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retl( ( p )->isFixedPitch( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_ISFIXEDPITCH FP=hb_retl( ( p )->isFixedPitch( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isScalable ( const QString & family, const QString & style = QString() ) const
 */
HB_FUNC( QT_QFONTDATABASE_ISSCALABLE )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retl( ( p )->isScalable( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_ISSCALABLE FP=hb_retl( ( p )->isScalable( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isSmoothlyScalable ( const QString & family, const QString & style = QString() ) const
 */
HB_FUNC( QT_QFONTDATABASE_ISSMOOTHLYSCALABLE )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retl( ( p )->isSmoothlyScalable( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_ISSMOOTHLYSCALABLE FP=hb_retl( ( p )->isSmoothlyScalable( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool italic ( const QString & family, const QString & style ) const
 */
HB_FUNC( QT_QFONTDATABASE_ITALIC )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retl( ( p )->italic( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_ITALIC FP=hb_retl( ( p )->italic( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QList<int> pointSizes ( const QString & family, const QString & style = QString() )
 */
HB_FUNC( QT_QFONTDATABASE_POINTSIZES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->pointSizes( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_POINTSIZES FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->pointSizes( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<int> smoothSizes ( const QString & family, const QString & style )
 */
HB_FUNC( QT_QFONTDATABASE_SMOOTHSIZES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->smoothSizes( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_SMOOTHSIZES FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->smoothSizes( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QString styleString ( const QFont & font )
 */
HB_FUNC( QT_QFONTDATABASE_STYLESTRING )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retc( ( p )->styleString( *hbqt_par_QFont( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_STYLESTRING FP=hb_retc( ( p )->styleString( *hbqt_par_QFont( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString styleString ( const QFontInfo & fontInfo )
 */
HB_FUNC( QT_QFONTDATABASE_STYLESTRING_1 )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retc( ( p )->styleString( *hbqt_par_QFontInfo( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_STYLESTRING_1 FP=hb_retc( ( p )->styleString( *hbqt_par_QFontInfo( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStringList styles ( const QString & family ) const
 */
HB_FUNC( QT_QFONTDATABASE_STYLES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->styles( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_STYLES FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->styles( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int weight ( const QString & family, const QString & style ) const
 */
HB_FUNC( QT_QFONTDATABASE_WEIGHT )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retni( ( p )->weight( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_WEIGHT FP=hb_retni( ( p )->weight( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * int addApplicationFont ( const QString & fileName )
 */
HB_FUNC( QT_QFONTDATABASE_ADDAPPLICATIONFONT )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retni( ( p )->addApplicationFont( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_ADDAPPLICATIONFONT FP=hb_retni( ( p )->addApplicationFont( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int addApplicationFontFromData ( const QByteArray & fontData )
 */
HB_FUNC( QT_QFONTDATABASE_ADDAPPLICATIONFONTFROMDATA )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retni( ( p )->addApplicationFontFromData( *hbqt_par_QByteArray( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_ADDAPPLICATIONFONTFROMDATA FP=hb_retni( ( p )->addApplicationFontFromData( *hbqt_par_QByteArray( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QStringList applicationFontFamilies ( int id )
 */
HB_FUNC( QT_QFONTDATABASE_APPLICATIONFONTFAMILIES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->applicationFontFamilies( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_APPLICATIONFONTFAMILIES FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->applicationFontFamilies( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool removeAllApplicationFonts ()
 */
HB_FUNC( QT_QFONTDATABASE_REMOVEALLAPPLICATIONFONTS )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retl( ( p )->removeAllApplicationFonts() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_REMOVEALLAPPLICATIONFONTS FP=hb_retl( ( p )->removeAllApplicationFonts() ); p is NULL" ) );
   }
}

/*
 * bool removeApplicationFont ( int id )
 */
HB_FUNC( QT_QFONTDATABASE_REMOVEAPPLICATIONFONT )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retl( ( p )->removeApplicationFont( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_REMOVEAPPLICATIONFONT FP=hb_retl( ( p )->removeApplicationFont( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QList<int> standardSizes ()
 */
HB_FUNC( QT_QFONTDATABASE_STANDARDSIZES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->standardSizes() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_STANDARDSIZES FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->standardSizes() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool supportsThreadedFontRendering ()
 */
HB_FUNC( QT_QFONTDATABASE_SUPPORTSTHREADEDFONTRENDERING )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retl( ( p )->supportsThreadedFontRendering() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_SUPPORTSTHREADEDFONTRENDERING FP=hb_retl( ( p )->supportsThreadedFontRendering() ); p is NULL" ) );
   }
}

/*
 * QString writingSystemName ( WritingSystem writingSystem )
 */
HB_FUNC( QT_QFONTDATABASE_WRITINGSYSTEMNAME )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retc( ( p )->writingSystemName( ( QFontDatabase::WritingSystem ) hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_WRITINGSYSTEMNAME FP=hb_retc( ( p )->writingSystemName( ( QFontDatabase::WritingSystem ) hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString writingSystemSample ( WritingSystem writingSystem )
 */
HB_FUNC( QT_QFONTDATABASE_WRITINGSYSTEMSAMPLE )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
      hb_retc( ( p )->writingSystemSample( ( QFontDatabase::WritingSystem ) hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTDATABASE_WRITINGSYSTEMSAMPLE FP=hb_retc( ( p )->writingSystemSample( ( QFontDatabase::WritingSystem ) hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
