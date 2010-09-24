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

/*
 *  Constructed[ 23/23 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QList<WritingSystem> writingSystems () const
 *  //QList<WritingSystem> writingSystems ( const QString & family ) const
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFontDatabase;

HBQT_GC_FUNC( hbqt_gcRelease_QFontDatabase )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

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
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

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
   {
      void * pText;
      hb_retl( ( p )->bold( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * QStringList families ( WritingSystem writingSystem = Any ) const
 */
HB_FUNC( QT_QFONTDATABASE_FAMILIES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->families( ( HB_ISNUM( 2 ) ? ( QFontDatabase::WritingSystem ) hb_parni( 2 ) : ( QFontDatabase::WritingSystem ) QFontDatabase::Any ) ) ), true ) );
   }
}

/*
 * QFont font ( const QString & family, const QString & style, int pointSize ) const
 */
HB_FUNC( QT_QFONTDATABASE_FONT )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), hb_parni( 4 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * bool isBitmapScalable ( const QString & family, const QString & style = QString() ) const
 */
HB_FUNC( QT_QFONTDATABASE_ISBITMAPSCALABLE )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->isBitmapScalable( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool isFixedPitch ( const QString & family, const QString & style = QString() ) const
 */
HB_FUNC( QT_QFONTDATABASE_ISFIXEDPITCH )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->isFixedPitch( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool isScalable ( const QString & family, const QString & style = QString() ) const
 */
HB_FUNC( QT_QFONTDATABASE_ISSCALABLE )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->isScalable( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool isSmoothlyScalable ( const QString & family, const QString & style = QString() ) const
 */
HB_FUNC( QT_QFONTDATABASE_ISSMOOTHLYSCALABLE )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->isSmoothlyScalable( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool italic ( const QString & family, const QString & style ) const
 */
HB_FUNC( QT_QFONTDATABASE_ITALIC )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->italic( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * QList<int> pointSizes ( const QString & family, const QString & style = QString() )
 */
HB_FUNC( QT_QFONTDATABASE_POINTSIZES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->pointSizes( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QList<int> smoothSizes ( const QString & family, const QString & style )
 */
HB_FUNC( QT_QFONTDATABASE_SMOOTHSIZES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->smoothSizes( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QString styleString ( const QFont & font )
 */
HB_FUNC( QT_QFONTDATABASE_STYLESTRING )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->styleString( *hbqt_par_QFont( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString styleString ( const QFontInfo & fontInfo )
 */
HB_FUNC( QT_QFONTDATABASE_STYLESTRING_1 )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->styleString( *hbqt_par_QFontInfo( 2 ) ).toUtf8().data() );
   }
}

/*
 * QStringList styles ( const QString & family ) const
 */
HB_FUNC( QT_QFONTDATABASE_STYLES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->styles( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * int weight ( const QString & family, const QString & style ) const
 */
HB_FUNC( QT_QFONTDATABASE_WEIGHT )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->weight( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * int addApplicationFont ( const QString & fileName )
 */
HB_FUNC( QT_QFONTDATABASE_ADDAPPLICATIONFONT )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->addApplicationFont( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * int addApplicationFontFromData ( const QByteArray & fontData )
 */
HB_FUNC( QT_QFONTDATABASE_ADDAPPLICATIONFONTFROMDATA )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      hb_retni( ( p )->addApplicationFontFromData( *hbqt_par_QByteArray( 2 ) ) );
   }
}

/*
 * QStringList applicationFontFamilies ( int id )
 */
HB_FUNC( QT_QFONTDATABASE_APPLICATIONFONTFAMILIES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->applicationFontFamilies( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * bool removeAllApplicationFonts ()
 */
HB_FUNC( QT_QFONTDATABASE_REMOVEALLAPPLICATIONFONTS )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      hb_retl( ( p )->removeAllApplicationFonts() );
   }
}

/*
 * bool removeApplicationFont ( int id )
 */
HB_FUNC( QT_QFONTDATABASE_REMOVEAPPLICATIONFONT )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      hb_retl( ( p )->removeApplicationFont( hb_parni( 2 ) ) );
   }
}

/*
 * QList<int> standardSizes ()
 */
HB_FUNC( QT_QFONTDATABASE_STANDARDSIZES )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<int>( ( p )->standardSizes() ), true ) );
   }
}

/*
 * bool supportsThreadedFontRendering ()
 */
HB_FUNC( QT_QFONTDATABASE_SUPPORTSTHREADEDFONTRENDERING )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      hb_retl( ( p )->supportsThreadedFontRendering() );
   }
}

/*
 * QString writingSystemName ( WritingSystem writingSystem )
 */
HB_FUNC( QT_QFONTDATABASE_WRITINGSYSTEMNAME )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->writingSystemName( ( QFontDatabase::WritingSystem ) hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString writingSystemSample ( WritingSystem writingSystem )
 */
HB_FUNC( QT_QFONTDATABASE_WRITINGSYSTEMSAMPLE )
{
   QFontDatabase * p = hbqt_par_QFontDatabase( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->writingSystemSample( ( QFontDatabase::WritingSystem ) hb_parni( 2 ) ).toUtf8().data() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
