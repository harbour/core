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

#include <QtCore/QSignalMapper>


/*
 * QSignalMapper ( QObject * parent = 0 )
 * ~QSignalMapper ()
 */

QT_G_FUNC( release_QSignalMapper )
{
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "release_QSignalMapper               %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QSignalMapper * ) ph )->~QSignalMapper();
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "  Object Name Missing: QSignalMapper" );  OutputDebugString( str );
#endif
      }
   }
   else
   {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "! ph____QSignalMapper" );  OutputDebugString( str );
#endif
   }
}

HB_FUNC( QT_QSIGNALMAPPER )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   QPointer< QSignalMapper > pObj = NULL;
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:  new QSignalMapper               %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif

   pObj = new QSignalMapper( hbqt_par_QObject( 1 ) ) ;

#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:                                  %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   p->ph = pObj;
   p->func = release_QSignalMapper;

   hb_retptrGC( p );
}
/*
 * QObject * mapping ( int id ) const
 */
HB_FUNC( QT_QSIGNALMAPPER_MAPPING )
{
   hb_retptr( ( QObject* ) hbqt_par_QSignalMapper( 1 )->mapping( hb_parni( 2 ) ) );
}

/*
 * QObject * mapping ( const QString & id ) const
 */
HB_FUNC( QT_QSIGNALMAPPER_MAPPING_1 )
{
   hb_retptr( ( QObject* ) hbqt_par_QSignalMapper( 1 )->mapping( hbqt_par_QString( 2 ) ) );
}

/*
 * QObject * mapping ( QWidget * widget ) const
 */
HB_FUNC( QT_QSIGNALMAPPER_MAPPING_2 )
{
   hb_retptr( ( QObject* ) hbqt_par_QSignalMapper( 1 )->mapping( hbqt_par_QWidget( 2 ) ) );
}

/*
 * QObject * mapping ( QObject * object ) const
 */
HB_FUNC( QT_QSIGNALMAPPER_MAPPING_3 )
{
   hb_retptr( ( QObject* ) hbqt_par_QSignalMapper( 1 )->mapping( hbqt_par_QObject( 2 ) ) );
}

/*
 * void removeMappings ( QObject * sender )
 */
HB_FUNC( QT_QSIGNALMAPPER_REMOVEMAPPINGS )
{
   hbqt_par_QSignalMapper( 1 )->removeMappings( hbqt_par_QObject( 2 ) );
}

/*
 * void setMapping ( QObject * sender, int id )
 */
HB_FUNC( QT_QSIGNALMAPPER_SETMAPPING )
{
   hbqt_par_QSignalMapper( 1 )->setMapping( hbqt_par_QObject( 2 ), hb_parni( 3 ) );
}

/*
 * void setMapping ( QObject * sender, const QString & text )
 */
HB_FUNC( QT_QSIGNALMAPPER_SETMAPPING_1 )
{
   hbqt_par_QSignalMapper( 1 )->setMapping( hbqt_par_QObject( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void setMapping ( QObject * sender, QWidget * widget )
 */
HB_FUNC( QT_QSIGNALMAPPER_SETMAPPING_2 )
{
   hbqt_par_QSignalMapper( 1 )->setMapping( hbqt_par_QObject( 2 ), hbqt_par_QWidget( 3 ) );
}

/*
 * void setMapping ( QObject * sender, QObject * object )
 */
HB_FUNC( QT_QSIGNALMAPPER_SETMAPPING_3 )
{
   hbqt_par_QSignalMapper( 1 )->setMapping( hbqt_par_QObject( 2 ), hbqt_par_QObject( 3 ) );
}

/*
 * void map ()
 */
HB_FUNC( QT_QSIGNALMAPPER_MAP )
{
   hbqt_par_QSignalMapper( 1 )->map();
}

/*
 * void map ( QObject * sender )
 */
HB_FUNC( QT_QSIGNALMAPPER_MAP_1 )
{
   hbqt_par_QSignalMapper( 1 )->map( hbqt_par_QObject( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
