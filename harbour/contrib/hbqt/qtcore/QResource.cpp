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

#include "../hbqt.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtCore/QResource>


/*
 * QResource ( const QString & file = QString(), const QLocale & locale = QLocale() )
 * ~QResource ()
 */

typedef struct
{
   QResource * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QResource;

QT_G_FUNC( hbqt_gcRelease_QResource )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QResource   /.\\", p->ph ) );
         delete ( ( QResource * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QResource   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QResource    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QResource    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QResource( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QResource * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QResource;
   p->type = HBQT_TYPE_QResource;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QResource", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QResource", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QRESOURCE )
{
   QResource * pObj = NULL;

   pObj =  new QResource() ;

   hb_retptrGC( hbqt_gcAllocate_QResource( ( void * ) pObj, true ) );
}

/*
 * QString absoluteFilePath () const
 */
HB_FUNC( QT_QRESOURCE_ABSOLUTEFILEPATH )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retc( ( p )->absoluteFilePath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_ABSOLUTEFILEPATH FP=hb_retc( ( p )->absoluteFilePath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * const uchar * data () const
 */
HB_FUNC( QT_QRESOURCE_DATA )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retc( ( const char * ) ( p )->data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_DATA FP=hb_retc( ( const char * ) ( p )->data() ); p is NULL" ) );
   }
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QRESOURCE_FILENAME )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retc( ( p )->fileName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_FILENAME FP=hb_retc( ( p )->fileName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool isCompressed () const
 */
HB_FUNC( QT_QRESOURCE_ISCOMPRESSED )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retl( ( p )->isCompressed() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_ISCOMPRESSED FP=hb_retl( ( p )->isCompressed() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QRESOURCE_ISVALID )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * QLocale locale () const
 */
HB_FUNC( QT_QRESOURCE_LOCALE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->locale() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_LOCALE FP=hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->locale() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setFileName ( const QString & file )
 */
HB_FUNC( QT_QRESOURCE_SETFILENAME )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      ( p )->setFileName( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_SETFILENAME FP=( p )->setFileName( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLocale ( const QLocale & locale )
 */
HB_FUNC( QT_QRESOURCE_SETLOCALE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      ( p )->setLocale( *hbqt_par_QLocale( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_SETLOCALE FP=( p )->setLocale( *hbqt_par_QLocale( 2 ) ); p is NULL" ) );
   }
}

/*
 * qint64 size () const
 */
HB_FUNC( QT_QRESOURCE_SIZE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retnint( ( p )->size() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_SIZE FP=hb_retnint( ( p )->size() ); p is NULL" ) );
   }
}

/*
 * bool registerResource ( const QString & rccFileName, const QString & mapRoot = QString() )
 */
HB_FUNC( QT_QRESOURCE_REGISTERRESOURCE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retl( ( p )->registerResource( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_REGISTERRESOURCE FP=hb_retl( ( p )->registerResource( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool registerResource ( const uchar * rccData, const QString & mapRoot = QString() )
 */
HB_FUNC( QT_QRESOURCE_REGISTERRESOURCE_1 )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retl( ( p )->registerResource( hbqt_par_uchar( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_REGISTERRESOURCE_1 FP=hb_retl( ( p )->registerResource( hbqt_par_uchar( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QStringList searchPaths ()
 */
HB_FUNC( QT_QRESOURCE_SEARCHPATHS )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->searchPaths() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_SEARCHPATHS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->searchPaths() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool unregisterResource ( const QString & rccFileName, const QString & mapRoot = QString() )
 */
HB_FUNC( QT_QRESOURCE_UNREGISTERRESOURCE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retl( ( p )->unregisterResource( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_UNREGISTERRESOURCE FP=hb_retl( ( p )->unregisterResource( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool unregisterResource ( const uchar * rccData, const QString & mapRoot = QString() )
 */
HB_FUNC( QT_QRESOURCE_UNREGISTERRESOURCE_1 )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
      hb_retl( ( p )->unregisterResource( hbqt_par_uchar( 2 ), hbqt_par_QString( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRESOURCE_UNREGISTERRESOURCE_1 FP=hb_retl( ( p )->unregisterResource( hbqt_par_uchar( 2 ), hbqt_par_QString( 3 ) ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
