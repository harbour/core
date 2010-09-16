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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QResource;

HBQT_GC_FUNC( hbqt_gcRelease_QResource )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

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
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

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
   {
      hb_retstr_utf8( ( p )->absoluteFilePath().toUtf8().data() );
   }
}

/*
 * const uchar * data () const
 */
HB_FUNC( QT_QRESOURCE_DATA )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      hb_retc( ( const char * ) ( p )->data() );
   }
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QRESOURCE_FILENAME )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
   }
}

/*
 * bool isCompressed () const
 */
HB_FUNC( QT_QRESOURCE_ISCOMPRESSED )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      hb_retl( ( p )->isCompressed() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QRESOURCE_ISVALID )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * QLocale locale () const
 */
HB_FUNC( QT_QRESOURCE_LOCALE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->locale() ), true ) );
   }
}

/*
 * void setFileName ( const QString & file )
 */
HB_FUNC( QT_QRESOURCE_SETFILENAME )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFileName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setLocale ( const QLocale & locale )
 */
HB_FUNC( QT_QRESOURCE_SETLOCALE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      ( p )->setLocale( *hbqt_par_QLocale( 2 ) );
   }
}

/*
 * qint64 size () const
 */
HB_FUNC( QT_QRESOURCE_SIZE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      hb_retnint( ( p )->size() );
   }
}

/*
 * bool registerResource ( const QString & rccFileName, const QString & mapRoot = QString() )
 */
HB_FUNC( QT_QRESOURCE_REGISTERRESOURCE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->registerResource( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool registerResource ( const uchar * rccData, const QString & mapRoot = QString() )
 */
HB_FUNC( QT_QRESOURCE_REGISTERRESOURCE_1 )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->registerResource( hbqt_par_uchar( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * QStringList searchPaths ()
 */
HB_FUNC( QT_QRESOURCE_SEARCHPATHS )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->searchPaths() ), true ) );
   }
}

/*
 * bool unregisterResource ( const QString & rccFileName, const QString & mapRoot = QString() )
 */
HB_FUNC( QT_QRESOURCE_UNREGISTERRESOURCE )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->unregisterResource( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool unregisterResource ( const uchar * rccData, const QString & mapRoot = QString() )
 */
HB_FUNC( QT_QRESOURCE_UNREGISTERRESOURCE_1 )
{
   QResource * p = hbqt_par_QResource( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->unregisterResource( hbqt_par_uchar( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
