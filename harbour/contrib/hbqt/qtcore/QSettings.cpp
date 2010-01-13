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

/*
 *  enum Format { NativeFormat, IniFormat, InvalidFormat }
 *  enum Scope { UserScope, SystemScope }
 *  enum Status { NoError, AccessError, FormatError }
 */

#include <QtCore/QPointer>

#include <QtCore/QSettings>
#include <QtCore/QStringList>

/*
 * QSettings ( const QString & organization, const QString & application = QString(), QObject * parent = 0 )
 * QSettings ( Scope scope, const QString & organization, const QString & application = QString(), QObject * parent =  * 0 )
 * QSettings ( Format format, Scope scope, const QString & organization, const QString & application = QString(),  * QObject * parent = 0 )
 * QSettings ( const QString & fileName, Format format, QObject * parent = 0 )
 * QSettings ( QObject * parent = 0 )
 * ~QSettings ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QSettings > pq;
} QGC_POINTER_QSettings;

QT_G_FUNC( hbqt_gcRelease_QSettings )
{
   QGC_POINTER_QSettings * p = ( QGC_POINTER_QSettings * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QSettings                    p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QSettings                   ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QSettings * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QSettings * ) p->ph )->~QSettings();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QSettings * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QSettings                   Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO hbqt_gcRelease_QSettings                   Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QSettings                   Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QSettings( void * pObj )
{
   QGC_POINTER_QSettings * p = ( QGC_POINTER_QSettings * ) hb_gcAllocate( sizeof( QGC_POINTER_QSettings ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QSettings;
   new( & p->pq ) QPointer< QSettings >( ( QSettings * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QSettings                   %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QSETTINGS )
{
   void * pObj = NULL;

   if( hb_pcount() >= 2 && HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      pObj = new QSettings( hbqt_par_QString( 1 ), hbqt_par_QString( 2 ), 0 ) ;
   }
   else if( hb_pcount() >= 2 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QSettings( hbqt_par_QString( 1 ), ( QSettings::Format ) hb_parni( 2 ) ) ;
   }
   else
   {
      pObj = new QSettings() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QSettings( pObj ) );
}
/*
 * QStringList allKeys () const
 */
HB_FUNC( QT_QSETTINGS_ALLKEYS )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QSettings( 1 )->allKeys() ) ) );
}

/*
 * QString applicationName () const
 */
HB_FUNC( QT_QSETTINGS_APPLICATIONNAME )
{
   hb_retc( hbqt_par_QSettings( 1 )->applicationName().toAscii().data() );
}

/*
 * void beginGroup ( const QString & prefix )
 */
HB_FUNC( QT_QSETTINGS_BEGINGROUP )
{
   hbqt_par_QSettings( 1 )->beginGroup( QSettings::tr( hb_parc( 2 ) ) );
}

/*
 * int beginReadArray ( const QString & prefix )
 */
HB_FUNC( QT_QSETTINGS_BEGINREADARRAY )
{
   hb_retni( hbqt_par_QSettings( 1 )->beginReadArray( QSettings::tr( hb_parc( 2 ) ) ) );
}

/*
 * void beginWriteArray ( const QString & prefix, int size = -1 )
 */
HB_FUNC( QT_QSETTINGS_BEGINWRITEARRAY )
{
   hbqt_par_QSettings( 1 )->beginWriteArray( QSettings::tr( hb_parc( 2 ) ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) );
}

/*
 * QStringList childGroups () const
 */
HB_FUNC( QT_QSETTINGS_CHILDGROUPS )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QSettings( 1 )->childGroups() ) ) );
}

/*
 * QStringList childKeys () const
 */
HB_FUNC( QT_QSETTINGS_CHILDKEYS )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QSettings( 1 )->childKeys() ) ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QSETTINGS_CLEAR )
{
   hbqt_par_QSettings( 1 )->clear();
}

/*
 * bool contains ( const QString & key ) const
 */
HB_FUNC( QT_QSETTINGS_CONTAINS )
{
   hb_retl( hbqt_par_QSettings( 1 )->contains( QSettings::tr( hb_parc( 2 ) ) ) );
}

/*
 * void endArray ()
 */
HB_FUNC( QT_QSETTINGS_ENDARRAY )
{
   hbqt_par_QSettings( 1 )->endArray();
}

/*
 * void endGroup ()
 */
HB_FUNC( QT_QSETTINGS_ENDGROUP )
{
   hbqt_par_QSettings( 1 )->endGroup();
}

/*
 * bool fallbacksEnabled () const
 */
HB_FUNC( QT_QSETTINGS_FALLBACKSENABLED )
{
   hb_retl( hbqt_par_QSettings( 1 )->fallbacksEnabled() );
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QSETTINGS_FILENAME )
{
   hb_retc( hbqt_par_QSettings( 1 )->fileName().toAscii().data() );
}

/*
 * Format format () const
 */
HB_FUNC( QT_QSETTINGS_FORMAT )
{
   hb_retni( ( QSettings::Format ) hbqt_par_QSettings( 1 )->format() );
}

/*
 * QString group () const
 */
HB_FUNC( QT_QSETTINGS_GROUP )
{
   hb_retc( hbqt_par_QSettings( 1 )->group().toAscii().data() );
}

/*
 * QTextCodec * iniCodec () const
 */
HB_FUNC( QT_QSETTINGS_INICODEC )
{
   hb_retptr( ( QTextCodec* ) hbqt_par_QSettings( 1 )->iniCodec() );
}

/*
 * bool isWritable () const
 */
HB_FUNC( QT_QSETTINGS_ISWRITABLE )
{
   hb_retl( hbqt_par_QSettings( 1 )->isWritable() );
}

/*
 * QString organizationName () const
 */
HB_FUNC( QT_QSETTINGS_ORGANIZATIONNAME )
{
   hb_retc( hbqt_par_QSettings( 1 )->organizationName().toAscii().data() );
}

/*
 * void remove ( const QString & key )
 */
HB_FUNC( QT_QSETTINGS_REMOVE )
{
   hbqt_par_QSettings( 1 )->remove( QSettings::tr( hb_parc( 2 ) ) );
}

/*
 * Scope scope () const
 */
HB_FUNC( QT_QSETTINGS_SCOPE )
{
   hb_retni( ( QSettings::Scope ) hbqt_par_QSettings( 1 )->scope() );
}

/*
 * void setArrayIndex ( int i )
 */
HB_FUNC( QT_QSETTINGS_SETARRAYINDEX )
{
   hbqt_par_QSettings( 1 )->setArrayIndex( hb_parni( 2 ) );
}

/*
 * void setFallbacksEnabled ( bool b )
 */
HB_FUNC( QT_QSETTINGS_SETFALLBACKSENABLED )
{
   hbqt_par_QSettings( 1 )->setFallbacksEnabled( hb_parl( 2 ) );
}

/*
 * void setIniCodec ( QTextCodec * codec )
 */
HB_FUNC( QT_QSETTINGS_SETINICODEC )
{
   hbqt_par_QSettings( 1 )->setIniCodec( hbqt_par_QTextCodec( 2 ) );
}

/*
 * void setIniCodec ( const char * codecName )
 */
HB_FUNC( QT_QSETTINGS_SETINICODEC_1 )
{
   hbqt_par_QSettings( 1 )->setIniCodec( hbqt_par_char( 2 ) );
}

/*
 * void setValue ( const QString & key, const QVariant & value )
 */
HB_FUNC( QT_QSETTINGS_SETVALUE )
{
   hbqt_par_QSettings( 1 )->setValue( QSettings::tr( hb_parc( 2 ) ), *hbqt_par_QVariant( 3 ) );
}

/*
 * Status status () const
 */
HB_FUNC( QT_QSETTINGS_STATUS )
{
   hb_retni( ( QSettings::Status ) hbqt_par_QSettings( 1 )->status() );
}

/*
 * void sync ()
 */
HB_FUNC( QT_QSETTINGS_SYNC )
{
   hbqt_par_QSettings( 1 )->sync();
}

/*
 * QVariant value ( const QString & key, const QVariant & defaultValue = QVariant() ) const
 */
HB_FUNC( QT_QSETTINGS_VALUE )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QSettings( 1 )->value( QSettings::tr( hb_parc( 2 ) ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QVariant( 3 ) : QVariant() ) ) ) ) );
}

/*
 * Format defaultFormat ()
 */
HB_FUNC( QT_QSETTINGS_DEFAULTFORMAT )
{
   hb_retni( ( QSettings::Format ) hbqt_par_QSettings( 1 )->defaultFormat() );
}

/*
 * void setDefaultFormat ( Format format )
 */
HB_FUNC( QT_QSETTINGS_SETDEFAULTFORMAT )
{
   hbqt_par_QSettings( 1 )->setDefaultFormat( ( QSettings::Format ) hb_parni( 2 ) );
}

/*
 * void setPath ( Format format, Scope scope, const QString & path )
 */
HB_FUNC( QT_QSETTINGS_SETPATH )
{
   hbqt_par_QSettings( 1 )->setPath( ( QSettings::Format ) hb_parni( 2 ), ( QSettings::Scope ) hb_parni( 3 ), QSettings::tr( hb_parc( 4 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
