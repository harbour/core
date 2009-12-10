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
 *  enum Command { None, SetTransferMode, SetProxy, ConnectToHost, ..., RawCommand }
 *  enum Error { NoError, HostNotFound, ConnectionRefused, NotConnected, UnknownError }
 *  enum State { Unconnected, HostLookup, Connecting, Connected, LoggedIn, Closing }
 *  enum TransferMode { Passive, Active }
 *  enum TransferType { Binary, Ascii }
 */

#include <QtCore/QPointer>

#include <QtNetwork/QFtp>


/*
 * QFtp ( QObject * parent = 0 )
 * virtual ~QFtp ()
 */

/*
 * qint64 read ( char * data, qint64 maxlen )
 */
HB_FUNC( QT_QFTP_READ )
{
   char * iData = ( char * ) hb_xgrab( hb_parnint( 3 ) + 1 );
   qint64 iRead;

   iRead = hbqt_par_QFtp( 1 )->read( iData, hb_parnint( 3 ) );

   hb_retnint( iRead );
   if( ! hb_storclen_buffer( iData, iRead, 2 ) )
      hb_xfree( iData );
}

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QFtp > pq;
} QGC_POINTER_QFtp;

QT_G_FUNC( release_QFtp )
{
   QGC_POINTER_QFtp * p = ( QGC_POINTER_QFtp * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QFtp                         p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QFtp                        ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QFtp * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QFtp * ) p->ph )->~QFtp();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QFtp * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QFtp                        Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QFtp                        Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QFtp                        Object Already deleted!" ) );
   }
}

void * gcAllocate_QFtp( void * pObj )
{
   QGC_POINTER_QFtp * p = ( QGC_POINTER_QFtp * ) hb_gcAllocate( sizeof( QGC_POINTER_QFtp ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QFtp;
   new( & p->pq ) QPointer< QFtp >( ( QFtp * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QFtp                        %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QFTP )
{
   void * pObj = NULL;

   pObj = new QFtp( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( gcAllocate_QFtp( pObj ) );
}
/*
 * qint64 bytesAvailable () const
 */
HB_FUNC( QT_QFTP_BYTESAVAILABLE )
{
   hb_retnint( hbqt_par_QFtp( 1 )->bytesAvailable() );
}

/*
 * int cd ( const QString & dir )
 */
HB_FUNC( QT_QFTP_CD )
{
   hb_retni( hbqt_par_QFtp( 1 )->cd( hbqt_par_QString( 2 ) ) );
}

/*
 * void clearPendingCommands ()
 */
HB_FUNC( QT_QFTP_CLEARPENDINGCOMMANDS )
{
   hbqt_par_QFtp( 1 )->clearPendingCommands();
}

/*
 * int close ()
 */
HB_FUNC( QT_QFTP_CLOSE )
{
   hb_retni( hbqt_par_QFtp( 1 )->close() );
}

/*
 * int connectToHost ( const QString & host, quint16 port = 21 )
 */
HB_FUNC( QT_QFTP_CONNECTTOHOST )
{
   hb_retni( hbqt_par_QFtp( 1 )->connectToHost( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 21 ) ) );
}

/*
 * Command currentCommand () const
 */
HB_FUNC( QT_QFTP_CURRENTCOMMAND )
{
   hb_retni( ( QFtp::Command ) hbqt_par_QFtp( 1 )->currentCommand() );
}

/*
 * QIODevice * currentDevice () const
 */
HB_FUNC( QT_QFTP_CURRENTDEVICE )
{
   hb_retptr( ( QIODevice* ) hbqt_par_QFtp( 1 )->currentDevice() );
}

/*
 * int currentId () const
 */
HB_FUNC( QT_QFTP_CURRENTID )
{
   hb_retni( hbqt_par_QFtp( 1 )->currentId() );
}

/*
 * Error error () const
 */
HB_FUNC( QT_QFTP_ERROR )
{
   hb_retni( ( QFtp::Error ) hbqt_par_QFtp( 1 )->error() );
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QFTP_ERRORSTRING )
{
   hb_retc( hbqt_par_QFtp( 1 )->errorString().toAscii().data() );
}

/*
 * int get ( const QString & file, QIODevice * dev = 0, TransferType type = Binary )
 */
HB_FUNC( QT_QFTP_GET )
{
   hb_retni( hbqt_par_QFtp( 1 )->get( hbqt_par_QString( 2 ), hbqt_par_QIODevice( 3 ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) );
}

/*
 * bool hasPendingCommands () const
 */
HB_FUNC( QT_QFTP_HASPENDINGCOMMANDS )
{
   hb_retl( hbqt_par_QFtp( 1 )->hasPendingCommands() );
}

/*
 * int list ( const QString & dir = QString() )
 */
HB_FUNC( QT_QFTP_LIST )
{
   hb_retni( hbqt_par_QFtp( 1 )->list( hbqt_par_QString( 2 ) ) );
}

/*
 * int login ( const QString & user = QString(), const QString & password = QString() )
 */
HB_FUNC( QT_QFTP_LOGIN )
{
   hb_retni( hbqt_par_QFtp( 1 )->login( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
}

/*
 * int mkdir ( const QString & dir )
 */
HB_FUNC( QT_QFTP_MKDIR )
{
   hb_retni( hbqt_par_QFtp( 1 )->mkdir( hbqt_par_QString( 2 ) ) );
}

/*
 * int put ( QIODevice * dev, const QString & file, TransferType type = Binary )
 */
HB_FUNC( QT_QFTP_PUT )
{
   hb_retni( hbqt_par_QFtp( 1 )->put( hbqt_par_QIODevice( 2 ), hbqt_par_QString( 3 ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) );
}

/*
 * int put ( const QByteArray & data, const QString & file, TransferType type = Binary )
 */
HB_FUNC( QT_QFTP_PUT_1 )
{
   hb_retni( hbqt_par_QFtp( 1 )->put( *hbqt_par_QByteArray( 2 ), hbqt_par_QString( 3 ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) );
}

/*
 * int rawCommand ( const QString & command )
 */
HB_FUNC( QT_QFTP_RAWCOMMAND )
{
   hb_retni( hbqt_par_QFtp( 1 )->rawCommand( hbqt_par_QString( 2 ) ) );
}

/*
 * QByteArray readAll ()
 */
HB_FUNC( QT_QFTP_READALL )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QFtp( 1 )->readAll() ) ) );
}

/*
 * int remove ( const QString & file )
 */
HB_FUNC( QT_QFTP_REMOVE )
{
   hb_retni( hbqt_par_QFtp( 1 )->remove( hbqt_par_QString( 2 ) ) );
}

/*
 * int rename ( const QString & oldname, const QString & newname )
 */
HB_FUNC( QT_QFTP_RENAME )
{
   hb_retni( hbqt_par_QFtp( 1 )->rename( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
}

/*
 * int rmdir ( const QString & dir )
 */
HB_FUNC( QT_QFTP_RMDIR )
{
   hb_retni( hbqt_par_QFtp( 1 )->rmdir( hbqt_par_QString( 2 ) ) );
}

/*
 * int setProxy ( const QString & host, quint16 port )
 */
HB_FUNC( QT_QFTP_SETPROXY )
{
   hb_retni( hbqt_par_QFtp( 1 )->setProxy( hbqt_par_QString( 2 ), hb_parni( 3 ) ) );
}

/*
 * int setTransferMode ( TransferMode mode )
 */
HB_FUNC( QT_QFTP_SETTRANSFERMODE )
{
   hb_retni( hbqt_par_QFtp( 1 )->setTransferMode( ( QFtp::TransferMode ) hb_parni( 2 ) ) );
}

/*
 * State state () const
 */
HB_FUNC( QT_QFTP_STATE )
{
   hb_retni( ( QFtp::State ) hbqt_par_QFtp( 1 )->state() );
}

/*
 * void abort ()
 */
HB_FUNC( QT_QFTP_ABORT )
{
   hbqt_par_QFtp( 1 )->abort();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
