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

#include "hbqt.h"
#include "hbqtnetwork_garbage.h"
#include "hbqtcore_garbage.h"

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
   QPointer< QFtp > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QFtp;

QT_G_FUNC( hbqt_gcRelease_QFtp )
{
   QFtp  * ph = NULL ;
   QGC_POINTER_QFtp * p = ( QGC_POINTER_QFtp * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFtp   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFtp   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QFtp          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFtp    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFtp    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFtp( void * pObj, bool bNew )
{
   QGC_POINTER_QFtp * p = ( QGC_POINTER_QFtp * ) hb_gcAllocate( sizeof( QGC_POINTER_QFtp ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFtp >( ( QFtp * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFtp;
   p->type = HBQT_TYPE_QFtp;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFtp  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFtp", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFTP )
{
   QFtp * pObj = NULL;

   pObj = new QFtp( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QFtp( ( void * ) pObj, true ) );
}

/*
 * qint64 bytesAvailable () const
 */
HB_FUNC( QT_QFTP_BYTESAVAILABLE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retnint( ( p )->bytesAvailable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_BYTESAVAILABLE FP=hb_retnint( ( p )->bytesAvailable() ); p is NULL" ) );
   }
}

/*
 * int cd ( const QString & dir )
 */
HB_FUNC( QT_QFTP_CD )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->cd( QFtp::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_CD FP=hb_retni( ( p )->cd( QFtp::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void clearPendingCommands ()
 */
HB_FUNC( QT_QFTP_CLEARPENDINGCOMMANDS )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      ( p )->clearPendingCommands();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_CLEARPENDINGCOMMANDS FP=( p )->clearPendingCommands(); p is NULL" ) );
   }
}

/*
 * int close ()
 */
HB_FUNC( QT_QFTP_CLOSE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->close() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_CLOSE FP=hb_retni( ( p )->close() ); p is NULL" ) );
   }
}

/*
 * int connectToHost ( const QString & host, quint16 port = 21 )
 */
HB_FUNC( QT_QFTP_CONNECTTOHOST )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->connectToHost( QFtp::tr( hb_parc( 2 ) ), hb_parnidef( 3, 21 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_CONNECTTOHOST FP=hb_retni( ( p )->connectToHost( QFtp::tr( hb_parc( 2 ) ), hb_parnidef( 3, 21 ) ) ); p is NULL" ) );
   }
}

/*
 * Command currentCommand () const
 */
HB_FUNC( QT_QFTP_CURRENTCOMMAND )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( QFtp::Command ) ( p )->currentCommand() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_CURRENTCOMMAND FP=hb_retni( ( QFtp::Command ) ( p )->currentCommand() ); p is NULL" ) );
   }
}

/*
 * QIODevice * currentDevice () const
 */
HB_FUNC( QT_QFTP_CURRENTDEVICE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->currentDevice(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_CURRENTDEVICE FP=hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->currentDevice(), false ) ); p is NULL" ) );
   }
}

/*
 * int currentId () const
 */
HB_FUNC( QT_QFTP_CURRENTID )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->currentId() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_CURRENTID FP=hb_retni( ( p )->currentId() ); p is NULL" ) );
   }
}

/*
 * Error error () const
 */
HB_FUNC( QT_QFTP_ERROR )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( QFtp::Error ) ( p )->error() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_ERROR FP=hb_retni( ( QFtp::Error ) ( p )->error() ); p is NULL" ) );
   }
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QFTP_ERRORSTRING )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retc( ( p )->errorString().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_ERRORSTRING FP=hb_retc( ( p )->errorString().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int get ( const QString & file, QIODevice * dev = 0, TransferType type = Binary )
 */
HB_FUNC( QT_QFTP_GET )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->get( QFtp::tr( hb_parc( 2 ) ), hbqt_par_QIODevice( 3 ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_GET FP=hb_retni( ( p )->get( QFtp::tr( hb_parc( 2 ) ), hbqt_par_QIODevice( 3 ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) ); p is NULL" ) );
   }
}

/*
 * bool hasPendingCommands () const
 */
HB_FUNC( QT_QFTP_HASPENDINGCOMMANDS )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retl( ( p )->hasPendingCommands() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_HASPENDINGCOMMANDS FP=hb_retl( ( p )->hasPendingCommands() ); p is NULL" ) );
   }
}

/*
 * int list ( const QString & dir = QString() )
 */
HB_FUNC( QT_QFTP_LIST )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->list( QFtp::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_LIST FP=hb_retni( ( p )->list( QFtp::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * int login ( const QString & user = QString(), const QString & password = QString() )
 */
HB_FUNC( QT_QFTP_LOGIN )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->login( QFtp::tr( hb_parc( 2 ) ), QFtp::tr( hb_parc( 3 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_LOGIN FP=hb_retni( ( p )->login( QFtp::tr( hb_parc( 2 ) ), QFtp::tr( hb_parc( 3 ) ) ) ); p is NULL" ) );
   }
}

/*
 * int mkdir ( const QString & dir )
 */
HB_FUNC( QT_QFTP_MKDIR )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->mkdir( QFtp::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_MKDIR FP=hb_retni( ( p )->mkdir( QFtp::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * int put ( QIODevice * dev, const QString & file, TransferType type = Binary )
 */
HB_FUNC( QT_QFTP_PUT )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->put( hbqt_par_QIODevice( 2 ), QFtp::tr( hb_parc( 3 ) ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_PUT FP=hb_retni( ( p )->put( hbqt_par_QIODevice( 2 ), QFtp::tr( hb_parc( 3 ) ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) ); p is NULL" ) );
   }
}

/*
 * int put ( const QByteArray & data, const QString & file, TransferType type = Binary )
 */
HB_FUNC( QT_QFTP_PUT_1 )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->put( *hbqt_par_QByteArray( 2 ), QFtp::tr( hb_parc( 3 ) ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_PUT_1 FP=hb_retni( ( p )->put( *hbqt_par_QByteArray( 2 ), QFtp::tr( hb_parc( 3 ) ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) ); p is NULL" ) );
   }
}

/*
 * int rawCommand ( const QString & command )
 */
HB_FUNC( QT_QFTP_RAWCOMMAND )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->rawCommand( QFtp::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_RAWCOMMAND FP=hb_retni( ( p )->rawCommand( QFtp::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * QByteArray readAll ()
 */
HB_FUNC( QT_QFTP_READALL )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAll() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_READALL FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAll() ), true ) ); p is NULL" ) );
   }
}

/*
 * int remove ( const QString & file )
 */
HB_FUNC( QT_QFTP_REMOVE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->remove( QFtp::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_REMOVE FP=hb_retni( ( p )->remove( QFtp::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * int rename ( const QString & oldname, const QString & newname )
 */
HB_FUNC( QT_QFTP_RENAME )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->rename( QFtp::tr( hb_parc( 2 ) ), QFtp::tr( hb_parc( 3 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_RENAME FP=hb_retni( ( p )->rename( QFtp::tr( hb_parc( 2 ) ), QFtp::tr( hb_parc( 3 ) ) ) ); p is NULL" ) );
   }
}

/*
 * int rmdir ( const QString & dir )
 */
HB_FUNC( QT_QFTP_RMDIR )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->rmdir( QFtp::tr( hb_parc( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_RMDIR FP=hb_retni( ( p )->rmdir( QFtp::tr( hb_parc( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * int setProxy ( const QString & host, quint16 port )
 */
HB_FUNC( QT_QFTP_SETPROXY )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->setProxy( QFtp::tr( hb_parc( 2 ) ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_SETPROXY FP=hb_retni( ( p )->setProxy( QFtp::tr( hb_parc( 2 ) ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * int setTransferMode ( TransferMode mode )
 */
HB_FUNC( QT_QFTP_SETTRANSFERMODE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->setTransferMode( ( QFtp::TransferMode ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_SETTRANSFERMODE FP=hb_retni( ( p )->setTransferMode( ( QFtp::TransferMode ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * State state () const
 */
HB_FUNC( QT_QFTP_STATE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( QFtp::State ) ( p )->state() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_STATE FP=hb_retni( ( QFtp::State ) ( p )->state() ); p is NULL" ) );
   }
}

/*
 * void abort ()
 */
HB_FUNC( QT_QFTP_ABORT )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      ( p )->abort();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFTP_ABORT FP=( p )->abort(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
