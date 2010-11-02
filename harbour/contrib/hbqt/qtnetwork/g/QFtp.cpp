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
#include "hbqtnetwork.h"

#if QT_VERSION >= 0x040500

/*
 *  enum Command { None, SetTransferMode, SetProxy, ConnectToHost, ..., RawCommand }
 *  enum Error { NoError, HostNotFound, ConnectionRefused, NotConnected, UnknownError }
 *  enum State { Unconnected, HostLookup, Connecting, Connected, LoggedIn, Closing }
 *  enum TransferMode { Passive, Active }
 *  enum TransferType { Binary, Ascii }
 */

/*
 *  Constructed[ 26/26 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // qint64 read ( char * data, qint64 maxlen )
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFtp;

HBQT_GC_FUNC( hbqt_gcRelease_QFtp )
{
   QFtp  * ph = NULL;
   HBQT_GC_T_QFtp * p = ( HBQT_GC_T_QFtp * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
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

void * hbqt_gcAllocate_QFtp( void * pObj, bool bNew )
{
   HBQT_GC_T_QFtp * p = ( HBQT_GC_T_QFtp * ) hb_gcAllocate( sizeof( HBQT_GC_T_QFtp ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFtp >( ( QFtp * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFtp;
   p->type = HBQT_TYPE_QFtp;

   return p;
}

HB_FUNC( QT_QFTP )
{
   QFtp * pObj = NULL;

   pObj = new QFtp( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QFtp( ( void * ) pObj, true ) );
}

/* qint64 bytesAvailable () const */
HB_FUNC( QT_QFTP_BYTESAVAILABLE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retnint( ( p )->bytesAvailable() );
}

/* int cd ( const QString & dir ) */
HB_FUNC( QT_QFTP_CD )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->cd( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* void clearPendingCommands () */
HB_FUNC( QT_QFTP_CLEARPENDINGCOMMANDS )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      ( p )->clearPendingCommands();
}

/* int close () */
HB_FUNC( QT_QFTP_CLOSE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->close() );
}

/* int connectToHost ( const QString & host, quint16 port = 21 ) */
HB_FUNC( QT_QFTP_CONNECTTOHOST )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->connectToHost( hb_parstr_utf8( 2, &pText, NULL ), hb_parnidef( 3, 21 ) ) );
      hb_strfree( pText );
   }
}

/* Command currentCommand () const */
HB_FUNC( QT_QFTP_CURRENTCOMMAND )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( QFtp::Command ) ( p )->currentCommand() );
}

/* QIODevice * currentDevice () const */
HB_FUNC( QT_QFTP_CURRENTDEVICE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->currentDevice(), false ) );
}

/* int currentId () const */
HB_FUNC( QT_QFTP_CURRENTID )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->currentId() );
}

/* Error error () const */
HB_FUNC( QT_QFTP_ERROR )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( QFtp::Error ) ( p )->error() );
}

/* QString errorString () const */
HB_FUNC( QT_QFTP_ERRORSTRING )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retstr_utf8( ( p )->errorString().toUtf8().data() );
}

/* int get ( const QString & file, QIODevice * dev = 0, TransferType type = Binary ) */
HB_FUNC( QT_QFTP_GET )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->get( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QIODevice( 3 ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) );
      hb_strfree( pText );
   }
}

/* bool hasPendingCommands () const */
HB_FUNC( QT_QFTP_HASPENDINGCOMMANDS )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retl( ( p )->hasPendingCommands() );
}

/* int list ( const QString & dir = QString() ) */
HB_FUNC( QT_QFTP_LIST )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->list( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int login ( const QString & user = QString(), const QString & password = QString() ) */
HB_FUNC( QT_QFTP_LOGIN )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->login( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int mkdir ( const QString & dir ) */
HB_FUNC( QT_QFTP_MKDIR )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->mkdir( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int put ( QIODevice * dev, const QString & file, TransferType type = Binary ) */
HB_FUNC( QT_QFTP_PUT )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->put( hbqt_par_QIODevice( 2 ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) );
      hb_strfree( pText );
   }
}

/* int put ( const QByteArray & data, const QString & file, TransferType type = Binary ) */
HB_FUNC( QT_QFTP_PUT_1 )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->put( *hbqt_par_QByteArray( 2 ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISNUM( 4 ) ? ( QFtp::TransferType ) hb_parni( 4 ) : ( QFtp::TransferType ) QFtp::Binary ) ) );
      hb_strfree( pText );
   }
}

/* int rawCommand ( const QString & command ) */
HB_FUNC( QT_QFTP_RAWCOMMAND )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->rawCommand( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* QByteArray readAll () */
HB_FUNC( QT_QFTP_READALL )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAll() ), true ) );
}

/* int remove ( const QString & file ) */
HB_FUNC( QT_QFTP_REMOVE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->remove( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int rename ( const QString & oldname, const QString & newname ) */
HB_FUNC( QT_QFTP_RENAME )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->rename( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int rmdir ( const QString & dir ) */
HB_FUNC( QT_QFTP_RMDIR )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->rmdir( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int setProxy ( const QString & host, quint16 port ) */
HB_FUNC( QT_QFTP_SETPROXY )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->setProxy( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ) ) );
      hb_strfree( pText );
   }
}

/* int setTransferMode ( TransferMode mode ) */
HB_FUNC( QT_QFTP_SETTRANSFERMODE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( p )->setTransferMode( ( QFtp::TransferMode ) hb_parni( 2 ) ) );
}

/* State state () const */
HB_FUNC( QT_QFTP_STATE )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      hb_retni( ( QFtp::State ) ( p )->state() );
}

/* void abort () */
HB_FUNC( QT_QFTP_ABORT )
{
   QFtp * p = hbqt_par_QFtp( 1 );
   if( p )
      ( p )->abort();
}


#endif /* #if QT_VERSION >= 0x040500 */
