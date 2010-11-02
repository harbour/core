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

#if QT_VERSION >= 0x040500

/*
 *  flags OpenMode
 *  enum OpenModeFlag { NotOpen, ReadOnly, WriteOnly, ReadWrite, ..., Unbuffered }
 */

/*
 *  Constructed[ 33/33 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QIODevice>


/*
 * QIODevice ()
 * QIODevice ( QObject * parent )
 * ~QIODevice ()
 */

typedef struct
{
   QPointer< QIODevice > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QIODevice;

HBQT_GC_FUNC( hbqt_gcRelease_QIODevice )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QIODevice( void * pObj, bool bNew )
{
   HBQT_GC_T_QIODevice * p = ( HBQT_GC_T_QIODevice * ) hb_gcAllocate( sizeof( HBQT_GC_T_QIODevice ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QIODevice >( ( QIODevice * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QIODevice;
   p->type = HBQT_TYPE_QIODevice;

   return p;
}

HB_FUNC( QT_QIODEVICE )
{
   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      // __HB_RETPTRGC__( new QIODevice( hbqt_par_QObject( 1 ) ) );
   }
}

/* virtual bool atEnd () const */
HB_FUNC( QT_QIODEVICE_ATEND )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->atEnd() );
}

/* virtual qint64 bytesAvailable () const */
HB_FUNC( QT_QIODEVICE_BYTESAVAILABLE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->bytesAvailable() );
}

/* virtual qint64 bytesToWrite () const */
HB_FUNC( QT_QIODEVICE_BYTESTOWRITE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->bytesToWrite() );
}

/* virtual bool canReadLine () const */
HB_FUNC( QT_QIODEVICE_CANREADLINE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->canReadLine() );
}

/* virtual void close () */
HB_FUNC( QT_QIODEVICE_CLOSE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      ( p )->close();
}

/* QString errorString () const */
HB_FUNC( QT_QIODEVICE_ERRORSTRING )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retstr_utf8( ( p )->errorString().toUtf8().data() );
}

/* bool getChar ( char * c ) */
HB_FUNC( QT_QIODEVICE_GETCHAR )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->getChar( ( char * ) hb_parc( 2 ) ) );
}

/* bool isOpen () const */
HB_FUNC( QT_QIODEVICE_ISOPEN )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->isOpen() );
}

/* bool isReadable () const */
HB_FUNC( QT_QIODEVICE_ISREADABLE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->isReadable() );
}

/* virtual bool isSequential () const */
HB_FUNC( QT_QIODEVICE_ISSEQUENTIAL )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->isSequential() );
}

/* bool isTextModeEnabled () const */
HB_FUNC( QT_QIODEVICE_ISTEXTMODEENABLED )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->isTextModeEnabled() );
}

/* bool isWritable () const */
HB_FUNC( QT_QIODEVICE_ISWRITABLE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->isWritable() );
}

/* virtual bool open ( OpenMode mode ) */
HB_FUNC( QT_QIODEVICE_OPEN )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->open( ( QIODevice::OpenMode ) hb_parni( 2 ) ) );
}

/* OpenMode openMode () const */
HB_FUNC( QT_QIODEVICE_OPENMODE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retni( ( QIODevice::OpenMode ) ( p )->openMode() );
}

/* qint64 peek ( char * data, qint64 maxSize ) */
HB_FUNC( QT_QIODEVICE_PEEK )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->peek( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
}

/* QByteArray peek ( qint64 maxSize ) */
HB_FUNC( QT_QIODEVICE_PEEK_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->peek( hb_parnint( 2 ) ) ), true ) );
}

/* virtual qint64 pos () const */
HB_FUNC( QT_QIODEVICE_POS )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->pos() );
}

/* bool putChar ( char c ) */
HB_FUNC( QT_QIODEVICE_PUTCHAR )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->putChar( ( char ) hb_parni( 2 ) ) );
}

/* qint64 read ( char * data, qint64 maxSize ) */
HB_FUNC( QT_QIODEVICE_READ )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->read( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
}

/* QByteArray read ( qint64 maxSize ) */
HB_FUNC( QT_QIODEVICE_READ_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->read( hb_parnint( 2 ) ) ), true ) );
}

/* QByteArray readAll () */
HB_FUNC( QT_QIODEVICE_READALL )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readAll() ), true ) );
}

/* qint64 readLine ( char * data, qint64 maxSize ) */
HB_FUNC( QT_QIODEVICE_READLINE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->readLine( ( char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
}

/* QByteArray readLine ( qint64 maxSize = 0 ) */
HB_FUNC( QT_QIODEVICE_READLINE_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->readLine( hb_parnint( 2 ) ) ), true ) );
}

/* virtual bool reset () */
HB_FUNC( QT_QIODEVICE_RESET )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->reset() );
}

/* virtual bool seek ( qint64 pos ) */
HB_FUNC( QT_QIODEVICE_SEEK )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->seek( hb_parnint( 2 ) ) );
}

/* void setTextModeEnabled ( bool enabled ) */
HB_FUNC( QT_QIODEVICE_SETTEXTMODEENABLED )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      ( p )->setTextModeEnabled( hb_parl( 2 ) );
}

/* virtual qint64 size () const */
HB_FUNC( QT_QIODEVICE_SIZE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->size() );
}

/* void ungetChar ( char c ) */
HB_FUNC( QT_QIODEVICE_UNGETCHAR )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      ( p )->ungetChar( ( char ) hb_parni( 2 ) );
}

/* virtual bool waitForBytesWritten ( int msecs ) */
HB_FUNC( QT_QIODEVICE_WAITFORBYTESWRITTEN )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->waitForBytesWritten( hb_parni( 2 ) ) );
}

/* virtual bool waitForReadyRead ( int msecs ) */
HB_FUNC( QT_QIODEVICE_WAITFORREADYREAD )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retl( ( p )->waitForReadyRead( hb_parni( 2 ) ) );
}

/* qint64 write ( const char * data, qint64 maxSize ) */
HB_FUNC( QT_QIODEVICE_WRITE )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->write( ( const char * ) hb_parc( 2 ), hb_parnint( 3 ) ) );
}

/* qint64 write ( const char * data ) */
HB_FUNC( QT_QIODEVICE_WRITE_1 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->write( ( const char * ) hb_parc( 2 ) ) );
}

/* qint64 write ( const QByteArray & byteArray ) */
HB_FUNC( QT_QIODEVICE_WRITE_2 )
{
   QIODevice * p = hbqt_par_QIODevice( 1 );
   if( p )
      hb_retnint( ( p )->write( *hbqt_par_QByteArray( 2 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
