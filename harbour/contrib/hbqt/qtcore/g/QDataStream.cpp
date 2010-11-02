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
 *  enum ByteOrder { BigEndian, LittleEndian }
 *  enum Status { Ok, ReadPastEnd, ReadCorruptData }
 *  enum Version { Qt_1_0, Qt_2_0, Qt_2_1, Qt_3_0, ..., Qt_4_5 }
 */

/*
 *  Constructed[ 13/13 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QDataStream & readBytes ( char *& s, uint & l )
 *  //QDataStream & writeBytes ( const char * s, uint len )
 */

#include <QtCore/QPointer>

#include <QtCore/QDataStream>


/* QDataStream ()
 * QDataStream ( QIODevice * d )
 * QDataStream ( QByteArray * a, QIODevice::OpenMode mode )
 * QDataStream ( const QByteArray & a )
 * virtual ~QDataStream ()
 */

typedef struct
{
   QDataStream * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDataStream;

HBQT_GC_FUNC( hbqt_gcRelease_QDataStream )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QDataStream * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDataStream( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QDataStream * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDataStream;
   p->type = HBQT_TYPE_QDataStream;

   return p;
}

HB_FUNC( QT_QDATASTREAM )
{
   QDataStream * pObj = NULL;

   pObj = new QDataStream() ;

   hb_retptrGC( hbqt_gcAllocate_QDataStream( ( void * ) pObj, true ) );
}

/* bool atEnd () const */
HB_FUNC( QT_QDATASTREAM_ATEND )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retl( ( p )->atEnd() );
}

/* ByteOrder byteOrder () const */
HB_FUNC( QT_QDATASTREAM_BYTEORDER )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( QDataStream::ByteOrder ) ( p )->byteOrder() );
}

/* QIODevice * device () const */
HB_FUNC( QT_QDATASTREAM_DEVICE )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
}

/* int readRawData ( char * s, int len ) */
HB_FUNC( QT_QDATASTREAM_READRAWDATA )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( p )->readRawData( ( char * ) hb_parc( 2 ), hb_parni( 3 ) ) );
}

/* void resetStatus () */
HB_FUNC( QT_QDATASTREAM_RESETSTATUS )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      ( p )->resetStatus();
}

/* void setByteOrder ( ByteOrder bo ) */
HB_FUNC( QT_QDATASTREAM_SETBYTEORDER )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      ( p )->setByteOrder( ( QDataStream::ByteOrder ) hb_parni( 2 ) );
}

/* void setDevice ( QIODevice * d ) */
HB_FUNC( QT_QDATASTREAM_SETDEVICE )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
}

/* void setStatus ( Status status ) */
HB_FUNC( QT_QDATASTREAM_SETSTATUS )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      ( p )->setStatus( ( QDataStream::Status ) hb_parni( 2 ) );
}

/* void setVersion ( int v ) */
HB_FUNC( QT_QDATASTREAM_SETVERSION )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      ( p )->setVersion( hb_parni( 2 ) );
}

/* int skipRawData ( int len ) */
HB_FUNC( QT_QDATASTREAM_SKIPRAWDATA )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( p )->skipRawData( hb_parni( 2 ) ) );
}

/* Status status () const */
HB_FUNC( QT_QDATASTREAM_STATUS )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( QDataStream::Status ) ( p )->status() );
}

/* int version () const */
HB_FUNC( QT_QDATASTREAM_VERSION )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( p )->version() );
}

/* int writeRawData ( const char * s, int len ) */
HB_FUNC( QT_QDATASTREAM_WRITERAWDATA )
{
   QDataStream * p = hbqt_par_QDataStream( 1 );
   if( p )
      hb_retni( ( p )->writeRawData( ( const char * ) hb_parc( 2 ), hb_parni( 3 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
