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
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtNetwork/QHttpResponseHeader>


/* QHttpResponseHeader ()
 * QHttpResponseHeader ( const QHttpResponseHeader & header )
 * QHttpResponseHeader ( const QString & str )
 * QHttpResponseHeader ( int code, const QString & text = QString(), int majorVer = 1, int minorVer = 1 )
 */

typedef struct
{
   QHttpResponseHeader * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QHttpResponseHeader;

HBQT_GC_FUNC( hbqt_gcRelease_QHttpResponseHeader )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QHttpResponseHeader * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QHttpResponseHeader( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QHttpResponseHeader * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QHttpResponseHeader;
   p->type = HBQT_TYPE_QHttpResponseHeader;

   return p;
}

HB_FUNC( QT_QHTTPRESPONSEHEADER )
{
   QHttpResponseHeader * pObj = NULL;

   pObj = new QHttpResponseHeader() ;

   hb_retptrGC( hbqt_gcAllocate_QHttpResponseHeader( ( void * ) pObj, true ) );
}

/* virtual int majorVersion () const */
HB_FUNC( QT_QHTTPRESPONSEHEADER_MAJORVERSION )
{
   QHttpResponseHeader * p = hbqt_par_QHttpResponseHeader( 1 );
   if( p )
      hb_retni( ( p )->majorVersion() );
}

/* virtual int minorVersion () const */
HB_FUNC( QT_QHTTPRESPONSEHEADER_MINORVERSION )
{
   QHttpResponseHeader * p = hbqt_par_QHttpResponseHeader( 1 );
   if( p )
      hb_retni( ( p )->minorVersion() );
}

/* QString reasonPhrase () const */
HB_FUNC( QT_QHTTPRESPONSEHEADER_REASONPHRASE )
{
   QHttpResponseHeader * p = hbqt_par_QHttpResponseHeader( 1 );
   if( p )
      hb_retstr_utf8( ( p )->reasonPhrase().toUtf8().data() );
}

/* void setStatusLine ( int code, const QString & text = QString(), int majorVer = 1, int minorVer = 1 ) */
HB_FUNC( QT_QHTTPRESPONSEHEADER_SETSTATUSLINE )
{
   QHttpResponseHeader * p = hbqt_par_QHttpResponseHeader( 1 );
   if( p )
   {
      void * pText;
      ( p )->setStatusLine( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parnidef( 4, 1 ), hb_parnidef( 5, 1 ) );
      hb_strfree( pText );
   }
}

/* int statusCode () const */
HB_FUNC( QT_QHTTPRESPONSEHEADER_STATUSCODE )
{
   QHttpResponseHeader * p = hbqt_par_QHttpResponseHeader( 1 );
   if( p )
      hb_retni( ( p )->statusCode() );
}


#endif /* #if QT_VERSION >= 0x040500 */
