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

#include <QtNetwork/QHttpRequestHeader>


/* QHttpRequestHeader ()
 * QHttpRequestHeader ( const QString & method, const QString & path, int majorVer = 1, int minorVer = 1 )
 * QHttpRequestHeader ( const QHttpRequestHeader & header )
 * QHttpRequestHeader ( const QString & str )
 */

typedef struct
{
   QHttpRequestHeader * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QHttpRequestHeader;

HBQT_GC_FUNC( hbqt_gcRelease_QHttpRequestHeader )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QHttpRequestHeader * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QHttpRequestHeader( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QHttpRequestHeader * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QHttpRequestHeader;
   p->type = HBQT_TYPE_QHttpRequestHeader;

   return p;
}

HB_FUNC( QT_QHTTPREQUESTHEADER )
{
   QHttpRequestHeader * pObj = NULL;

   pObj = new QHttpRequestHeader() ;

   hb_retptrGC( hbqt_gcAllocate_QHttpRequestHeader( ( void * ) pObj, true ) );
}

/* virtual int majorVersion () const */
HB_FUNC( QT_QHTTPREQUESTHEADER_MAJORVERSION )
{
   QHttpRequestHeader * p = hbqt_par_QHttpRequestHeader( 1 );
   if( p )
      hb_retni( ( p )->majorVersion() );
}

/* QString method () const */
HB_FUNC( QT_QHTTPREQUESTHEADER_METHOD )
{
   QHttpRequestHeader * p = hbqt_par_QHttpRequestHeader( 1 );
   if( p )
      hb_retstr_utf8( ( p )->method().toUtf8().data() );
}

/* virtual int minorVersion () const */
HB_FUNC( QT_QHTTPREQUESTHEADER_MINORVERSION )
{
   QHttpRequestHeader * p = hbqt_par_QHttpRequestHeader( 1 );
   if( p )
      hb_retni( ( p )->minorVersion() );
}

/* QString path () const */
HB_FUNC( QT_QHTTPREQUESTHEADER_PATH )
{
   QHttpRequestHeader * p = hbqt_par_QHttpRequestHeader( 1 );
   if( p )
      hb_retstr_utf8( ( p )->path().toUtf8().data() );
}

/* void setRequest ( const QString & method, const QString & path, int majorVer = 1, int minorVer = 1 ) */
HB_FUNC( QT_QHTTPREQUESTHEADER_SETREQUEST )
{
   QHttpRequestHeader * p = hbqt_par_QHttpRequestHeader( 1 );
   if( p )
   {
      void * pText;
      ( p )->setRequest( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), hb_parnidef( 4, 1 ), hb_parnidef( 5, 1 ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
