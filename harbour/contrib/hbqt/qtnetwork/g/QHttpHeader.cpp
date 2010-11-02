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
 *  Constructed[ 18/19 [ 94.74% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void setValues ( const QList<QPair<QString, QString> > & values )
 */

#include <QtCore/QPointer>

#include <QtNetwork/QHttpHeader>


/*
 * QHttpHeader ()
 * QHttpHeader ( const QHttpHeader & header )
 * QHttpHeader ( const QString & str )
 * virtual ~QHttpHeader ()
 */

typedef struct
{
   QHttpHeader * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QHttpHeader;

HBQT_GC_FUNC( hbqt_gcRelease_QHttpHeader )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QHttpHeader( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QHttpHeader * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QHttpHeader;
   p->type = HBQT_TYPE_QHttpHeader;

   return p;
}

HB_FUNC( QT_QHTTPHEADER )
{

}

/* void addValue ( const QString & key, const QString & value ) */
HB_FUNC( QT_QHTTPHEADER_ADDVALUE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
   {
      void * pText;
      ( p )->addValue( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QStringList allValues ( const QString & key ) const */
HB_FUNC( QT_QHTTPHEADER_ALLVALUES )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->allValues( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* uint contentLength () const */
HB_FUNC( QT_QHTTPHEADER_CONTENTLENGTH )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retni( ( p )->contentLength() );
}

/* QString contentType () const */
HB_FUNC( QT_QHTTPHEADER_CONTENTTYPE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retstr_utf8( ( p )->contentType().toUtf8().data() );
}

/* bool hasContentLength () const */
HB_FUNC( QT_QHTTPHEADER_HASCONTENTLENGTH )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retl( ( p )->hasContentLength() );
}

/* bool hasContentType () const */
HB_FUNC( QT_QHTTPHEADER_HASCONTENTTYPE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retl( ( p )->hasContentType() );
}

/* bool hasKey ( const QString & key ) const */
HB_FUNC( QT_QHTTPHEADER_HASKEY )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->hasKey( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* bool isValid () const */
HB_FUNC( QT_QHTTPHEADER_ISVALID )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* QStringList keys () const */
HB_FUNC( QT_QHTTPHEADER_KEYS )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->keys() ), true ) );
}

/* virtual int majorVersion () const = 0 */
HB_FUNC( QT_QHTTPHEADER_MAJORVERSION )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retni( ( p )->majorVersion() );
}

/* virtual int minorVersion () const = 0 */
HB_FUNC( QT_QHTTPHEADER_MINORVERSION )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retni( ( p )->minorVersion() );
}

/* void removeAllValues ( const QString & key ) */
HB_FUNC( QT_QHTTPHEADER_REMOVEALLVALUES )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
   {
      void * pText;
      ( p )->removeAllValues( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void removeValue ( const QString & key ) */
HB_FUNC( QT_QHTTPHEADER_REMOVEVALUE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
   {
      void * pText;
      ( p )->removeValue( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setContentLength ( int len ) */
HB_FUNC( QT_QHTTPHEADER_SETCONTENTLENGTH )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      ( p )->setContentLength( hb_parni( 2 ) );
}

/* void setContentType ( const QString & type ) */
HB_FUNC( QT_QHTTPHEADER_SETCONTENTTYPE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
   {
      void * pText;
      ( p )->setContentType( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setValue ( const QString & key, const QString & value ) */
HB_FUNC( QT_QHTTPHEADER_SETVALUE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
   {
      void * pText;
      ( p )->setValue( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* virtual QString toString () const */
HB_FUNC( QT_QHTTPHEADER_TOSTRING )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toString().toUtf8().data() );
}

/* QString value ( const QString & key ) const */
HB_FUNC( QT_QHTTPHEADER_VALUE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->value( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
