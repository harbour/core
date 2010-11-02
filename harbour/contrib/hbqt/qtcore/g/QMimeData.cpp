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
 *  Constructed[ 21/22 [ 95.45% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void setUrls ( const QList<QUrl> & urls )
 */

#include <QtCore/QPointer>

#include <QtCore/QMimeData>
#include <QtCore/QStringList>
#include <QtCore/QUrl>

/* QMimeData ()
 * ~QMimeData ()
 */

typedef struct
{
   QPointer< QMimeData > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMimeData;

HBQT_GC_FUNC( hbqt_gcRelease_QMimeData )
{
   HBQT_GC_T_QMimeData * p = ( HBQT_GC_T_QMimeData * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QMimeData * ph = p->ph;
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

void * hbqt_gcAllocate_QMimeData( void * pObj, bool bNew )
{
   HBQT_GC_T_QMimeData * p = ( HBQT_GC_T_QMimeData * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMimeData ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMimeData >( ( QMimeData * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMimeData;
   p->type = HBQT_TYPE_QMimeData;

   return p;
}

HB_FUNC( QT_QMIMEDATA )
{
   QMimeData * pObj = NULL;

   pObj = new QMimeData() ;

   hb_retptrGC( hbqt_gcAllocate_QMimeData( ( void * ) pObj, true ) );
}

/* void clear () */
HB_FUNC( QT_QMIMEDATA_CLEAR )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      ( p )->clear();
}

/* QVariant colorData () const */
HB_FUNC( QT_QMIMEDATA_COLORDATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->colorData() ), true ) );
}

/* QByteArray data ( const QString & mimeType ) const */
HB_FUNC( QT_QMIMEDATA_DATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->data( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* virtual QStringList formats () const */
HB_FUNC( QT_QMIMEDATA_FORMATS )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->formats() ), true ) );
}

/* bool hasColor () const */
HB_FUNC( QT_QMIMEDATA_HASCOLOR )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      hb_retl( ( p )->hasColor() );
}

/* virtual bool hasFormat ( const QString & mimeType ) const */
HB_FUNC( QT_QMIMEDATA_HASFORMAT )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->hasFormat( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* bool hasHtml () const */
HB_FUNC( QT_QMIMEDATA_HASHTML )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      hb_retl( ( p )->hasHtml() );
}

/* bool hasImage () const */
HB_FUNC( QT_QMIMEDATA_HASIMAGE )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      hb_retl( ( p )->hasImage() );
}

/* bool hasText () const */
HB_FUNC( QT_QMIMEDATA_HASTEXT )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      hb_retl( ( p )->hasText() );
}

/* bool hasUrls () const */
HB_FUNC( QT_QMIMEDATA_HASURLS )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      hb_retl( ( p )->hasUrls() );
}

/* QString html () const */
HB_FUNC( QT_QMIMEDATA_HTML )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      hb_retstr_utf8( ( p )->html().toUtf8().data() );
}

/* QVariant imageData () const */
HB_FUNC( QT_QMIMEDATA_IMAGEDATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->imageData() ), true ) );
}

/* void removeFormat ( const QString & mimeType ) */
HB_FUNC( QT_QMIMEDATA_REMOVEFORMAT )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      ( p )->removeFormat( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setColorData ( const QVariant & color ) */
HB_FUNC( QT_QMIMEDATA_SETCOLORDATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      ( p )->setColorData( *hbqt_par_QVariant( 2 ) );
}

/* void setData ( const QString & mimeType, const QByteArray & data ) */
HB_FUNC( QT_QMIMEDATA_SETDATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      ( p )->setData( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QByteArray( 3 ) );
      hb_strfree( pText );
   }
}

/* void setHtml ( const QString & html ) */
HB_FUNC( QT_QMIMEDATA_SETHTML )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      ( p )->setHtml( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setImageData ( const QVariant & image ) */
HB_FUNC( QT_QMIMEDATA_SETIMAGEDATA )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      ( p )->setImageData( *hbqt_par_QVariant( 2 ) );
}

/* void setText ( const QString & text ) */
HB_FUNC( QT_QMIMEDATA_SETTEXT )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QString text () const */
HB_FUNC( QT_QMIMEDATA_TEXT )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* QList<QUrl> urls () const */
HB_FUNC( QT_QMIMEDATA_URLS )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QUrl>( ( p )->urls() ), true ) );
}

/* QStringList * hbUrlList() const */
HB_FUNC( QT_QMIMEDATA_HBURLLIST )
{
   QMimeData * p = hbqt_par_QMimeData( 1 );
   if( p )
   {
      QStringList strList;
      foreach ( QUrl url, ( p )->urls() )
      {
         strList << ( QString ) url.toString().toAscii().data();
      }
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( strList ), true ) );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
