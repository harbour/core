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
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextDocumentWriter>


/*
 * QTextDocumentWriter ()
 * QTextDocumentWriter ( QIODevice * device, const QByteArray & format )
 * QTextDocumentWriter ( const QString & fileName, const QByteArray & format = QByteArray() )
 * ~QTextDocumentWriter ()
 */

typedef struct
{
   QTextDocumentWriter * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextDocumentWriter;

HBQT_GC_FUNC( hbqt_gcRelease_QTextDocumentWriter )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextDocumentWriter * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextDocumentWriter( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextDocumentWriter * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextDocumentWriter;
   p->type = HBQT_TYPE_QTextDocumentWriter;

   return p;
}

HB_FUNC( QT_QTEXTDOCUMENTWRITER )
{
   QTextDocumentWriter * pObj = NULL;

   pObj = new QTextDocumentWriter() ;

   hb_retptrGC( hbqt_gcAllocate_QTextDocumentWriter( ( void * ) pObj, true ) );
}

/* QTextCodec * codec () const */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_CODEC )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->codec(), false ) );
}

/* QIODevice * device () const */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_DEVICE )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIODevice( ( p )->device(), false ) );
}

/* QString fileName () const */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_FILENAME )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
}

/* QByteArray format () const */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_FORMAT )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->format() ), true ) );
}

/* void setCodec ( QTextCodec * codec ) */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETCODEC )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      ( p )->setCodec( hbqt_par_QTextCodec( 2 ) );
}

/* void setDevice ( QIODevice * device ) */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETDEVICE )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      ( p )->setDevice( hbqt_par_QIODevice( 2 ) );
}

/* void setFileName ( const QString & fileName ) */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETFILENAME )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFileName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setFormat ( const QByteArray & format ) */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_SETFORMAT )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      ( p )->setFormat( *hbqt_par_QByteArray( 2 ) );
}

/* bool write ( const QTextDocument * document ) */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_WRITE )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retl( ( p )->write( hbqt_par_QTextDocument( 2 ) ) );
}

/* bool write ( const QTextDocumentFragment & fragment ) */
HB_FUNC( QT_QTEXTDOCUMENTWRITER_WRITE_1 )
{
   QTextDocumentWriter * p = hbqt_par_QTextDocumentWriter( 1 );
   if( p )
      hb_retl( ( p )->write( *hbqt_par_QTextDocumentFragment( 2 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
