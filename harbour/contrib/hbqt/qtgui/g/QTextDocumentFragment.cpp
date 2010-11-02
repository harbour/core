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
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextDocumentFragment>


/*
 * QTextDocumentFragment ()
 * QTextDocumentFragment ( const QTextDocument * document )
 * QTextDocumentFragment ( const QTextCursor & cursor )
 * QTextDocumentFragment ( const QTextDocumentFragment & other )
 * ~QTextDocumentFragment ()
 */

typedef struct
{
   QTextDocumentFragment * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextDocumentFragment;

HBQT_GC_FUNC( hbqt_gcRelease_QTextDocumentFragment )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextDocumentFragment * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextDocumentFragment( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextDocumentFragment * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextDocumentFragment;
   p->type = HBQT_TYPE_QTextDocumentFragment;

   return p;
}

HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT )
{
   QTextDocumentFragment * pObj = NULL;

   pObj = new QTextDocumentFragment() ;

   hb_retptrGC( hbqt_gcAllocate_QTextDocumentFragment( ( void * ) pObj, true ) );
}

/* bool isEmpty () const */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_ISEMPTY )
{
   QTextDocumentFragment * p = hbqt_par_QTextDocumentFragment( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* QString toHtml ( const QByteArray & encoding ) const */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_TOHTML )
{
   QTextDocumentFragment * p = hbqt_par_QTextDocumentFragment( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toHtml( *hbqt_par_QByteArray( 2 ) ).toUtf8().data() );
}

/* QString toHtml () const */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_TOHTML_1 )
{
   QTextDocumentFragment * p = hbqt_par_QTextDocumentFragment( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toHtml().toUtf8().data() );
}

/* QString toPlainText () const */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_TOPLAINTEXT )
{
   QTextDocumentFragment * p = hbqt_par_QTextDocumentFragment( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toPlainText().toUtf8().data() );
}

/* QTextDocumentFragment fromHtml ( const QString & text ) */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_FROMHTML )
{
   QTextDocumentFragment * p = hbqt_par_QTextDocumentFragment( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QTextDocumentFragment( new QTextDocumentFragment( ( p )->fromHtml( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QTextDocumentFragment fromHtml ( const QString & text, const QTextDocument * resourceProvider ) */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_FROMHTML_1 )
{
   QTextDocumentFragment * p = hbqt_par_QTextDocumentFragment( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QTextDocumentFragment( new QTextDocumentFragment( ( p )->fromHtml( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QTextDocument( 3 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QTextDocumentFragment fromPlainText ( const QString & plainText ) */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_FROMPLAINTEXT )
{
   QTextDocumentFragment * p = hbqt_par_QTextDocumentFragment( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QTextDocumentFragment( new QTextDocumentFragment( ( p )->fromPlainText( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
