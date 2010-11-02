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
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QPlainTextDocumentLayout>


/*
 * QPlainTextDocumentLayout ( QTextDocument * document )
 * ~QPlainTextDocumentLayout ()
 */

typedef struct
{
   QPointer< QPlainTextDocumentLayout > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPlainTextDocumentLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QPlainTextDocumentLayout )
{
   HBQT_GC_T_QPlainTextDocumentLayout * p = ( HBQT_GC_T_QPlainTextDocumentLayout * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QPlainTextDocumentLayout * ph = p->ph;
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

void * hbqt_gcAllocate_QPlainTextDocumentLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QPlainTextDocumentLayout * p = ( HBQT_GC_T_QPlainTextDocumentLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QPlainTextDocumentLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QPlainTextDocumentLayout >( ( QPlainTextDocumentLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPlainTextDocumentLayout;
   p->type = HBQT_TYPE_QPlainTextDocumentLayout;

   return p;
}

HB_FUNC( QT_QPLAINTEXTDOCUMENTLAYOUT )
{
   QPlainTextDocumentLayout * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPlainTextDocumentLayout( hbqt_par_QTextDocument( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPlainTextDocumentLayout( ( void * ) pObj, true ) );
}

/* int cursorWidth () const */
HB_FUNC( QT_QPLAINTEXTDOCUMENTLAYOUT_CURSORWIDTH )
{
   QPlainTextDocumentLayout * p = hbqt_par_QPlainTextDocumentLayout( 1 );
   if( p )
      hb_retni( ( p )->cursorWidth() );
}

/* void ensureBlockLayout ( const QTextBlock & block ) const */
HB_FUNC( QT_QPLAINTEXTDOCUMENTLAYOUT_ENSUREBLOCKLAYOUT )
{
   QPlainTextDocumentLayout * p = hbqt_par_QPlainTextDocumentLayout( 1 );
   if( p )
      ( p )->ensureBlockLayout( *hbqt_par_QTextBlock( 2 ) );
}

/* void requestUpdate () */
HB_FUNC( QT_QPLAINTEXTDOCUMENTLAYOUT_REQUESTUPDATE )
{
   QPlainTextDocumentLayout * p = hbqt_par_QPlainTextDocumentLayout( 1 );
   if( p )
      ( p )->requestUpdate();
}

/* void setCursorWidth ( int width ) */
HB_FUNC( QT_QPLAINTEXTDOCUMENTLAYOUT_SETCURSORWIDTH )
{
   QPlainTextDocumentLayout * p = hbqt_par_QPlainTextDocumentLayout( 1 );
   if( p )
      ( p )->setCursorWidth( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
