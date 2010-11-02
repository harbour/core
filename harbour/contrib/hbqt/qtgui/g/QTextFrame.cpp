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
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //iterator begin () const
 *  //iterator end () const
 */

#include <QtCore/QPointer>

#include <QtGui/QTextFrame>
#include <QtGui/QTextCursor>


/*
 * QTextFrame ( QTextDocument * document )
 * ~QTextFrame ()
 */

typedef struct
{
   QPointer< QTextFrame > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextFrame;

HBQT_GC_FUNC( hbqt_gcRelease_QTextFrame )
{
   QTextFrame  * ph = NULL;
   HBQT_GC_T_QTextFrame * p = ( HBQT_GC_T_QTextFrame * ) Cargo;

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

void * hbqt_gcAllocate_QTextFrame( void * pObj, bool bNew )
{
   HBQT_GC_T_QTextFrame * p = ( HBQT_GC_T_QTextFrame * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTextFrame ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTextFrame >( ( QTextFrame * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextFrame;
   p->type = HBQT_TYPE_QTextFrame;

   return p;
}

HB_FUNC( QT_QTEXTFRAME )
{
   QTextFrame * pObj = NULL;

   pObj = new QTextFrame( hbqt_par_QTextDocument( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( void * ) pObj, true ) );
}

/* QList<QTextFrame *> childFrames () const */
HB_FUNC( QT_QTEXTFRAME_CHILDFRAMES )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTextFrame *>( ( p )->childFrames() ), true ) );
}

/* QTextCursor firstCursorPosition () const */
HB_FUNC( QT_QTEXTFRAME_FIRSTCURSORPOSITION )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->firstCursorPosition() ), true ) );
}

/* int firstPosition () const */
HB_FUNC( QT_QTEXTFRAME_FIRSTPOSITION )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retni( ( p )->firstPosition() );
}

/* QTextFrameFormat frameFormat () const */
HB_FUNC( QT_QTEXTFRAME_FRAMEFORMAT )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFrameFormat( new QTextFrameFormat( ( p )->frameFormat() ), true ) );
}

/* QTextCursor lastCursorPosition () const */
HB_FUNC( QT_QTEXTFRAME_LASTCURSORPOSITION )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->lastCursorPosition() ), true ) );
}

/* int lastPosition () const */
HB_FUNC( QT_QTEXTFRAME_LASTPOSITION )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retni( ( p )->lastPosition() );
}

/* QTextFrame * parentFrame () const */
HB_FUNC( QT_QTEXTFRAME_PARENTFRAME )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->parentFrame(), false ) );
}

/* void setFrameFormat ( const QTextFrameFormat & format ) */
HB_FUNC( QT_QTEXTFRAME_SETFRAMEFORMAT )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      ( p )->setFrameFormat( *hbqt_par_QTextFrameFormat( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
