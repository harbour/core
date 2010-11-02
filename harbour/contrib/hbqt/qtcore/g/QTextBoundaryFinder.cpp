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
 *  flags BoundaryReasons
 *  enum BoundaryReason { NotAtBoundary, StartWord, EndWord }
 *  enum BoundaryType { Grapheme, Word, Line, Sentence }
 */

/*
 *  Constructed[ 11/11 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QTextBoundaryFinder>


/*
 * QTextBoundaryFinder ()
 * QTextBoundaryFinder ( const QTextBoundaryFinder & other )
 * QTextBoundaryFinder ( BoundaryType type, const QString & string )
 * QTextBoundaryFinder ( BoundaryType type, const QChar * chars, int length, unsigned char * buffer = 0, int bufferSize = 0 )
 * ~QTextBoundaryFinder ()
 */

typedef struct
{
   QTextBoundaryFinder * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextBoundaryFinder;

HBQT_GC_FUNC( hbqt_gcRelease_QTextBoundaryFinder )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextBoundaryFinder * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextBoundaryFinder( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextBoundaryFinder * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextBoundaryFinder;
   p->type = HBQT_TYPE_QTextBoundaryFinder;

   return p;
}

HB_FUNC( QT_QTEXTBOUNDARYFINDER )
{
   QTextBoundaryFinder * pObj = NULL;

   pObj = new QTextBoundaryFinder() ;

   hb_retptrGC( hbqt_gcAllocate_QTextBoundaryFinder( ( void * ) pObj, true ) );
}

/* BoundaryReasons boundaryReasons () const */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_BOUNDARYREASONS )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retni( ( QTextBoundaryFinder::BoundaryReasons ) ( p )->boundaryReasons() );
}

/* bool isAtBoundary () const */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_ISATBOUNDARY )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retl( ( p )->isAtBoundary() );
}

/* bool isValid () const */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_ISVALID )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* int position () const */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_POSITION )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retni( ( p )->position() );
}

/* void setPosition ( int position ) */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_SETPOSITION )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      ( p )->setPosition( hb_parni( 2 ) );
}

/* QString string () const */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_STRING )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retstr_utf8( ( p )->string().toUtf8().data() );
}

/* void toEnd () */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TOEND )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      ( p )->toEnd();
}

/* int toNextBoundary () */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TONEXTBOUNDARY )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retni( ( p )->toNextBoundary() );
}

/* int toPreviousBoundary () */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TOPREVIOUSBOUNDARY )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retni( ( p )->toPreviousBoundary() );
}

/* void toStart () */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TOSTART )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      ( p )->toStart();
}

/* BoundaryType type () const */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TYPE )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retni( ( QTextBoundaryFinder::BoundaryType ) ( p )->type() );
}


#endif /* #if QT_VERSION >= 0x040500 */
