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
 *  Constructed[ 3/3 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSyntaxHighlighter>
#include "hbqt_hbqsyntaxhighlighter.h"

/*
 * QSyntaxHighlighter ( QObject * parent )
 * QSyntaxHighlighter ( QTextDocument * parent )
 * QSyntaxHighlighter ( QTextEdit * parent )
 * virtual ~QSyntaxHighlighter ()
 */

typedef struct
{
   QPointer< QSyntaxHighlighter > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSyntaxHighlighter;

HBQT_GC_FUNC( hbqt_gcRelease_QSyntaxHighlighter )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QSyntaxHighlighter( void * pObj, bool bNew )
{
   HBQT_GC_T_QSyntaxHighlighter * p = ( HBQT_GC_T_QSyntaxHighlighter * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSyntaxHighlighter ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSyntaxHighlighter >( ( QSyntaxHighlighter * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSyntaxHighlighter;
   p->type = HBQT_TYPE_QSyntaxHighlighter;

   return p;
}

HB_FUNC( QT_QSYNTAXHIGHLIGHTER )
{

}

/* QTextDocument * document () const */
HB_FUNC( QT_QSYNTAXHIGHLIGHTER_DOCUMENT )
{
   QSyntaxHighlighter * p = hbqt_par_QSyntaxHighlighter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) );
}

/* void setDocument ( QTextDocument * doc ) */
HB_FUNC( QT_QSYNTAXHIGHLIGHTER_SETDOCUMENT )
{
   QSyntaxHighlighter * p = hbqt_par_QSyntaxHighlighter( 1 );
   if( p )
      ( p )->setDocument( hbqt_par_QTextDocument( 2 ) );
}

/* void rehighlight () */
HB_FUNC( QT_QSYNTAXHIGHLIGHTER_REHIGHLIGHT )
{
   QSyntaxHighlighter * p = hbqt_par_QSyntaxHighlighter( 1 );
   if( p )
      ( p )->rehighlight();
}


#endif /* #if QT_VERSION >= 0x040500 */
