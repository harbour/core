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
 *  Constructed[ 0/0 [ 0% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //virtual void blockFormatChanged ( const QTextBlock & block )
 *  //virtual void blockInserted ( const QTextBlock & block )
 *  //QList<QTextBlock> blockList () const
 *  //virtual void blockRemoved ( const QTextBlock & block )
 */

#include <QtCore/QPointer>

#include <QtGui/QTextBlockGroup>


/*
 * QTextBlockGroup ( QTextDocument * document )
 */

typedef struct
{
   QPointer< QTextBlockGroup > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextBlockGroup;

HBQT_GC_FUNC( hbqt_gcRelease_QTextBlockGroup )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextBlockGroup( void * pObj, bool bNew )
{
   HBQT_GC_T_QTextBlockGroup * p = ( HBQT_GC_T_QTextBlockGroup * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTextBlockGroup ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTextBlockGroup >( ( QTextBlockGroup * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextBlockGroup;
   p->type = HBQT_TYPE_QTextBlockGroup;

   return p;
}

HB_FUNC( QT_QTEXTBLOCKGROUP )
{
   //__HB_RETPTRGC__( new QTextBlockGroup( hbqt_par_QTextDocument( 1 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
