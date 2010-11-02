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

#include <QtGui/QTextObject>


/*
 * QTextObject ()
 */

typedef struct
{
   QPointer< QTextObject > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextObject;

HBQT_GC_FUNC( hbqt_gcRelease_QTextObject )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextObject( void * pObj, bool bNew )
{
   HBQT_GC_T_QTextObject * p = ( HBQT_GC_T_QTextObject * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTextObject ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTextObject >( ( QTextObject * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextObject;
   p->type = HBQT_TYPE_QTextObject;

   return p;
}

HB_FUNC( QT_QTEXTOBJECT )
{

}

/* QTextDocument * document () const */
HB_FUNC( QT_QTEXTOBJECT_DOCUMENT )
{
   QTextObject * p = hbqt_par_QTextObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) );
}

/* QTextFormat format () const */
HB_FUNC( QT_QTEXTOBJECT_FORMAT )
{
   QTextObject * p = hbqt_par_QTextObject( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFormat( new QTextFormat( ( p )->format() ), true ) );
}

/* int formatIndex () const */
HB_FUNC( QT_QTEXTOBJECT_FORMATINDEX )
{
   QTextObject * p = hbqt_par_QTextObject( 1 );
   if( p )
      hb_retni( ( p )->formatIndex() );
}

/* int objectIndex () const */
HB_FUNC( QT_QTEXTOBJECT_OBJECTINDEX )
{
   QTextObject * p = hbqt_par_QTextObject( 1 );
   if( p )
      hb_retni( ( p )->objectIndex() );
}


#endif /* #if QT_VERSION >= 0x040500 */
