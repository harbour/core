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
 *  enum Type { VariableLength, FixedLength, PercentageLength }
 */

/*
 *  Constructed[ 3/3 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextLength>


/*
 * QTextLength ()
 * QTextLength ( Type type, qreal value )
 */

typedef struct
{
   QTextLength * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextLength;

HBQT_GC_FUNC( hbqt_gcRelease_QTextLength )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QTextLength * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextLength( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextLength * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextLength;
   p->type = HBQT_TYPE_QTextLength;

   return p;
}

HB_FUNC( QT_QTEXTLENGTH )
{
   QTextLength * pObj = NULL;

   pObj = new QTextLength() ;

   hb_retptrGC( hbqt_gcAllocate_QTextLength( ( void * ) pObj, true ) );
}

/* qreal rawValue () const */
HB_FUNC( QT_QTEXTLENGTH_RAWVALUE )
{
   QTextLength * p = hbqt_par_QTextLength( 1 );
   if( p )
      hb_retnd( ( p )->rawValue() );
}

/* Type type () const */
HB_FUNC( QT_QTEXTLENGTH_TYPE )
{
   QTextLength * p = hbqt_par_QTextLength( 1 );
   if( p )
      hb_retni( ( QTextLength::Type ) ( p )->type() );
}

/* qreal value ( qreal maximumLength ) const */
HB_FUNC( QT_QTEXTLENGTH_VALUE )
{
   QTextLength * p = hbqt_par_QTextLength( 1 );
   if( p )
      hb_retnd( ( p )->value( hb_parnd( 2 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
