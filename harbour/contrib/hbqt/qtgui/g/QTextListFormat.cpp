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
 *  enum Style { ListDisc, ListCircle, ListSquare, ListDecimal, ListLowerAlpha, ListUpperAlpha }
 */

/*
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextListFormat>


/* QTextListFormat ()
 *
 */

typedef struct
{
   QTextListFormat * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextListFormat;

HBQT_GC_FUNC( hbqt_gcRelease_QTextListFormat )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextListFormat * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextListFormat( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextListFormat * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextListFormat;
   p->type = HBQT_TYPE_QTextListFormat;

   return p;
}

HB_FUNC( QT_QTEXTLISTFORMAT )
{
   QTextListFormat * pObj = NULL;

   pObj = new QTextListFormat() ;

   hb_retptrGC( hbqt_gcAllocate_QTextListFormat( ( void * ) pObj, true ) );
}

/* int indent () const */
HB_FUNC( QT_QTEXTLISTFORMAT_INDENT )
{
   QTextListFormat * p = hbqt_par_QTextListFormat( 1 );
   if( p )
      hb_retni( ( p )->indent() );
}

/* bool isValid () const */
HB_FUNC( QT_QTEXTLISTFORMAT_ISVALID )
{
   QTextListFormat * p = hbqt_par_QTextListFormat( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* void setIndent ( int indentation ) */
HB_FUNC( QT_QTEXTLISTFORMAT_SETINDENT )
{
   QTextListFormat * p = hbqt_par_QTextListFormat( 1 );
   if( p )
      ( p )->setIndent( hb_parni( 2 ) );
}

/* void setStyle ( Style style ) */
HB_FUNC( QT_QTEXTLISTFORMAT_SETSTYLE )
{
   QTextListFormat * p = hbqt_par_QTextListFormat( 1 );
   if( p )
      ( p )->setStyle( ( QTextListFormat::Style ) hb_parni( 2 ) );
}

/* Style style () const */
HB_FUNC( QT_QTEXTLISTFORMAT_STYLE )
{
   QTextListFormat * p = hbqt_par_QTextListFormat( 1 );
   if( p )
      hb_retni( ( QTextListFormat::Style ) ( p )->style() );
}


#endif /* #if QT_VERSION >= 0x040500 */
