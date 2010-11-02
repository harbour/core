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
#include "hbqscintilla.h"

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <qscistyledtext.h>


/*
 * QsciStyledText (const QString &text, int style)
 * QsciStyledText (const QString &text, const QsciStyle &style)
 *
 */

typedef struct
{
   QsciStyledText * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciStyledText;

HBQT_GC_FUNC( hbqt_gcRelease_QsciStyledText )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QsciStyledText * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QsciStyledText( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QsciStyledText * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciStyledText;
   p->type = HBQT_TYPE_QsciStyledText;

   return p;
}

HB_FUNC( QT_QSCISTYLEDTEXT )
{
   QsciStyledText * pObj = NULL;

   if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QsciStyledText( hbqt_par_QString( 1 ), hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QsciStyledText( hbqt_par_QString( 1 ), *hbqt_par_QsciStyle( 2 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QsciStyledText( ( void * ) pObj, true ) );
}

/* const QString & text () const */
HB_FUNC( QT_QSCISTYLEDTEXT_TEXT )
{
   QsciStyledText * p = hbqt_par_QsciStyledText( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* int style () const */
HB_FUNC( QT_QSCISTYLEDTEXT_STYLE )
{
   QsciStyledText * p = hbqt_par_QsciStyledText( 1 );
   if( p )
      hb_retni( ( p )->style() );
}


#endif /* #if QT_VERSION >= 0x040500 */
