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
 *  Constructed[ 0/0 [ 0% ] ]
 *
 */

#include <QtCore/QPointer>

#include <qscidocument.h>


/*
 * QsciDocument ()
 * QsciDocument (const QsciDocument &)
 *
 */

typedef struct
{
   QsciDocument * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciDocument;

HBQT_GC_FUNC( hbqt_gcRelease_QsciDocument )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QsciDocument * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QsciDocument( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QsciDocument * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciDocument;
   p->type = HBQT_TYPE_QsciDocument;

   return p;
}

HB_FUNC( QT_QSCIDOCUMENT )
{
   QsciDocument * pObj = NULL;

   if( HB_ISPOINTER( 1 ) )
   {
      pObj = new QsciDocument( *hbqt_par_QsciDocument( 1 ) ) ;
   }
   else
   {
      pObj = new QsciDocument() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QsciDocument( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
