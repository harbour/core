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


#include "hbqt_hbqsciscintilla.h"


/*
 *
 */

typedef struct
{
   QPointer< HBQsciScintilla > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQsciScintilla;

HBQT_GC_FUNC( hbqt_gcRelease_HBQsciScintilla )
{
   HBQT_GC_T_HBQsciScintilla * p = ( HBQT_GC_T_HBQsciScintilla * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      HBQsciScintilla * ph = p->ph;
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

void * hbqt_gcAllocate_HBQsciScintilla( void * pObj, bool bNew )
{
   HBQT_GC_T_HBQsciScintilla * p = ( HBQT_GC_T_HBQsciScintilla * ) hb_gcAllocate( sizeof( HBQT_GC_T_HBQsciScintilla ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< HBQsciScintilla >( ( HBQsciScintilla * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQsciScintilla;
   p->type = HBQT_TYPE_HBQsciScintilla;

   return p;
}

HB_FUNC( QT_HBQSCISCINTILLA )
{
   HBQsciScintilla * pObj = NULL;

   pObj = new HBQsciScintilla( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_HBQsciScintilla( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
