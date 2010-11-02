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
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QGenericArgument>


/*
 * QGenericArgument ( const char * name = 0, const void * data = 0 )
 *
 */

typedef struct
{
   QGenericArgument * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGenericArgument;

HBQT_GC_FUNC( hbqt_gcRelease_QGenericArgument )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QGenericArgument * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGenericArgument( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGenericArgument * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGenericArgument;
   p->type = HBQT_TYPE_QGenericArgument;

   return p;
}

HB_FUNC( QT_QGENERICARGUMENT )
{
   QGenericArgument * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QGenericArgument( hb_parc( 1 ), 0 ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QGenericArgument( hb_parc( 1 ), hb_parptr( 2 ) ) ;
   }
   else
   {
      pObj = new QGenericArgument() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGenericArgument( ( void * ) pObj, true ) );
}

/* void * data () const */
HB_FUNC( QT_QGENERICARGUMENT_DATA )
{
   QGenericArgument * p = hbqt_par_QGenericArgument( 1 );
   if( p )
      ( p )->data();
}

/* const char * name () const */
HB_FUNC( QT_QGENERICARGUMENT_NAME )
{
   QGenericArgument * p = hbqt_par_QGenericArgument( 1 );
   if( p )
      hb_retc( ( p )->name() );
}


#endif /* #if QT_VERSION >= 0x040500 */
