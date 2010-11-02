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
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleFactory>


/*
 * QStyleFactory ()
 * ~QStyleFactory()
 */

typedef struct
{
   QStyleFactory * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleFactory;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleFactory )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleFactory * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleFactory( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleFactory * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleFactory;
   p->type = HBQT_TYPE_QStyleFactory;

   return p;
}

HB_FUNC( QT_QSTYLEFACTORY )
{
   QStyleFactory * pObj = NULL;

   pObj = new QStyleFactory() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleFactory( ( void * ) pObj, true ) );
}

/* QStyle * create ( const QString & key ) */
HB_FUNC( QT_QSTYLEFACTORY_CREATE )
{
   QStyleFactory * p = hbqt_par_QStyleFactory( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->create( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/* QStringList keys () */
HB_FUNC( QT_QSTYLEFACTORY_KEYS )
{
   QStyleFactory * p = hbqt_par_QStyleFactory( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->keys() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
