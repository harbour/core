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
 *  enum IconType { Computer, Desktop, Trashcan, Network, ..., File }
 */

/*
 *  Constructed[ 3/3 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QFileIconProvider>


/* QFileIconProvider ()
 * virtual ~QFileIconProvider ()
 */

typedef struct
{
   QFileIconProvider * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFileIconProvider;

HBQT_GC_FUNC( hbqt_gcRelease_QFileIconProvider )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QFileIconProvider * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFileIconProvider( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QFileIconProvider * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFileIconProvider;
   p->type = HBQT_TYPE_QFileIconProvider;

   return p;
}

HB_FUNC( QT_QFILEICONPROVIDER )
{
   QFileIconProvider * pObj = NULL;

   pObj = new QFileIconProvider() ;

   hb_retptrGC( hbqt_gcAllocate_QFileIconProvider( ( void * ) pObj, true ) );
}

/* virtual QIcon icon ( IconType type ) const */
HB_FUNC( QT_QFILEICONPROVIDER_ICON )
{
   QFileIconProvider * p = hbqt_par_QFileIconProvider( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon( ( QFileIconProvider::IconType ) hb_parni( 2 ) ) ), true ) );
}

/* virtual QIcon icon ( const QFileInfo & info ) const */
HB_FUNC( QT_QFILEICONPROVIDER_ICON_1 )
{
   QFileIconProvider * p = hbqt_par_QFileIconProvider( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon( *hbqt_par_QFileInfo( 2 ) ) ), true ) );
}

/* virtual QString type ( const QFileInfo & info ) const */
HB_FUNC( QT_QFILEICONPROVIDER_TYPE )
{
   QFileIconProvider * p = hbqt_par_QFileIconProvider( 1 );
   if( p )
      hb_retstr_utf8( ( p )->type( *hbqt_par_QFileInfo( 2 ) ).toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
