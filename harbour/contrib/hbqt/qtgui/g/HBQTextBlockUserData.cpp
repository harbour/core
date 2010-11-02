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

#include <QtGui/QTextBlockUserData>
#include "hbqt_hbqsyntaxhighlighter.h"

/*
 *  HBQTextBlockUserData ()
 * ~HBQTextBlockUserData ()
 */

typedef struct
{
   HBQTextBlockUserData * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQTextBlockUserData;

HBQT_GC_FUNC( hbqt_gcRelease_HBQTextBlockUserData )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( HBQTextBlockUserData * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_HBQTextBlockUserData( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( HBQTextBlockUserData * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQTextBlockUserData;
   p->type = HBQT_TYPE_HBQTextBlockUserData;

   return p;
}

HB_FUNC( QT_HBQTEXTBLOCKUSERDATA )
{
   HBQTextBlockUserData * pObj = NULL;

   pObj = new HBQTextBlockUserData() ;

   hb_retptrGC( hbqt_gcAllocate_HBQTextBlockUserData( ( void * ) pObj, true ) );
}

/* int hbSetState( int state ) */
HB_FUNC( QT_HBQTEXTBLOCKUSERDATA_HBSETSTATE )
{
   HBQTextBlockUserData * p = hbqt_par_HBQTextBlockUserData( 1 );
   if( p )
      hb_retni( ( p )->hbSetState( hb_parni( 2 ) ) );
}

/* int hbState() */
HB_FUNC( QT_HBQTEXTBLOCKUSERDATA_HBSTATE )
{
   HBQTextBlockUserData * p = hbqt_par_HBQTextBlockUserData( 1 );
   if( p )
      hb_retni( ( p )->hbState() );
}


#endif /* #if QT_VERSION >= 0x040500 */
