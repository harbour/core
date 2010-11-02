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
 *  Constructed[ 3/3 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSpacerItem>


/* QSpacerItem ( int w, int h, QSizePolicy::Policy hPolicy = QSizePolicy::Minimum, QSizePolicy::Policy vPolicy = QSizePolicy::Minimum )
 * ~QSpacerItem ()
 */

typedef struct
{
   QSpacerItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSpacerItem;

HBQT_GC_FUNC( hbqt_gcRelease_QSpacerItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QSpacerItem * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSpacerItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QSpacerItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSpacerItem;
   p->type = HBQT_TYPE_QSpacerItem;

   return p;
}

HB_FUNC( QT_QSPACERITEM )
{
   QSpacerItem * pObj = NULL;

   if( hb_pcount() >= 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QSpacerItem( hb_parni( 1 ), hb_parni( 2 ),
                  HB_ISNUM( 3 ) ? ( QSizePolicy::Policy ) hb_parni( 3 ) : QSizePolicy::Minimum,
                  HB_ISNUM( 4 ) ? ( QSizePolicy::Policy ) hb_parni( 4 ) : QSizePolicy::Minimum );
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QSpacerItem( *hbqt_par_QSpacerItem( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QSpacerItem( ( void * ) pObj, true ) );
}

/* void changeSize ( int w, int h, QSizePolicy::Policy hPolicy = QSizePolicy::Minimum, QSizePolicy::Policy vPolicy = QSizePolicy::Minimum ) */
HB_FUNC( QT_QSPACERITEM_CHANGESIZE )
{
   QSpacerItem * p = hbqt_par_QSpacerItem( 1 );
   if( p )
      ( p )->changeSize( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QSizePolicy::Policy ) hb_parni( 4 ) : ( QSizePolicy::Policy ) QSizePolicy::Minimum ), ( HB_ISNUM( 5 ) ? ( QSizePolicy::Policy ) hb_parni( 5 ) : ( QSizePolicy::Policy ) QSizePolicy::Minimum ) );
}

/* virtual bool isEmpty () const */
HB_FUNC( QT_QSPACERITEM_ISEMPTY )
{
   QSpacerItem * p = hbqt_par_QSpacerItem( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* virtual QSpacerItem * spacerItem () */
HB_FUNC( QT_QSPACERITEM_SPACERITEM )
{
   QSpacerItem * p = hbqt_par_QSpacerItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSpacerItem( ( p )->spacerItem(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
