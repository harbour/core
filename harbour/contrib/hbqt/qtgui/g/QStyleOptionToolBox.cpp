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
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

/*
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionToolBox>


/*
 * QStyleOptionToolBox ()
 * QStyleOptionToolBox ( const QStyleOptionToolBox & other )
 */

typedef struct
{
   QStyleOptionToolBox * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionToolBox;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionToolBox )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionToolBox * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionToolBox( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionToolBox * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionToolBox;
   p->type = HBQT_TYPE_QStyleOptionToolBox;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONTOOLBOX )
{
   QStyleOptionToolBox * pObj = NULL;

   pObj = new QStyleOptionToolBox() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionToolBox( ( void * ) pObj, true ) );
}

/* QIcon icon */
HB_FUNC( QT_QSTYLEOPTIONTOOLBOX_ICON )
{
   QStyleOptionToolBox * p = hbqt_par_QStyleOptionToolBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon ), true ) );
}

/* QString text */
HB_FUNC( QT_QSTYLEOPTIONTOOLBOX_TEXT )
{
   QStyleOptionToolBox * p = hbqt_par_QStyleOptionToolBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text.toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
