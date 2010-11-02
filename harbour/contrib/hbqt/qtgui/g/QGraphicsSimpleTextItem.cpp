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
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsSimpleTextItem>
#include <QtGui/QFont>


/*
 * QGraphicsSimpleTextItem ( QGraphicsItem * parent = 0 )
 * QGraphicsSimpleTextItem ( const QString & text, QGraphicsItem * parent = 0 )
 * ~QGraphicsSimpleTextItem ()
 */

typedef struct
{
   QGraphicsSimpleTextItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsSimpleTextItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsSimpleTextItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QGraphicsSimpleTextItem * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsSimpleTextItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsSimpleTextItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsSimpleTextItem;
   p->type = HBQT_TYPE_QGraphicsSimpleTextItem;

   return p;
}

HB_FUNC( QT_QGRAPHICSSIMPLETEXTITEM )
{
   QGraphicsSimpleTextItem * pObj = NULL;

   if( hb_pcount() >= 1 )
   {
      if( HB_ISCHAR( 1 ) )
      {
         pObj = new QGraphicsSimpleTextItem( hbqt_par_QString( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsItem( 1 ) : 0 ) ) ;
      }
      else if( HB_ISPOINTER( 1 ) )
      {
         pObj = new QGraphicsSimpleTextItem( hbqt_par_QGraphicsItem( 1 ) ) ;
      }
      else
      {
         pObj = new QGraphicsSimpleTextItem() ;
      }
   }
   else
   {
      pObj = new QGraphicsSimpleTextItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsSimpleTextItem( ( void * ) pObj, true ) );
}

/* QFont font () const */
HB_FUNC( QT_QGRAPHICSSIMPLETEXTITEM_FONT )
{
   QGraphicsSimpleTextItem * p = hbqt_par_QGraphicsSimpleTextItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
}

/* void setFont ( const QFont & font ) */
HB_FUNC( QT_QGRAPHICSSIMPLETEXTITEM_SETFONT )
{
   QGraphicsSimpleTextItem * p = hbqt_par_QGraphicsSimpleTextItem( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
}

/* void setText ( const QString & text ) */
HB_FUNC( QT_QGRAPHICSSIMPLETEXTITEM_SETTEXT )
{
   QGraphicsSimpleTextItem * p = hbqt_par_QGraphicsSimpleTextItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QString text () const */
HB_FUNC( QT_QGRAPHICSSIMPLETEXTITEM_TEXT )
{
   QGraphicsSimpleTextItem * p = hbqt_par_QGraphicsSimpleTextItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
