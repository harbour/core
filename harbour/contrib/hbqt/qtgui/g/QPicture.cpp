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
 *  Constructed[ 11/11 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QPicture>


/*
 * QPicture ( int formatVersion = -1 )
 * QPicture ( const QPicture & pic )
 * ~QPicture ()
 */

typedef struct
{
   QPicture * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPicture;

HBQT_GC_FUNC( hbqt_gcRelease_QPicture )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QPicture * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPicture( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPicture * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPicture;
   p->type = HBQT_TYPE_QPicture;

   return p;
}

HB_FUNC( QT_QPICTURE )
{
   QPicture * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj = new QPicture( hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPicture( *hbqt_par_QPicture( 1 ) ) ;
   }
   else
   {
      pObj = new QPicture() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPicture( ( void * ) pObj, true ) );
}

/* QRect boundingRect () const */
HB_FUNC( QT_QPICTURE_BOUNDINGRECT )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect() ), true ) );
}

/* const char * data () const */
HB_FUNC( QT_QPICTURE_DATA )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retc( ( p )->data() );
}

/* bool isNull () const */
HB_FUNC( QT_QPICTURE_ISNULL )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* bool load ( const QString & fileName, const char * format = 0 ) */
HB_FUNC( QT_QPICTURE_LOAD )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->load( hb_parstr_utf8( 2, &pText, NULL ), ( const char * ) hb_parc( 3 ) ) );
      hb_strfree( pText );
   }
}

/* bool load ( QIODevice * dev, const char * format = 0 ) */
HB_FUNC( QT_QPICTURE_LOAD_1 )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retl( ( p )->load( hbqt_par_QIODevice( 2 ), ( const char * ) hb_parc( 3 ) ) );
}

/* bool play ( QPainter * painter ) */
HB_FUNC( QT_QPICTURE_PLAY )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retl( ( p )->play( hbqt_par_QPainter( 2 ) ) );
}

/* bool save ( const QString & fileName, const char * format = 0 ) */
HB_FUNC( QT_QPICTURE_SAVE )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->save( hb_parstr_utf8( 2, &pText, NULL ), ( const char * ) hb_parc( 3 ) ) );
      hb_strfree( pText );
   }
}

/* bool save ( QIODevice * dev, const char * format = 0 ) */
HB_FUNC( QT_QPICTURE_SAVE_1 )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retl( ( p )->save( hbqt_par_QIODevice( 2 ), ( const char * ) hb_parc( 3 ) ) );
}

/* void setBoundingRect ( const QRect & r ) */
HB_FUNC( QT_QPICTURE_SETBOUNDINGRECT )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      ( p )->setBoundingRect( *hbqt_par_QRect( 2 ) );
}

/* virtual void setData ( const char * data, uint size ) */
HB_FUNC( QT_QPICTURE_SETDATA )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      ( p )->setData( ( const char * ) hb_parc( 2 ), hb_parni( 3 ) );
}

/* uint size () const */
HB_FUNC( QT_QPICTURE_SIZE )
{
   QPicture * p = hbqt_par_QPicture( 1 );
   if( p )
      hb_retni( ( p )->size() );
}


#endif /* #if QT_VERSION >= 0x040500 */
