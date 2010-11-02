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
 *  Constructed[ 12/12 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QFontInfo>


/*
 * QFontInfo ( const QFont & font )
 * QFontInfo ( const QFontInfo & fi )
 * ~QFontInfo ()
 */

typedef struct
{
   QFontInfo * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFontInfo;

HBQT_GC_FUNC( hbqt_gcRelease_QFontInfo )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QFontInfo * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QFontInfo( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QFontInfo * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontInfo;
   p->type = HBQT_TYPE_QFontInfo;

   return p;
}

HB_FUNC( QT_QFONTINFO )
{
   QFontInfo * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QFontInfo( *hbqt_par_QFontInfo( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QFontInfo( *hbqt_par_QFont( 2 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFontInfo( ( void * ) pObj, true ) );
}

/* bool bold () const */
HB_FUNC( QT_QFONTINFO_BOLD )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retl( ( p )->bold() );
}

/* bool exactMatch () const */
HB_FUNC( QT_QFONTINFO_EXACTMATCH )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retl( ( p )->exactMatch() );
}

/* QString family () const */
HB_FUNC( QT_QFONTINFO_FAMILY )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retstr_utf8( ( p )->family().toUtf8().data() );
}

/* bool fixedPitch () const */
HB_FUNC( QT_QFONTINFO_FIXEDPITCH )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retl( ( p )->fixedPitch() );
}

/* bool italic () const */
HB_FUNC( QT_QFONTINFO_ITALIC )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retl( ( p )->italic() );
}

/* int pixelSize () const */
HB_FUNC( QT_QFONTINFO_PIXELSIZE )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retni( ( p )->pixelSize() );
}

/* int pointSize () const */
HB_FUNC( QT_QFONTINFO_POINTSIZE )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retni( ( p )->pointSize() );
}

/* qreal pointSizeF () const */
HB_FUNC( QT_QFONTINFO_POINTSIZEF )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retnd( ( p )->pointSizeF() );
}

/* bool rawMode () const */
HB_FUNC( QT_QFONTINFO_RAWMODE )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retl( ( p )->rawMode() );
}

/* QFont::Style style () const */
HB_FUNC( QT_QFONTINFO_STYLE )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retni( ( QFont::Style ) ( p )->style() );
}

/* QFont::StyleHint styleHint () const */
HB_FUNC( QT_QFONTINFO_STYLEHINT )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retni( ( QFont::StyleHint ) ( p )->styleHint() );
}

/* int weight () const */
HB_FUNC( QT_QFONTINFO_WEIGHT )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retni( ( p )->weight() );
}


#endif /* #if QT_VERSION >= 0x040500 */
