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
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextFragment>


/*
 * QTextFragment ()
 * QTextFragment ( const QTextFragment & other )
 */

typedef struct
{
   QTextFragment * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextFragment;

HBQT_GC_FUNC( hbqt_gcRelease_QTextFragment )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QTextFragment * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextFragment( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextFragment * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextFragment;
   p->type = HBQT_TYPE_QTextFragment;

   return p;
}

HB_FUNC( QT_QTEXTFRAGMENT )
{
   QTextFragment * pObj = NULL;

   pObj = new QTextFragment() ;

   hb_retptrGC( hbqt_gcAllocate_QTextFragment( ( void * ) pObj, true ) );
}

/* QTextCharFormat charFormat () const */
HB_FUNC( QT_QTEXTFRAGMENT_CHARFORMAT )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->charFormat() ), true ) );
}

/* int charFormatIndex () const */
HB_FUNC( QT_QTEXTFRAGMENT_CHARFORMATINDEX )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retni( ( p )->charFormatIndex() );
}

/* bool contains ( int position ) const */
HB_FUNC( QT_QTEXTFRAGMENT_CONTAINS )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retl( ( p )->contains( hb_parni( 2 ) ) );
}

/* bool isValid () const */
HB_FUNC( QT_QTEXTFRAGMENT_ISVALID )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* int length () const */
HB_FUNC( QT_QTEXTFRAGMENT_LENGTH )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retni( ( p )->length() );
}

/* int position () const */
HB_FUNC( QT_QTEXTFRAGMENT_POSITION )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retni( ( p )->position() );
}

/* QString text () const */
HB_FUNC( QT_QTEXTFRAGMENT_TEXT )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}


#endif /* #if QT_VERSION >= 0x040500 */
