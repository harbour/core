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
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextList>


/*
 * QTextList ()
 *
 */

typedef struct
{
   QPointer< QTextList > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextList;

HBQT_GC_FUNC( hbqt_gcRelease_QTextList )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextList( void * pObj, bool bNew )
{
   HBQT_GC_T_QTextList * p = ( HBQT_GC_T_QTextList * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTextList ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTextList >( ( QTextList * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextList;
   p->type = HBQT_TYPE_QTextList;

   return p;
}

HB_FUNC( QT_QTEXTLIST )
{

}

/* void add ( const QTextBlock & block ) */
HB_FUNC( QT_QTEXTLIST_ADD )
{
   QTextList * p = hbqt_par_QTextList( 1 );
   if( p )
      ( p )->add( *hbqt_par_QTextBlock( 2 ) );
}

/* int count () const */
HB_FUNC( QT_QTEXTLIST_COUNT )
{
   QTextList * p = hbqt_par_QTextList( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* QTextListFormat format () const */
HB_FUNC( QT_QTEXTLIST_FORMAT )
{
   QTextList * p = hbqt_par_QTextList( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextListFormat( new QTextListFormat( ( p )->format() ), true ) );
}

/* QTextBlock item ( int i ) const */
HB_FUNC( QT_QTEXTLIST_ITEM )
{
   QTextList * p = hbqt_par_QTextList( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->item( hb_parni( 2 ) ) ), true ) );
}

/* int itemNumber ( const QTextBlock & block ) const */
HB_FUNC( QT_QTEXTLIST_ITEMNUMBER )
{
   QTextList * p = hbqt_par_QTextList( 1 );
   if( p )
      hb_retni( ( p )->itemNumber( *hbqt_par_QTextBlock( 2 ) ) );
}

/* QString itemText ( const QTextBlock & block ) const */
HB_FUNC( QT_QTEXTLIST_ITEMTEXT )
{
   QTextList * p = hbqt_par_QTextList( 1 );
   if( p )
      hb_retstr_utf8( ( p )->itemText( *hbqt_par_QTextBlock( 2 ) ).toUtf8().data() );
}

/* void remove ( const QTextBlock & block ) */
HB_FUNC( QT_QTEXTLIST_REMOVE )
{
   QTextList * p = hbqt_par_QTextList( 1 );
   if( p )
      ( p )->remove( *hbqt_par_QTextBlock( 2 ) );
}

/* void removeItem ( int i ) */
HB_FUNC( QT_QTEXTLIST_REMOVEITEM )
{
   QTextList * p = hbqt_par_QTextList( 1 );
   if( p )
      ( p )->removeItem( hb_parni( 2 ) );
}

/* void setFormat ( const QTextListFormat & format ) */
HB_FUNC( QT_QTEXTLIST_SETFORMAT )
{
   QTextList * p = hbqt_par_QTextList( 1 );
   if( p )
      ( p )->setFormat( *hbqt_par_QTextListFormat( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
