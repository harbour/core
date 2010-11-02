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
 *  enum SequenceFormat { NativeText, PortableText }
 *  enum SequenceMatch { NoMatch, PartialMatch, ExactMatch }
 *  enum StandardKey { AddTab, Back, Bold, Close, ..., ZoomOut }
 */

/*
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QKeySequence>


/*
 * QKeySequence ()
 * QKeySequence ( const QString & key )
 * QKeySequence ( int k1, int k2 = 0, int k3 = 0, int k4 = 0 )
 * QKeySequence ( const QKeySequence & keysequence )
 * QKeySequence ( StandardKey key )
 * ~QKeySequence ()
 */

typedef struct
{
   QKeySequence * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QKeySequence;

HBQT_GC_FUNC( hbqt_gcRelease_QKeySequence )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QKeySequence * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QKeySequence( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QKeySequence * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QKeySequence;
   p->type = HBQT_TYPE_QKeySequence;

   return p;
}

HB_FUNC( QT_QKEYSEQUENCE )
{
   QKeySequence * pObj = NULL;

   if( HB_ISPOINTER( 1 ) )
      pObj = new QKeySequence( *hbqt_par_QKeySequence( 1 ) ) ;
   else if( HB_ISCHAR( 1 ) )
      pObj = new QKeySequence( hbqt_par_QString( 1 ) ) ;
   else if( HB_ISNUM( 1 ) )
      pObj = new QKeySequence( hb_parni( 1 ) ) ;
   else
      pObj = new QKeySequence() ;

   hb_retptrGC( hbqt_gcAllocate_QKeySequence( ( void * ) pObj, true ) );
}

/* uint count () const */
HB_FUNC( QT_QKEYSEQUENCE_COUNT )
{
   QKeySequence * p = hbqt_par_QKeySequence( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* bool isEmpty () const */
HB_FUNC( QT_QKEYSEQUENCE_ISEMPTY )
{
   QKeySequence * p = hbqt_par_QKeySequence( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* SequenceMatch matches ( const QKeySequence & seq ) const */
HB_FUNC( QT_QKEYSEQUENCE_MATCHES )
{
   QKeySequence * p = hbqt_par_QKeySequence( 1 );
   if( p )
      hb_retni( ( QKeySequence::SequenceMatch ) ( p )->matches( *hbqt_par_QKeySequence( 2 ) ) );
}

/* QString toString ( SequenceFormat format = PortableText ) const */
HB_FUNC( QT_QKEYSEQUENCE_TOSTRING )
{
   QKeySequence * p = hbqt_par_QKeySequence( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toString( ( HB_ISNUM( 2 ) ? ( QKeySequence::SequenceFormat ) hb_parni( 2 ) : ( QKeySequence::SequenceFormat ) QKeySequence::PortableText ) ).toUtf8().data() );
}

/* QKeySequence fromString ( const QString & str, SequenceFormat format = PortableText ) */
HB_FUNC( QT_QKEYSEQUENCE_FROMSTRING )
{
   QKeySequence * p = hbqt_par_QKeySequence( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QKeySequence( new QKeySequence( ( p )->fromString( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( QKeySequence::SequenceFormat ) hb_parni( 3 ) : ( QKeySequence::SequenceFormat ) QKeySequence::PortableText ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QList<QKeySequence> keyBindings ( StandardKey key ) */
HB_FUNC( QT_QKEYSEQUENCE_KEYBINDINGS )
{
   QKeySequence * p = hbqt_par_QKeySequence( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QKeySequence>( ( p )->keyBindings( ( QKeySequence::StandardKey ) hb_parni( 2 ) ) ), true ) );
}

/* QKeySequence mnemonic ( const QString & text ) */
HB_FUNC( QT_QKEYSEQUENCE_MNEMONIC )
{
   QKeySequence * p = hbqt_par_QKeySequence( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QKeySequence( new QKeySequence( ( p )->mnemonic( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
