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
#include "hbqscintilla.h"

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <qscicommand.h>


/*
 *
 *
 */

typedef struct
{
   QsciCommand * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciCommand;

HBQT_GC_FUNC( hbqt_gcRelease_QsciCommand )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QsciCommand( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QsciCommand * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciCommand;
   p->type = HBQT_TYPE_QsciCommand;

   return p;
}

HB_FUNC( QT_QSCICOMMAND )
{

}

/* void setKey (int key) */
HB_FUNC( QT_QSCICOMMAND_SETKEY )
{
   QsciCommand * p = hbqt_par_QsciCommand( 1 );
   if( p )
      ( p )->setKey( hb_parni( 2 ) );
}

/* void setAlternateKey (int altkey) */
HB_FUNC( QT_QSCICOMMAND_SETALTERNATEKEY )
{
   QsciCommand * p = hbqt_par_QsciCommand( 1 );
   if( p )
      ( p )->setAlternateKey( hb_parni( 2 ) );
}

/* int key () const */
HB_FUNC( QT_QSCICOMMAND_KEY )
{
   QsciCommand * p = hbqt_par_QsciCommand( 1 );
   if( p )
      hb_retni( ( p )->key() );
}

/* int alternateKey () const */
HB_FUNC( QT_QSCICOMMAND_ALTERNATEKEY )
{
   QsciCommand * p = hbqt_par_QsciCommand( 1 );
   if( p )
      hb_retni( ( p )->alternateKey() );
}

/* QString description () const */
HB_FUNC( QT_QSCICOMMAND_DESCRIPTION )
{
   QsciCommand * p = hbqt_par_QsciCommand( 1 );
   if( p )
      hb_retstr_utf8( ( p )->description().toUtf8().data() );
}

/* bool validKey (int key) */
HB_FUNC( QT_QSCICOMMAND_VALIDKEY )
{
   QsciCommand * p = hbqt_par_QsciCommand( 1 );
   if( p )
      hb_retl( ( p )->validKey( hb_parni( 2 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
