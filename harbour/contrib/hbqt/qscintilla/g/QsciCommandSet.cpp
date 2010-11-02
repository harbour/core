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
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <qscicommandset.h>


/*
 *
 *
 */

typedef struct
{
   QsciCommandSet * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciCommandSet;

HBQT_GC_FUNC( hbqt_gcRelease_QsciCommandSet )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QsciCommandSet( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QsciCommandSet * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciCommandSet;
   p->type = HBQT_TYPE_QsciCommandSet;

   return p;
}

HB_FUNC( QT_QSCICOMMANDSET )
{
   //__HB_RETPTRGC__( new QsciCommandSet() );
}

/* bool readSettings (QSettings &qs, const char *prefix="/Scintilla") */
HB_FUNC( QT_QSCICOMMANDSET_READSETTINGS )
{
   QsciCommandSet * p = hbqt_par_QsciCommandSet( 1 );
   if( p )
      hb_retl( ( p )->readSettings( *hbqt_par_QSettings( 2 ), ( const char * ) hb_parc( 3 ) ) );
}

/* bool writeSettings (QSettings &qs, const char *prefix="/Scintilla") */
HB_FUNC( QT_QSCICOMMANDSET_WRITESETTINGS )
{
   QsciCommandSet * p = hbqt_par_QsciCommandSet( 1 );
   if( p )
      hb_retl( ( p )->writeSettings( *hbqt_par_QSettings( 2 ), ( const char * ) hb_parc( 3 ) ) );
}

/* QList< QsciCommand * > & commands () */
HB_FUNC( QT_QSCICOMMANDSET_COMMANDS )
{
   QsciCommandSet * p = hbqt_par_QsciCommandSet( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList< QsciCommand * > &( ( p )->commands() ), true ) );
}

/* void clearKeys () */
HB_FUNC( QT_QSCICOMMANDSET_CLEARKEYS )
{
   QsciCommandSet * p = hbqt_par_QsciCommandSet( 1 );
   if( p )
      ( p )->clearKeys();
}

/* void clearAlternateKeys () */
HB_FUNC( QT_QSCICOMMANDSET_CLEARALTERNATEKEYS )
{
   QsciCommandSet * p = hbqt_par_QsciCommandSet( 1 );
   if( p )
      ( p )->clearAlternateKeys();
}


#endif /* #if QT_VERSION >= 0x040500 */
