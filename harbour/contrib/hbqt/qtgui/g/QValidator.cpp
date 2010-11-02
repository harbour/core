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
 *  enum State { Invalid, Intermediate, Acceptable }
 */

/*
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // virtual void fixup ( QString & input ) const
 *  // virtual State validate ( QString & input, int & pos ) const = 0
 */

#include <QtCore/QPointer>

#include <QtGui/QValidator>


/* QValidator ( QObject * parent )
 * ~QValidator ()
 */

typedef struct
{
   QPointer< QValidator > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QValidator;

HBQT_GC_FUNC( hbqt_gcRelease_QValidator )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QValidator( void * pObj, bool bNew )
{
   HBQT_GC_T_QValidator * p = ( HBQT_GC_T_QValidator * ) hb_gcAllocate( sizeof( HBQT_GC_T_QValidator ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QValidator >( ( QValidator * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QValidator;
   p->type = HBQT_TYPE_QValidator;

   return p;
}

HB_FUNC( QT_QVALIDATOR )
{
   // __HB_RETPTRGC__( new QValidator() );
}

/* QLocale locale () const */
HB_FUNC( QT_QVALIDATOR_LOCALE )
{
   QValidator * p = hbqt_par_QValidator( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->locale() ), true ) );
}

/* void setLocale ( const QLocale & locale ) */
HB_FUNC( QT_QVALIDATOR_SETLOCALE )
{
   QValidator * p = hbqt_par_QValidator( 1 );
   if( p )
      ( p )->setLocale( *hbqt_par_QLocale( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
