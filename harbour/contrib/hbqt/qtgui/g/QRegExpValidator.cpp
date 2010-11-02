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
 *  //virtual QValidator::State validate ( QString & input, int & pos ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QRegExpValidator>


/* QRegExpValidator ( QObject * parent )
 * QRegExpValidator ( const QRegExp & rx, QObject * parent )
 * ~QRegExpValidator ()
 */

typedef struct
{
   QPointer< QRegExpValidator > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QRegExpValidator;

HBQT_GC_FUNC( hbqt_gcRelease_QRegExpValidator )
{
   HBQT_GC_T_QRegExpValidator * p = ( HBQT_GC_T_QRegExpValidator * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QRegExpValidator * ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QRegExpValidator( void * pObj, bool bNew )
{
   HBQT_GC_T_QRegExpValidator * p = ( HBQT_GC_T_QRegExpValidator * ) hb_gcAllocate( sizeof( HBQT_GC_T_QRegExpValidator ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QRegExpValidator >( ( QRegExpValidator * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRegExpValidator;
   p->type = HBQT_TYPE_QRegExpValidator;

   return p;
}

HB_FUNC( QT_QREGEXPVALIDATOR )
{
   QRegExpValidator * pObj = NULL;

   if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QRegExpValidator( *hbqt_par_QRegExp( 1 ), hbqt_par_QObject( 2 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QRegExpValidator( hbqt_par_QObject( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QRegExpValidator( ( void * ) pObj, true ) );
}

/* const QRegExp & regExp () const */
HB_FUNC( QT_QREGEXPVALIDATOR_REGEXP )
{
   QRegExpValidator * p = hbqt_par_QRegExpValidator( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegExp( new QRegExp( ( p )->regExp() ), true ) );
}

/* void setRegExp ( const QRegExp & rx ) */
HB_FUNC( QT_QREGEXPVALIDATOR_SETREGEXP )
{
   QRegExpValidator * p = hbqt_par_QRegExpValidator( 1 );
   if( p )
      ( p )->setRegExp( *hbqt_par_QRegExp( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
