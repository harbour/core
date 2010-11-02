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
 *  enum AttributeType { TextFormat, Cursor, Language, Ruby }
 */

/*
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // const QList<Attribute> & attributes () const
 */

#include <QtCore/QPointer>

#include <QtGui/QInputMethodEvent>


/* QInputMethodEvent ()
 * QInputMethodEvent ( const QString & preeditText, const QList<Attribute> & attributes )
 * QInputMethodEvent ( const QInputMethodEvent & other )
 */

typedef struct
{
   QInputMethodEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QInputMethodEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QInputMethodEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QInputMethodEvent * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QInputMethodEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QInputMethodEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QInputMethodEvent;
   p->type = HBQT_TYPE_QInputMethodEvent;

   return p;
}

HB_FUNC( QT_QINPUTMETHODEVENT )
{
   QInputMethodEvent * pObj = NULL;

   pObj = new QInputMethodEvent() ;

   hb_retptrGC( hbqt_gcAllocate_QInputMethodEvent( ( void * ) pObj, true ) );
}

/* const QString & commitString () const */
HB_FUNC( QT_QINPUTMETHODEVENT_COMMITSTRING )
{
   QInputMethodEvent * p = hbqt_par_QInputMethodEvent( 1 );
   if( p )
      hb_retstr_utf8( ( p )->commitString().toUtf8().data() );
}

/* const QString & preeditString () const */
HB_FUNC( QT_QINPUTMETHODEVENT_PREEDITSTRING )
{
   QInputMethodEvent * p = hbqt_par_QInputMethodEvent( 1 );
   if( p )
      hb_retstr_utf8( ( p )->preeditString().toUtf8().data() );
}

/* int replacementLength () const */
HB_FUNC( QT_QINPUTMETHODEVENT_REPLACEMENTLENGTH )
{
   QInputMethodEvent * p = hbqt_par_QInputMethodEvent( 1 );
   if( p )
      hb_retni( ( p )->replacementLength() );
}

/* int replacementStart () const */
HB_FUNC( QT_QINPUTMETHODEVENT_REPLACEMENTSTART )
{
   QInputMethodEvent * p = hbqt_par_QInputMethodEvent( 1 );
   if( p )
      hb_retni( ( p )->replacementStart() );
}

/* void setCommitString ( const QString & commitString, int replaceFrom = 0, int replaceLength = 0 ) */
HB_FUNC( QT_QINPUTMETHODEVENT_SETCOMMITSTRING )
{
   QInputMethodEvent * p = hbqt_par_QInputMethodEvent( 1 );
   if( p )
   {
      void * pText;
      ( p )->setCommitString( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ), hb_parni( 4 ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
