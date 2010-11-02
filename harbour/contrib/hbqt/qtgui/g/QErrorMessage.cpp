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
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QErrorMessage>


/*
 * QErrorMessage ( QWidget * parent = 0 )
 * ~QErrorMessage ()
 */

typedef struct
{
   QPointer< QErrorMessage > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QErrorMessage;

HBQT_GC_FUNC( hbqt_gcRelease_QErrorMessage )
{
   HBQT_GC_T_QErrorMessage * p = ( HBQT_GC_T_QErrorMessage * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QErrorMessage * ph = p->ph;
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

void * hbqt_gcAllocate_QErrorMessage( void * pObj, bool bNew )
{
   HBQT_GC_T_QErrorMessage * p = ( HBQT_GC_T_QErrorMessage * ) hb_gcAllocate( sizeof( HBQT_GC_T_QErrorMessage ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QErrorMessage >( ( QErrorMessage * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QErrorMessage;
   p->type = HBQT_TYPE_QErrorMessage;

   return p;
}

HB_FUNC( QT_QERRORMESSAGE )
{
   QErrorMessage * pObj = NULL;

   pObj = new QErrorMessage( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QErrorMessage( ( void * ) pObj, true ) );
}

/* void showMessage ( const QString & message ) */
HB_FUNC( QT_QERRORMESSAGE_SHOWMESSAGE )
{
   QErrorMessage * p = hbqt_par_QErrorMessage( 1 );
   if( p )
   {
      void * pText;
      ( p )->showMessage( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void showMessage ( const QString & message, const QString & type ) */
HB_FUNC( QT_QERRORMESSAGE_SHOWMESSAGE_1 )
{
   QErrorMessage * p = hbqt_par_QErrorMessage( 1 );
   if( p )
   {
      void * pText;
      ( p )->showMessage( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
