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

#include <QtGui/QCommandLinkButton>


/*
 * QCommandLinkButton ( QWidget * parent = 0 )
 * QCommandLinkButton ( const QString & text, QWidget * parent = 0 )
 * QCommandLinkButton ( const QString & text, const QString & description, QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QCommandLinkButton > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QCommandLinkButton;

HBQT_GC_FUNC( hbqt_gcRelease_QCommandLinkButton )
{
   QCommandLinkButton  * ph = NULL;
   HBQT_GC_T_QCommandLinkButton * p = ( HBQT_GC_T_QCommandLinkButton * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
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

void * hbqt_gcAllocate_QCommandLinkButton( void * pObj, bool bNew )
{
   HBQT_GC_T_QCommandLinkButton * p = ( HBQT_GC_T_QCommandLinkButton * ) hb_gcAllocate( sizeof( HBQT_GC_T_QCommandLinkButton ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QCommandLinkButton >( ( QCommandLinkButton * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QCommandLinkButton;
   p->type = HBQT_TYPE_QCommandLinkButton;

   return p;
}

HB_FUNC( QT_QCOMMANDLINKBUTTON )
{
   QCommandLinkButton * pObj = NULL;

   pObj = new QCommandLinkButton( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QCommandLinkButton( ( void * ) pObj, true ) );
}

/* QString description () const */
HB_FUNC( QT_QCOMMANDLINKBUTTON_DESCRIPTION )
{
   QCommandLinkButton * p = hbqt_par_QCommandLinkButton( 1 );
   if( p )
      hb_retstr_utf8( ( p )->description().toUtf8().data() );
}

/* void setDescription ( const QString & description ) */
HB_FUNC( QT_QCOMMANDLINKBUTTON_SETDESCRIPTION )
{
   QCommandLinkButton * p = hbqt_par_QCommandLinkButton( 1 );
   if( p )
   {
      void * pText;
      ( p )->setDescription( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
