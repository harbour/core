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
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGroupBox>


/*
 * QGroupBox ( QWidget * parent = 0 )
 * QGroupBox ( const QString & title, QWidget * parent = 0 )
 * ~QGroupBox ()
 */

typedef struct
{
   QPointer< QGroupBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGroupBox;

HBQT_GC_FUNC( hbqt_gcRelease_QGroupBox )
{
   QGroupBox  * ph = NULL;
   HBQT_GC_T_QGroupBox * p = ( HBQT_GC_T_QGroupBox * ) Cargo;

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

void * hbqt_gcAllocate_QGroupBox( void * pObj, bool bNew )
{
   HBQT_GC_T_QGroupBox * p = ( HBQT_GC_T_QGroupBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QGroupBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QGroupBox >( ( QGroupBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGroupBox;
   p->type = HBQT_TYPE_QGroupBox;

   return p;
}

HB_FUNC( QT_QGROUPBOX )
{
   QGroupBox * pObj = NULL;

   pObj = new QGroupBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QGroupBox( ( void * ) pObj, true ) );
}

/* Qt::Alignment alignment () const */
HB_FUNC( QT_QGROUPBOX_ALIGNMENT )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
}

/* bool isCheckable () const */
HB_FUNC( QT_QGROUPBOX_ISCHECKABLE )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
      hb_retl( ( p )->isCheckable() );
}

/* bool isChecked () const */
HB_FUNC( QT_QGROUPBOX_ISCHECKED )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
      hb_retl( ( p )->isChecked() );
}

/* bool isFlat () const */
HB_FUNC( QT_QGROUPBOX_ISFLAT )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
      hb_retl( ( p )->isFlat() );
}

/* void setAlignment ( int alignment ) */
HB_FUNC( QT_QGROUPBOX_SETALIGNMENT )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
      ( p )->setAlignment( hb_parni( 2 ) );
}

/* void setCheckable ( bool checkable ) */
HB_FUNC( QT_QGROUPBOX_SETCHECKABLE )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
      ( p )->setCheckable( hb_parl( 2 ) );
}

/* void setFlat ( bool flat ) */
HB_FUNC( QT_QGROUPBOX_SETFLAT )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
      ( p )->setFlat( hb_parl( 2 ) );
}

/* void setTitle ( const QString & title ) */
HB_FUNC( QT_QGROUPBOX_SETTITLE )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
   {
      void * pText;
      ( p )->setTitle( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QString title () const */
HB_FUNC( QT_QGROUPBOX_TITLE )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->title().toUtf8().data() );
}

/* void setChecked ( bool checked ) */
HB_FUNC( QT_QGROUPBOX_SETCHECKED )
{
   QGroupBox * p = hbqt_par_QGroupBox( 1 );
   if( p )
      ( p )->setChecked( hb_parl( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
