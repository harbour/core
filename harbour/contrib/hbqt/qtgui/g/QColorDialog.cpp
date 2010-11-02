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
 *  enum ColorDialogOption { ShowAlphaChannel, NoButtons, DontUseNativeDialog }
 *  flags ColorDialogOptions
 */

/*
 *  Constructed[ 15/15 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //void open ( QObject * receiver, const char * member )
 */

#include <QtCore/QPointer>

#include <QtGui/QColorDialog>

/*
 * QColorDialog ( QWidget * parent = 0 )
 * QColorDialog ( const QColor & initial, QWidget * parent = 0 )
 * ~QColorDialog ()
 */

typedef struct
{
   QPointer< QColorDialog > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QColorDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QColorDialog )
{
   HBQT_GC_T_QColorDialog * p = ( HBQT_GC_T_QColorDialog * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QColorDialog * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QColorDialog( void * pObj, bool bNew )
{
   HBQT_GC_T_QColorDialog * p = ( HBQT_GC_T_QColorDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QColorDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QColorDialog >( ( QColorDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QColorDialog;
   p->type = HBQT_TYPE_QColorDialog;

   return p;
}

HB_FUNC( QT_QCOLORDIALOG )
{
   QColorDialog * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISNUM( 1 ) )
   {
      pObj = new QColorDialog( hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;
   }
   else
   {
      pObj = new QColorDialog( hbqt_par_QWidget( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QColorDialog( ( void * ) pObj, true ) );
}

/* QColor currentColor () const */
HB_FUNC( QT_QCOLORDIALOG_CURRENTCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->currentColor() ), true ) );
}

/* void open () */
HB_FUNC( QT_QCOLORDIALOG_OPEN )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->open();
}

/* ColorDialogOptions options () const */
HB_FUNC( QT_QCOLORDIALOG_OPTIONS )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retni( ( QColorDialog::ColorDialogOptions ) ( p )->options() );
}

/* QColor selectedColor () const */
HB_FUNC( QT_QCOLORDIALOG_SELECTEDCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->selectedColor() ), true ) );
}

/* void setCurrentColor ( const QColor & color ) */
HB_FUNC( QT_QCOLORDIALOG_SETCURRENTCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setCurrentColor( *hbqt_par_QColor( 2 ) );
}

/* void setOption ( ColorDialogOption option, bool on = true ) */
HB_FUNC( QT_QCOLORDIALOG_SETOPTION )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setOption( ( QColorDialog::ColorDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setOptions ( ColorDialogOptions options ) */
HB_FUNC( QT_QCOLORDIALOG_SETOPTIONS )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setOptions( ( QColorDialog::ColorDialogOptions ) hb_parni( 2 ) );
}

/* virtual void setVisible ( bool visible ) */
HB_FUNC( QT_QCOLORDIALOG_SETVISIBLE )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
}

/* bool testOption ( ColorDialogOption option ) const */
HB_FUNC( QT_QCOLORDIALOG_TESTOPTION )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QColorDialog::ColorDialogOption ) hb_parni( 2 ) ) );
}

/* QRgb customColor ( int index ) */
HB_FUNC( QT_QCOLORDIALOG_CUSTOMCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retnl( ( p )->customColor( hb_parni( 2 ) ) );
}

/* int customCount () */
HB_FUNC( QT_QCOLORDIALOG_CUSTOMCOUNT )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retni( ( p )->customCount() );
}

/* QColor getColor ( const QColor & initial, QWidget * parent, const QString & title, ColorDialogOptions options = 0 ) */
HB_FUNC( QT_QCOLORDIALOG_GETCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->getColor( *hbqt_par_QColor( 2 ), hbqt_par_QWidget( 3 ), hb_parstr_utf8( 4, &pText, NULL ), ( QColorDialog::ColorDialogOptions ) hb_parni( 5 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QColor getColor ( const QColor & initial = Qt::white, QWidget * parent = 0 ) */
HB_FUNC( QT_QCOLORDIALOG_GETCOLOR_1 )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->getColor( *hbqt_par_QColor( 2 ), hbqt_par_QWidget( 3 ) ) ), true ) );
}

/* void setCustomColor ( int index, QRgb color ) */
HB_FUNC( QT_QCOLORDIALOG_SETCUSTOMCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setCustomColor( hb_parni( 2 ), hb_parnl( 3 ) );
}

/* void setStandardColor ( int index, QRgb color ) */
HB_FUNC( QT_QCOLORDIALOG_SETSTANDARDCOLOR )
{
   QColorDialog * p = hbqt_par_QColorDialog( 1 );
   if( p )
      ( p )->setStandardColor( hb_parni( 2 ), hb_parnl( 3 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
