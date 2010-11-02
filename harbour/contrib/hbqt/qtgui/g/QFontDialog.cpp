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
 *  enum FontDialogOption { NoButtons, DontUseNativeDialog }
 *  flags FontDialogOptions
 */

/*
 *  Constructed[ 12/12 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // void open ( QObject * receiver, const char * member )
 *  // QFont getFont ( bool * ok, QWidget * parent, const char * name )
 */

#include <QtCore/QPointer>

#include <QtGui/QFontDialog>


/*
 * QFontDialog ( QWidget * parent = 0 )
 * QFontDialog ( const QFont & initial, QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QFontDialog > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFontDialog;

HBQT_GC_FUNC( hbqt_gcRelease_QFontDialog )
{
   HBQT_GC_T_QFontDialog * p = ( HBQT_GC_T_QFontDialog * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QFontDialog * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFontDialog( void * pObj, bool bNew )
{
   HBQT_GC_T_QFontDialog * p = ( HBQT_GC_T_QFontDialog * ) hb_gcAllocate( sizeof( HBQT_GC_T_QFontDialog ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFontDialog >( ( QFontDialog * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontDialog;
   p->type = HBQT_TYPE_QFontDialog;

   return p;
}

HB_FUNC( QT_QFONTDIALOG )
{
   QFontDialog * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QFontDialog( hbqt_par_QWidget( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QFontDialog( *hbqt_par_QFont( 1 ), hbqt_par_QWidget( 2 ) ) ;
   }
   else
   {
      pObj = new QFontDialog( 0 ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFontDialog( ( void * ) pObj, true ) );
}

/* QFont currentFont () const */
HB_FUNC( QT_QFONTDIALOG_CURRENTFONT )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->currentFont() ), true ) );
}

/* FontDialogOptions options () const */
HB_FUNC( QT_QFONTDIALOG_OPTIONS )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      hb_retni( ( QFontDialog::FontDialogOptions ) ( p )->options() );
}

/* QFont selectedFont () const */
HB_FUNC( QT_QFONTDIALOG_SELECTEDFONT )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->selectedFont() ), true ) );
}

/* void setCurrentFont ( const QFont & font ) */
HB_FUNC( QT_QFONTDIALOG_SETCURRENTFONT )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      ( p )->setCurrentFont( *hbqt_par_QFont( 2 ) );
}

/* void setOption ( FontDialogOption option, bool on = true ) */
HB_FUNC( QT_QFONTDIALOG_SETOPTION )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      ( p )->setOption( ( QFontDialog::FontDialogOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setOptions ( FontDialogOptions options ) */
HB_FUNC( QT_QFONTDIALOG_SETOPTIONS )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      ( p )->setOptions( ( QFontDialog::FontDialogOptions ) hb_parni( 2 ) );
}

/* bool testOption ( FontDialogOption option ) const */
HB_FUNC( QT_QFONTDIALOG_TESTOPTION )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QFontDialog::FontDialogOption ) hb_parni( 2 ) ) );
}

/* QFont getFont ( bool * ok, const QFont & initial, QWidget * parent, const QString & title, FontDialogOptions options ) */
HB_FUNC( QT_QFONTDIALOG_GETFONT )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   bool iOk = 0;

   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), hb_parstr_utf8( 5, &pText, NULL ), ( QFontDialog::FontDialogOptions ) hb_parni( 6 ) ) ), true ) );
      hb_strfree( pText );
   }

   hb_stornl( iOk, 2 );
}

/* QFont getFont ( bool * ok, const QFont & initial, QWidget * parent, const char * name ) */
HB_FUNC( QT_QFONTDIALOG_GETFONT_1 )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), ( const char * ) hb_parc( 5 ) ) ), true ) );

   hb_stornl( iOk, 2 );
}

/* QFont getFont ( bool * ok, const QFont & initial, QWidget * parent, const QString & title ) */
HB_FUNC( QT_QFONTDIALOG_GETFONT_2 )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   bool iOk = 0;

   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ), hb_parstr_utf8( 5, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }

   hb_stornl( iOk, 2 );
}

/* QFont getFont ( bool * ok, const QFont & initial, QWidget * parent = 0 ) */
HB_FUNC( QT_QFONTDIALOG_GETFONT_3 )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, *hbqt_par_QFont( 3 ), hbqt_par_QWidget( 4 ) ) ), true ) );

   hb_stornl( iOk, 2 );
}

/* QFont getFont ( bool * ok, QWidget * parent = 0 ) */
HB_FUNC( QT_QFONTDIALOG_GETFONT_4 )
{
   QFontDialog * p = hbqt_par_QFontDialog( 1 );
   bool iOk = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->getFont( &iOk, hbqt_par_QWidget( 3 ) ) ), true ) );

   hb_stornl( iOk, 2 );
}


#endif /* #if QT_VERSION >= 0x040500 */
