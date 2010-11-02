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
 *  enum FontFilter { AllFonts, ScalableFonts, NonScalableFonts, MonospacedFonts, ProportionalFonts }
 *  flags FontFilters
 */

/*
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QFontComboBox>


/*
 * QFontComboBox ( QWidget * parent = 0 )
 * ~QFontComboBox ()
 */

typedef struct
{
   QPointer< QFontComboBox > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFontComboBox;

HBQT_GC_FUNC( hbqt_gcRelease_QFontComboBox )
{
   HBQT_GC_T_QFontComboBox * p = ( HBQT_GC_T_QFontComboBox * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QFontComboBox * ph = p->ph;
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

void * hbqt_gcAllocate_QFontComboBox( void * pObj, bool bNew )
{
   HBQT_GC_T_QFontComboBox * p = ( HBQT_GC_T_QFontComboBox * ) hb_gcAllocate( sizeof( HBQT_GC_T_QFontComboBox ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFontComboBox >( ( QFontComboBox * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontComboBox;
   p->type = HBQT_TYPE_QFontComboBox;

   return p;
}

HB_FUNC( QT_QFONTCOMBOBOX )
{
   QFontComboBox * pObj = NULL;

   pObj = new QFontComboBox( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QFontComboBox( ( void * ) pObj, true ) );
}

/* QFont currentFont () const */
HB_FUNC( QT_QFONTCOMBOBOX_CURRENTFONT )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->currentFont() ), true ) );
}

/* FontFilters fontFilters () const */
HB_FUNC( QT_QFONTCOMBOBOX_FONTFILTERS )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      hb_retni( ( QFontComboBox::FontFilters ) ( p )->fontFilters() );
}

/* void setFontFilters ( FontFilters filters ) */
HB_FUNC( QT_QFONTCOMBOBOX_SETFONTFILTERS )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      ( p )->setFontFilters( ( QFontComboBox::FontFilters ) hb_parni( 2 ) );
}

/* void setWritingSystem ( QFontDatabase::WritingSystem script ) */
HB_FUNC( QT_QFONTCOMBOBOX_SETWRITINGSYSTEM )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      ( p )->setWritingSystem( ( QFontDatabase::WritingSystem ) hb_parni( 2 ) );
}

/* QFontDatabase::WritingSystem writingSystem () const */
HB_FUNC( QT_QFONTCOMBOBOX_WRITINGSYSTEM )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      hb_retni( ( QFontDatabase::WritingSystem ) ( p )->writingSystem() );
}

/* void setCurrentFont ( const QFont & font ) */
HB_FUNC( QT_QFONTCOMBOBOX_SETCURRENTFONT )
{
   QFontComboBox * p = hbqt_par_QFontComboBox( 1 );
   if( p )
      ( p )->setCurrentFont( *hbqt_par_QFont( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
