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
 *  enum TextCase { OriginalCase = 0, UpperCase = 1, LowerCase = 2 }
 */

/*
 *  Constructed[ 20/20 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <qscistyle.h>

/*
 * QsciStyle (int style=-1)
 * QsciStyle (int style, const QString &description, const QColor &color, const QColor &paper, const QFont &font, bool eol_fill=false)
 *
 */

typedef struct
{
   QPointer< QsciStyle > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciStyle;

HBQT_GC_FUNC( hbqt_gcRelease_QsciStyle )
{
   HBQT_GC_T_QsciStyle * p = ( HBQT_GC_T_QsciStyle * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QsciStyle * ph = p->ph;
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

void * hbqt_gcAllocate_QsciStyle( void * pObj, bool bNew )
{
   HBQT_GC_T_QsciStyle * p = ( HBQT_GC_T_QsciStyle * ) hb_gcAllocate( sizeof( HBQT_GC_T_QsciStyle ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QsciStyle >( ( QsciStyle * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciStyle;
   p->type = HBQT_TYPE_QsciStyle;

   return p;
}

HB_FUNC( QT_QSCISTYLE )
{
   QsciStyle * pObj = NULL;

   if( HB_ISNUMERIC( 1 ) )
   {
      pObj = new QsciStyle( hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() >= 5 )
   {
      pObj = new QsciStyle( hb_parni( 1 ), hbqt_par_QString( 2 ), *hbqt_par_QColor( 3 ), *hbqt_par_QColor( 4 ), * hbqt_par_QFont( 5 ), HB_ISLOG( 6 ) ? hb_parl( 6 ) : false ) ;
   }
   else
   {
      pObj = new QsciStyle() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QsciStyle( ( void * ) pObj, true ) );
}

/* int style () const */
HB_FUNC( QT_QSCISTYLE_STYLE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      hb_retni( ( p )->style() );
}

/* void setDescription (const QString &description) */
HB_FUNC( QT_QSCISTYLE_SETDESCRIPTION )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
   {
      void * pText;
      ( p )->setDescription( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QString description () const */
HB_FUNC( QT_QSCISTYLE_DESCRIPTION )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      hb_retstr_utf8( ( p )->description().toUtf8().data() );
}

/* void setColor (const QColor &color) */
HB_FUNC( QT_QSCISTYLE_SETCOLOR )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      ( p )->setColor( *hbqt_par_QColor( 2 ) );
}

/* QColor color () const */
HB_FUNC( QT_QSCISTYLE_COLOR )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color() ), true ) );
}

/* void setPaper (const QColor &paper) */
HB_FUNC( QT_QSCISTYLE_SETPAPER )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      ( p )->setPaper( *hbqt_par_QColor( 2 ) );
}

/* QColor paper () const */
HB_FUNC( QT_QSCISTYLE_PAPER )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->paper() ), true ) );
}

/* void setFont (const QFont &font) */
HB_FUNC( QT_QSCISTYLE_SETFONT )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
}

/* QFont font () const */
HB_FUNC( QT_QSCISTYLE_FONT )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
}

/* void setEolFill (bool fill) */
HB_FUNC( QT_QSCISTYLE_SETEOLFILL )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      ( p )->setEolFill( hb_parl( 2 ) );
}

/* bool eolFill () const */
HB_FUNC( QT_QSCISTYLE_EOLFILL )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      hb_retl( ( p )->eolFill() );
}

/* void setTextCase (TextCase text_case) */
HB_FUNC( QT_QSCISTYLE_SETTEXTCASE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      ( p )->setTextCase( ( QsciStyle::TextCase ) hb_parni( 2 ) );
}

/* TextCase textCase () const */
HB_FUNC( QT_QSCISTYLE_TEXTCASE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      hb_retni( ( QsciStyle::TextCase ) ( p )->textCase() );
}

/* void setVisible (bool visible) */
HB_FUNC( QT_QSCISTYLE_SETVISIBLE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
}

/* bool visible () const */
HB_FUNC( QT_QSCISTYLE_VISIBLE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      hb_retl( ( p )->visible() );
}

/* void setChangeable (bool changeable) */
HB_FUNC( QT_QSCISTYLE_SETCHANGEABLE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      ( p )->setChangeable( hb_parl( 2 ) );
}

/* bool changeable () const */
HB_FUNC( QT_QSCISTYLE_CHANGEABLE )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      hb_retl( ( p )->changeable() );
}

/* void setHotspot (bool hotspot) */
HB_FUNC( QT_QSCISTYLE_SETHOTSPOT )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      ( p )->setHotspot( hb_parl( 2 ) );
}

/* bool hotspot () const */
HB_FUNC( QT_QSCISTYLE_HOTSPOT )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      hb_retl( ( p )->hotspot() );
}

/* void refresh () */
HB_FUNC( QT_QSCISTYLE_REFRESH )
{
   QsciStyle * p = hbqt_par_QsciStyle( 1 );
   if( p )
      ( p )->refresh();
}


#endif /* #if QT_VERSION >= 0x040500 */
