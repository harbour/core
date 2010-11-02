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
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextImageFormat>


/*
 * QTextImageFormat ()
 *
 */

typedef struct
{
   QTextImageFormat * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextImageFormat;

HBQT_GC_FUNC( hbqt_gcRelease_QTextImageFormat )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QTextImageFormat * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextImageFormat( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextImageFormat * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextImageFormat;
   p->type = HBQT_TYPE_QTextImageFormat;

   return p;
}

HB_FUNC( QT_QTEXTIMAGEFORMAT )
{
   QTextImageFormat * pObj = NULL;

   pObj = new QTextImageFormat() ;

   hb_retptrGC( hbqt_gcAllocate_QTextImageFormat( ( void * ) pObj, true ) );
}

/* qreal height () const */
HB_FUNC( QT_QTEXTIMAGEFORMAT_HEIGHT )
{
   QTextImageFormat * p = hbqt_par_QTextImageFormat( 1 );
   if( p )
      hb_retnd( ( p )->height() );
}

/* bool isValid () const */
HB_FUNC( QT_QTEXTIMAGEFORMAT_ISVALID )
{
   QTextImageFormat * p = hbqt_par_QTextImageFormat( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* QString name () const */
HB_FUNC( QT_QTEXTIMAGEFORMAT_NAME )
{
   QTextImageFormat * p = hbqt_par_QTextImageFormat( 1 );
   if( p )
      hb_retstr_utf8( ( p )->name().toUtf8().data() );
}

/* void setHeight ( qreal height ) */
HB_FUNC( QT_QTEXTIMAGEFORMAT_SETHEIGHT )
{
   QTextImageFormat * p = hbqt_par_QTextImageFormat( 1 );
   if( p )
      ( p )->setHeight( hb_parnd( 2 ) );
}

/* void setName ( const QString & name ) */
HB_FUNC( QT_QTEXTIMAGEFORMAT_SETNAME )
{
   QTextImageFormat * p = hbqt_par_QTextImageFormat( 1 );
   if( p )
   {
      void * pText;
      ( p )->setName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setWidth ( qreal width ) */
HB_FUNC( QT_QTEXTIMAGEFORMAT_SETWIDTH )
{
   QTextImageFormat * p = hbqt_par_QTextImageFormat( 1 );
   if( p )
      ( p )->setWidth( hb_parnd( 2 ) );
}

/* qreal width () const */
HB_FUNC( QT_QTEXTIMAGEFORMAT_WIDTH )
{
   QTextImageFormat * p = hbqt_par_QTextImageFormat( 1 );
   if( p )
      hb_retnd( ( p )->width() );
}


#endif /* #if QT_VERSION >= 0x040500 */
