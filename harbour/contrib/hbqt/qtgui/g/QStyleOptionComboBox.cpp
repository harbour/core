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
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

/*
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionComboBox>


/*
 * QStyleOptionComboBox ()
 * QStyleOptionComboBox ( const QStyleOptionComboBox & other )
 */

typedef struct
{
   QStyleOptionComboBox * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionComboBox;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionComboBox )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionComboBox * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionComboBox( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionComboBox * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionComboBox;
   p->type = HBQT_TYPE_QStyleOptionComboBox;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONCOMBOBOX )
{
   QStyleOptionComboBox * pObj = NULL;

   pObj = new QStyleOptionComboBox() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionComboBox( ( void * ) pObj, true ) );
}

/* QIcon currentIcon */
HB_FUNC( QT_QSTYLEOPTIONCOMBOBOX_CURRENTICON )
{
   QStyleOptionComboBox * p = hbqt_par_QStyleOptionComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->currentIcon ), true ) );
}

/* QString currentText */
HB_FUNC( QT_QSTYLEOPTIONCOMBOBOX_CURRENTTEXT )
{
   QStyleOptionComboBox * p = hbqt_par_QStyleOptionComboBox( 1 );
   if( p )
      hb_retstr_utf8( ( p )->currentText.toUtf8().data() );
}

/* bool editable */
HB_FUNC( QT_QSTYLEOPTIONCOMBOBOX_EDITABLE )
{
   QStyleOptionComboBox * p = hbqt_par_QStyleOptionComboBox( 1 );
   if( p )
      hb_retl( ( p )->editable );
}

/* bool frame */
HB_FUNC( QT_QSTYLEOPTIONCOMBOBOX_FRAME )
{
   QStyleOptionComboBox * p = hbqt_par_QStyleOptionComboBox( 1 );
   if( p )
      hb_retl( ( p )->frame );
}

/* QSize iconSize */
HB_FUNC( QT_QSTYLEOPTIONCOMBOBOX_ICONSIZE )
{
   QStyleOptionComboBox * p = hbqt_par_QStyleOptionComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize ), true ) );
}

/* QRect popupRect */
HB_FUNC( QT_QSTYLEOPTIONCOMBOBOX_POPUPRECT )
{
   QStyleOptionComboBox * p = hbqt_par_QStyleOptionComboBox( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->popupRect ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
