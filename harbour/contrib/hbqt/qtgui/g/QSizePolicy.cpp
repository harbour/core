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
 *  enum Policy { Fixed, Minimum, Maximum, Preferred, ..., Ignored }
 *  enum PolicyFlag { GrowFlag, ExpandFlag, ShrinkFlag, IgnoreFlag }
 *  enum ControlType { DefaultType, ButtonBox, CheckBox, ComboBox, ..., ToolButton }
 *  flags ControlTypes
 */

/*
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSizePolicy>


/*
 * QSizePolicy ()
 * QSizePolicy ( Policy horizontal, Policy vertical )
 * QSizePolicy ( Policy horizontal, Policy vertical, ControlType type )
 */

typedef struct
{
   QSizePolicy * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSizePolicy;

HBQT_GC_FUNC( hbqt_gcRelease_QSizePolicy )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QSizePolicy * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSizePolicy( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QSizePolicy * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSizePolicy;
   p->type = HBQT_TYPE_QSizePolicy;

   return p;
}

HB_FUNC( QT_QSIZEPOLICY )
{
   QSizePolicy * pObj = NULL;

   if( hb_pcount() >= 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QSizePolicy( ( QSizePolicy::Policy ) hb_parni( 1 ), ( QSizePolicy::Policy ) hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QSizePolicy( *hbqt_par_QSizePolicy( 1 ) ) ;
   }
   else
   {
      pObj = new QSizePolicy() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QSizePolicy( ( void * ) pObj, true ) );
}

/* ControlType controlType () const */
HB_FUNC( QT_QSIZEPOLICY_CONTROLTYPE )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      hb_retni( ( QSizePolicy::ControlType ) ( p )->controlType() );
}

/* Qt::Orientations expandingDirections () const */
HB_FUNC( QT_QSIZEPOLICY_EXPANDINGDIRECTIONS )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      hb_retni( ( Qt::Orientations ) ( p )->expandingDirections() );
}

/* bool hasHeightForWidth () const */
HB_FUNC( QT_QSIZEPOLICY_HASHEIGHTFORWIDTH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      hb_retl( ( p )->hasHeightForWidth() );
}

/* Policy horizontalPolicy () const */
HB_FUNC( QT_QSIZEPOLICY_HORIZONTALPOLICY )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      hb_retni( ( QSizePolicy::Policy ) ( p )->horizontalPolicy() );
}

/* int horizontalStretch () const */
HB_FUNC( QT_QSIZEPOLICY_HORIZONTALSTRETCH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      hb_retni( ( p )->horizontalStretch() );
}

/* void setControlType ( ControlType type ) */
HB_FUNC( QT_QSIZEPOLICY_SETCONTROLTYPE )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      ( p )->setControlType( ( QSizePolicy::ControlType ) hb_parni( 2 ) );
}

/* void setHeightForWidth ( bool dependent ) */
HB_FUNC( QT_QSIZEPOLICY_SETHEIGHTFORWIDTH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      ( p )->setHeightForWidth( hb_parl( 2 ) );
}

/* void setHorizontalPolicy ( Policy policy ) */
HB_FUNC( QT_QSIZEPOLICY_SETHORIZONTALPOLICY )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      ( p )->setHorizontalPolicy( ( QSizePolicy::Policy ) hb_parni( 2 ) );
}

/* void setHorizontalStretch ( uchar stretchFactor ) */
HB_FUNC( QT_QSIZEPOLICY_SETHORIZONTALSTRETCH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      ( p )->setHorizontalStretch( ( uchar ) hb_parni( 2 ) );
}

/* void setVerticalPolicy ( Policy policy ) */
HB_FUNC( QT_QSIZEPOLICY_SETVERTICALPOLICY )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      ( p )->setVerticalPolicy( ( QSizePolicy::Policy ) hb_parni( 2 ) );
}

/* void setVerticalStretch ( uchar stretchFactor ) */
HB_FUNC( QT_QSIZEPOLICY_SETVERTICALSTRETCH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      ( p )->setVerticalStretch( ( uchar ) hb_parni( 2 ) );
}

/* void transpose () */
HB_FUNC( QT_QSIZEPOLICY_TRANSPOSE )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      ( p )->transpose();
}

/* Policy verticalPolicy () const */
HB_FUNC( QT_QSIZEPOLICY_VERTICALPOLICY )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      hb_retni( ( QSizePolicy::Policy ) ( p )->verticalPolicy() );
}

/* int verticalStretch () const */
HB_FUNC( QT_QSIZEPOLICY_VERTICALSTRETCH )
{
   QSizePolicy * p = hbqt_par_QSizePolicy( 1 );
   if( p )
      hb_retni( ( p )->verticalStretch() );
}


#endif /* #if QT_VERSION >= 0x040500 */
