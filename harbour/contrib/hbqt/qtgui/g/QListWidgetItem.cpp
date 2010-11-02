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
 *  enum ItemType { Type, UserType }
 */

/*
 *  Constructed[ 35/35 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QListWidgetItem>


/*
 * QListWidgetItem ( QListWidget * parent = 0, int type = Type )
 * QListWidgetItem ( const QString & text, QListWidget * parent = 0, int type = Type )
 * QListWidgetItem ( const QIcon & icon, const QString & text, QListWidget * parent = 0, int type = Type )
 * QListWidgetItem ( const QListWidgetItem & other )
 * virtual ~QListWidgetItem ()
 */

typedef struct
{
   QListWidgetItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QListWidgetItem;

HBQT_GC_FUNC( hbqt_gcRelease_QListWidgetItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QListWidgetItem * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QListWidgetItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QListWidgetItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QListWidgetItem;
   p->type = HBQT_TYPE_QListWidgetItem;

   return p;
}

HB_FUNC( QT_QLISTWIDGETITEM )
{
   QListWidgetItem * pObj = NULL;

   pObj = new QListWidgetItem( hbqt_par_QListWidget( 1 ), hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QListWidgetItem( ( void * ) pObj, true ) );
}

/* QBrush background () const */
HB_FUNC( QT_QLISTWIDGETITEM_BACKGROUND )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
}

/* Qt::CheckState checkState () const */
HB_FUNC( QT_QLISTWIDGETITEM_CHECKSTATE )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retni( ( Qt::CheckState ) ( p )->checkState() );
}

/* virtual QListWidgetItem * clone () const */
HB_FUNC( QT_QLISTWIDGETITEM_CLONE )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QListWidgetItem( ( p )->clone(), false ) );
}

/* virtual QVariant data ( int role ) const */
HB_FUNC( QT_QLISTWIDGETITEM_DATA )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parni( 2 ) ) ), true ) );
}

/* Qt::ItemFlags flags () const */
HB_FUNC( QT_QLISTWIDGETITEM_FLAGS )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retni( ( Qt::ItemFlags ) ( p )->flags() );
}

/* QFont font () const */
HB_FUNC( QT_QLISTWIDGETITEM_FONT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
}

/* QBrush foreground () const */
HB_FUNC( QT_QLISTWIDGETITEM_FOREGROUND )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground() ), true ) );
}

/* QIcon icon () const */
HB_FUNC( QT_QLISTWIDGETITEM_ICON )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
}

/* bool isHidden () const */
HB_FUNC( QT_QLISTWIDGETITEM_ISHIDDEN )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retl( ( p )->isHidden() );
}

/* bool isSelected () const */
HB_FUNC( QT_QLISTWIDGETITEM_ISSELECTED )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retl( ( p )->isSelected() );
}

/* QListWidget * listWidget () const */
HB_FUNC( QT_QLISTWIDGETITEM_LISTWIDGET )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QListWidget( ( p )->listWidget(), false ) );
}

/* virtual void read ( QDataStream & in ) */
HB_FUNC( QT_QLISTWIDGETITEM_READ )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->read( *hbqt_par_QDataStream( 2 ) );
}

/* void setBackground ( const QBrush & brush ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETBACKGROUND )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
}

/* void setCheckState ( Qt::CheckState state ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETCHECKSTATE )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) );
}

/* virtual void setData ( int role, const QVariant & value ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETDATA )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/* void setFlags ( Qt::ItemFlags flags ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETFLAGS )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) );
}

/* void setFont ( const QFont & font ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETFONT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
}

/* void setForeground ( const QBrush & brush ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETFOREGROUND )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setForeground( *hbqt_par_QBrush( 2 ) );
}

/* void setHidden ( bool hide ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETHIDDEN )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setHidden( hb_parl( 2 ) );
}

/* void setIcon ( const QIcon & icon ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETICON )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setIcon( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )) );
}

/* void setSelected ( bool select ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETSELECTED )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setSelected( hb_parl( 2 ) );
}

/* void setSizeHint ( const QSize & size ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETSIZEHINT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setSizeHint( *hbqt_par_QSize( 2 ) );
}

/* void setStatusTip ( const QString & statusTip ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETSTATUSTIP )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setStatusTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setText ( const QString & text ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETTEXT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setTextAlignment ( int alignment ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETTEXTALIGNMENT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->setTextAlignment( hb_parni( 2 ) );
}

/* void setToolTip ( const QString & toolTip ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETTOOLTIP )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setToolTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setWhatsThis ( const QString & whatsThis ) */
HB_FUNC( QT_QLISTWIDGETITEM_SETWHATSTHIS )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setWhatsThis( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QSize sizeHint () const */
HB_FUNC( QT_QLISTWIDGETITEM_SIZEHINT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
}

/* QString statusTip () const */
HB_FUNC( QT_QLISTWIDGETITEM_STATUSTIP )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->statusTip().toUtf8().data() );
}

/* QString text () const */
HB_FUNC( QT_QLISTWIDGETITEM_TEXT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* int textAlignment () const */
HB_FUNC( QT_QLISTWIDGETITEM_TEXTALIGNMENT )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->textAlignment() );
}

/* QString toolTip () const */
HB_FUNC( QT_QLISTWIDGETITEM_TOOLTIP )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toolTip().toUtf8().data() );
}

/* int type () const */
HB_FUNC( QT_QLISTWIDGETITEM_TYPE )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->type() );
}

/* QString whatsThis () const */
HB_FUNC( QT_QLISTWIDGETITEM_WHATSTHIS )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->whatsThis().toUtf8().data() );
}

/* virtual void write ( QDataStream & out ) const */
HB_FUNC( QT_QLISTWIDGETITEM_WRITE )
{
   QListWidgetItem * p = hbqt_par_QListWidgetItem( 1 );
   if( p )
      ( p )->write( *hbqt_par_QDataStream( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
