/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QGraphicsTextItem>
#include <QtGui/QFont>
#include <QtGui/QTextCursor>
#include <QtGui/QTextDocument>


/*
 * QGraphicsTextItem ( QGraphicsItem * parent = 0 )
 * QGraphicsTextItem ( const QString & text, QGraphicsItem * parent = 0 )
 * ~QGraphicsTextItem ()
 */

typedef struct
{
   QPointer< QGraphicsTextItem > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsTextItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsTextItem )
{
   QGraphicsTextItem  * ph = NULL ;
   HBQT_GC_T_QGraphicsTextItem * p = ( HBQT_GC_T_QGraphicsTextItem * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QGraphicsTextItem   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QGraphicsTextItem   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QGraphicsTextItem          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QGraphicsTextItem    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QGraphicsTextItem    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsTextItem( void * pObj, bool bNew )
{
   HBQT_GC_T_QGraphicsTextItem * p = ( HBQT_GC_T_QGraphicsTextItem * ) hb_gcAllocate( sizeof( HBQT_GC_T_QGraphicsTextItem ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QGraphicsTextItem >( ( QGraphicsTextItem * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsTextItem;
   p->type = HBQT_TYPE_QGraphicsTextItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGraphicsTextItem  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGraphicsTextItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRAPHICSTEXTITEM )
{
   QGraphicsTextItem * pObj = NULL;

   if( hb_pcount() >= 1 )
   {
      if( HB_ISCHAR( 1 ) )
      {
         pObj = new QGraphicsTextItem( hbqt_par_QString( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsItem( 1 ) : 0 ) ) ;
      }
      else if( HB_ISPOINTER( 1 ) )
      {
         pObj = new QGraphicsTextItem( hbqt_par_QGraphicsItem( 1 ) ) ;
      }
      else
      {
         pObj = new QGraphicsTextItem() ;
      }
   }
   else
   {
      pObj = new QGraphicsTextItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsTextItem( ( void * ) pObj, true ) );
}

/*
 * void adjustSize ()
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_ADJUSTSIZE )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      ( p )->adjustSize();
   }
}

/*
 * QColor defaultTextColor () const
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_DEFAULTTEXTCOLOR )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultTextColor() ), true ) );
   }
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_DOCUMENT )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) );
   }
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_FONT )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   }
}

/*
 * bool openExternalLinks () const
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_OPENEXTERNALLINKS )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      hb_retl( ( p )->openExternalLinks() );
   }
}

/*
 * void setDefaultTextColor ( const QColor & col )
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_SETDEFAULTTEXTCOLOR )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      ( p )->setDefaultTextColor( *hbqt_par_QColor( 2 ) );
   }
}

/*
 * void setDocument ( QTextDocument * document )
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_SETDOCUMENT )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      ( p )->setDocument( hbqt_par_QTextDocument( 2 ) );
   }
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_SETFONT )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   }
}

/*
 * void setHtml ( const QString & text )
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_SETHTML )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setHtml( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setOpenExternalLinks ( bool open )
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_SETOPENEXTERNALLINKS )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      ( p )->setOpenExternalLinks( hb_parl( 2 ) );
   }
}

/*
 * void setPlainText ( const QString & text )
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_SETPLAINTEXT )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPlainText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setTabChangesFocus ( bool b )
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_SETTABCHANGESFOCUS )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      ( p )->setTabChangesFocus( hb_parl( 2 ) );
   }
}

/*
 * void setTextCursor ( const QTextCursor & cursor )
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_SETTEXTCURSOR )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      ( p )->setTextCursor( *hbqt_par_QTextCursor( 2 ) );
   }
}

/*
 * void setTextInteractionFlags ( Qt::TextInteractionFlags flags )
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_SETTEXTINTERACTIONFLAGS )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      ( p )->setTextInteractionFlags( ( Qt::TextInteractionFlags ) hb_parni( 2 ) );
   }
}

/*
 * void setTextWidth ( qreal width )
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_SETTEXTWIDTH )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      ( p )->setTextWidth( hb_parnd( 2 ) );
   }
}

/*
 * bool tabChangesFocus () const
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_TABCHANGESFOCUS )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      hb_retl( ( p )->tabChangesFocus() );
   }
}

/*
 * QTextCursor textCursor () const
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_TEXTCURSOR )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->textCursor() ), true ) );
   }
}

/*
 * Qt::TextInteractionFlags textInteractionFlags () const
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_TEXTINTERACTIONFLAGS )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      hb_retni( ( Qt::TextInteractionFlags ) ( p )->textInteractionFlags() );
   }
}

/*
 * qreal textWidth () const
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_TEXTWIDTH )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      hb_retnd( ( p )->textWidth() );
   }
}

/*
 * QString toHtml () const
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_TOHTML )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toHtml().toUtf8().data() );
   }
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QGRAPHICSTEXTITEM_TOPLAINTEXT )
{
   QGraphicsTextItem * p = hbqt_par_QGraphicsTextItem( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toPlainText().toUtf8().data() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
