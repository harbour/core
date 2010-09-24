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

/*
 *  Constructed[ 29/29 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QLabel>
#include <QtGui/QPicture>


/*
 * QLabel ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * QLabel ( const QString & text, QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QLabel ()
 */

typedef struct
{
   QPointer< QLabel > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLabel;

HBQT_GC_FUNC( hbqt_gcRelease_QLabel )
{
   QLabel  * ph = NULL ;
   HBQT_GC_T_QLabel * p = ( HBQT_GC_T_QLabel * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QLabel   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QLabel   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QLabel          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QLabel    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QLabel    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLabel( void * pObj, bool bNew )
{
   HBQT_GC_T_QLabel * p = ( HBQT_GC_T_QLabel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QLabel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QLabel >( ( QLabel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLabel;
   p->type = HBQT_TYPE_QLabel;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QLabel  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QLabel", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QLABEL )
{
   QLabel * pObj = NULL;

   pObj = new QLabel( hbqt_par_QWidget( 1 ), HB_ISNUM( 2 ) ? ( Qt::WindowFlags ) hb_parni( 2 ) : ( Qt::WindowFlags ) 0 ) ;

   hb_retptrGC( hbqt_gcAllocate_QLabel( ( void * ) pObj, true ) );
}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QLABEL_ALIGNMENT )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
   }
}

/*
 * QWidget * buddy () const
 */
HB_FUNC( QT_QLABEL_BUDDY )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->buddy(), false ) );
   }
}

/*
 * bool hasScaledContents () const
 */
HB_FUNC( QT_QLABEL_HASSCALEDCONTENTS )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retl( ( p )->hasScaledContents() );
   }
}

/*
 * int indent () const
 */
HB_FUNC( QT_QLABEL_INDENT )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retni( ( p )->indent() );
   }
}

/*
 * int margin () const
 */
HB_FUNC( QT_QLABEL_MARGIN )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retni( ( p )->margin() );
   }
}

/*
 * QMovie * movie () const
 */
HB_FUNC( QT_QLABEL_MOVIE )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMovie( ( p )->movie(), false ) );
   }
}

/*
 * bool openExternalLinks () const
 */
HB_FUNC( QT_QLABEL_OPENEXTERNALLINKS )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retl( ( p )->openExternalLinks() );
   }
}

/*
 * const QPicture * picture () const
 */
HB_FUNC( QT_QLABEL_PICTURE )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPicture( new QPicture( *( ( p )->picture() ) ), true ) );
   }
}

/*
 * const QPixmap * pixmap () const
 */
HB_FUNC( QT_QLABEL_PIXMAP )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( *( ( p )->pixmap() ) ), true ) );
   }
}

/*
 * void setAlignment ( Qt::Alignment )
 */
HB_FUNC( QT_QLABEL_SETALIGNMENT )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   }
}

/*
 * void setBuddy ( QWidget * buddy )
 */
HB_FUNC( QT_QLABEL_SETBUDDY )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setBuddy( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * void setIndent ( int )
 */
HB_FUNC( QT_QLABEL_SETINDENT )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setIndent( hb_parni( 2 ) );
   }
}

/*
 * void setMargin ( int )
 */
HB_FUNC( QT_QLABEL_SETMARGIN )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setMargin( hb_parni( 2 ) );
   }
}

/*
 * void setOpenExternalLinks ( bool open )
 */
HB_FUNC( QT_QLABEL_SETOPENEXTERNALLINKS )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setOpenExternalLinks( hb_parl( 2 ) );
   }
}

/*
 * void setScaledContents ( bool )
 */
HB_FUNC( QT_QLABEL_SETSCALEDCONTENTS )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setScaledContents( hb_parl( 2 ) );
   }
}

/*
 * void setTextFormat ( Qt::TextFormat )
 */
HB_FUNC( QT_QLABEL_SETTEXTFORMAT )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setTextFormat( ( Qt::TextFormat ) hb_parni( 2 ) );
   }
}

/*
 * void setTextInteractionFlags ( Qt::TextInteractionFlags flags )
 */
HB_FUNC( QT_QLABEL_SETTEXTINTERACTIONFLAGS )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setTextInteractionFlags( ( Qt::TextInteractionFlags ) hb_parni( 2 ) );
   }
}

/*
 * void setWordWrap ( bool on )
 */
HB_FUNC( QT_QLABEL_SETWORDWRAP )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setWordWrap( hb_parl( 2 ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QLABEL_TEXT )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
   }
}

/*
 * Qt::TextFormat textFormat () const
 */
HB_FUNC( QT_QLABEL_TEXTFORMAT )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retni( ( Qt::TextFormat ) ( p )->textFormat() );
   }
}

/*
 * Qt::TextInteractionFlags textInteractionFlags () const
 */
HB_FUNC( QT_QLABEL_TEXTINTERACTIONFLAGS )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retni( ( Qt::TextInteractionFlags ) ( p )->textInteractionFlags() );
   }
}

/*
 * bool wordWrap () const
 */
HB_FUNC( QT_QLABEL_WORDWRAP )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      hb_retl( ( p )->wordWrap() );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QLABEL_CLEAR )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * void setMovie ( QMovie * movie )
 */
HB_FUNC( QT_QLABEL_SETMOVIE )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setMovie( hbqt_par_QMovie( 2 ) );
   }
}

/*
 * void setNum ( int num )
 */
HB_FUNC( QT_QLABEL_SETNUM )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setNum( hb_parni( 2 ) );
   }
}

/*
 * void setNum ( double num )
 */
HB_FUNC( QT_QLABEL_SETNUM_1 )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setNum( hb_parnd( 2 ) );
   }
}

/*
 * void setPicture ( const QPicture & picture )
 */
HB_FUNC( QT_QLABEL_SETPICTURE )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setPicture( *hbqt_par_QPicture( 2 ) );
   }
}

/*
 * void setPixmap ( const QPixmap & )
 */
HB_FUNC( QT_QLABEL_SETPIXMAP )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      ( p )->setPixmap( *hbqt_par_QPixmap( 2 ) );
   }
}

/*
 * void setText ( const QString & )
 */
HB_FUNC( QT_QLABEL_SETTEXT )
{
   QLabel * p = hbqt_par_QLabel( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
