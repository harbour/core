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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QLabel>


/*
 * QLabel ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * QLabel ( const QString & text, QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QLabel ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QLabel > pq;
} QGC_POINTER_QLabel;

QT_G_FUNC( release_QLabel )
{
   QGC_POINTER_QLabel * p = ( QGC_POINTER_QLabel * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QLabel                       p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QLabel                      ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QLabel * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QLabel * ) p->ph )->~QLabel();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QLabel * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QLabel                      Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QLabel                      Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QLabel                      Object Allready deleted!" ) );
   }
}

void * gcAllocate_QLabel( void * pObj )
{
   QGC_POINTER_QLabel * p = ( QGC_POINTER_QLabel * ) hb_gcAllocate( sizeof( QGC_POINTER_QLabel ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QLabel;
   new( & p->pq ) QPointer< QLabel >( ( QLabel * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QLabel                      %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QLABEL )
{
   void * pObj = NULL;

   pObj = ( QLabel* ) new QLabel( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( gcAllocate_QLabel( pObj ) );
}
/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QLABEL_ALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QLabel( 1 )->alignment() );
}

/*
 * QWidget * buddy () const
 */
HB_FUNC( QT_QLABEL_BUDDY )
{
   hb_retptr( ( QWidget* ) hbqt_par_QLabel( 1 )->buddy() );
}

/*
 * bool hasScaledContents () const
 */
HB_FUNC( QT_QLABEL_HASSCALEDCONTENTS )
{
   hb_retl( hbqt_par_QLabel( 1 )->hasScaledContents() );
}

/*
 * int indent () const
 */
HB_FUNC( QT_QLABEL_INDENT )
{
   hb_retni( hbqt_par_QLabel( 1 )->indent() );
}

/*
 * int margin () const
 */
HB_FUNC( QT_QLABEL_MARGIN )
{
   hb_retni( hbqt_par_QLabel( 1 )->margin() );
}

/*
 * QMovie * movie () const
 */
HB_FUNC( QT_QLABEL_MOVIE )
{
   hb_retptr( ( QMovie* ) hbqt_par_QLabel( 1 )->movie() );
}

/*
 * bool openExternalLinks () const
 */
HB_FUNC( QT_QLABEL_OPENEXTERNALLINKS )
{
   hb_retl( hbqt_par_QLabel( 1 )->openExternalLinks() );
}

/*
 * const QPicture * picture () const
 */
HB_FUNC( QT_QLABEL_PICTURE )
{
   hb_retptr( ( QPicture* ) hbqt_par_QLabel( 1 )->picture() );
}

/*
 * const QPixmap * pixmap () const
 */
HB_FUNC( QT_QLABEL_PIXMAP )
{
   hb_retptr( ( QPixmap* ) hbqt_par_QLabel( 1 )->pixmap() );
}

/*
 * void setAlignment ( Qt::Alignment )
 */
HB_FUNC( QT_QLABEL_SETALIGNMENT )
{
   hbqt_par_QLabel( 1 )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
 * void setBuddy ( QWidget * buddy )
 */
HB_FUNC( QT_QLABEL_SETBUDDY )
{
   hbqt_par_QLabel( 1 )->setBuddy( hbqt_par_QWidget( 2 ) );
}

/*
 * void setIndent ( int )
 */
HB_FUNC( QT_QLABEL_SETINDENT )
{
   hbqt_par_QLabel( 1 )->setIndent( hb_parni( 2 ) );
}

/*
 * void setMargin ( int )
 */
HB_FUNC( QT_QLABEL_SETMARGIN )
{
   hbqt_par_QLabel( 1 )->setMargin( hb_parni( 2 ) );
}

/*
 * void setOpenExternalLinks ( bool open )
 */
HB_FUNC( QT_QLABEL_SETOPENEXTERNALLINKS )
{
   hbqt_par_QLabel( 1 )->setOpenExternalLinks( hb_parl( 2 ) );
}

/*
 * void setScaledContents ( bool )
 */
HB_FUNC( QT_QLABEL_SETSCALEDCONTENTS )
{
   hbqt_par_QLabel( 1 )->setScaledContents( hb_parl( 2 ) );
}

/*
 * void setTextFormat ( Qt::TextFormat )
 */
HB_FUNC( QT_QLABEL_SETTEXTFORMAT )
{
   hbqt_par_QLabel( 1 )->setTextFormat( ( Qt::TextFormat ) hb_parni( 2 ) );
}

/*
 * void setTextInteractionFlags ( Qt::TextInteractionFlags flags )
 */
HB_FUNC( QT_QLABEL_SETTEXTINTERACTIONFLAGS )
{
   hbqt_par_QLabel( 1 )->setTextInteractionFlags( ( Qt::TextInteractionFlags ) hb_parni( 2 ) );
}

/*
 * void setWordWrap ( bool on )
 */
HB_FUNC( QT_QLABEL_SETWORDWRAP )
{
   hbqt_par_QLabel( 1 )->setWordWrap( hb_parl( 2 ) );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QLABEL_TEXT )
{
   hb_retc( hbqt_par_QLabel( 1 )->text().toAscii().data() );
}

/*
 * Qt::TextFormat textFormat () const
 */
HB_FUNC( QT_QLABEL_TEXTFORMAT )
{
   hb_retni( ( Qt::TextFormat ) hbqt_par_QLabel( 1 )->textFormat() );
}

/*
 * Qt::TextInteractionFlags textInteractionFlags () const
 */
HB_FUNC( QT_QLABEL_TEXTINTERACTIONFLAGS )
{
   hb_retni( ( Qt::TextInteractionFlags ) hbqt_par_QLabel( 1 )->textInteractionFlags() );
}

/*
 * bool wordWrap () const
 */
HB_FUNC( QT_QLABEL_WORDWRAP )
{
   hb_retl( hbqt_par_QLabel( 1 )->wordWrap() );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QLABEL_CLEAR )
{
   hbqt_par_QLabel( 1 )->clear();
}

/*
 * void setMovie ( QMovie * movie )
 */
HB_FUNC( QT_QLABEL_SETMOVIE )
{
   hbqt_par_QLabel( 1 )->setMovie( hbqt_par_QMovie( 2 ) );
}

/*
 * void setNum ( int num )
 */
HB_FUNC( QT_QLABEL_SETNUM )
{
   hbqt_par_QLabel( 1 )->setNum( hb_parni( 2 ) );
}

/*
 * void setNum ( double num )
 */
HB_FUNC( QT_QLABEL_SETNUM_1 )
{
   hbqt_par_QLabel( 1 )->setNum( hb_parnd( 2 ) );
}

/*
 * void setPicture ( const QPicture & picture )
 */
HB_FUNC( QT_QLABEL_SETPICTURE )
{
   hbqt_par_QLabel( 1 )->setPicture( *hbqt_par_QPicture( 2 ) );
}

/*
 * void setPixmap ( const QPixmap & )
 */
HB_FUNC( QT_QLABEL_SETPIXMAP )
{
   hbqt_par_QLabel( 1 )->setPixmap( *hbqt_par_QPixmap( 2 ) );
}

/*
 * void setText ( const QString & )
 */
HB_FUNC( QT_QLABEL_SETTEXT )
{
   hbqt_par_QLabel( 1 )->setText( hbqt_par_QString( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
