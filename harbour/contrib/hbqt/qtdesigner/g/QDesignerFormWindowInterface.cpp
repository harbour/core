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
#include "hbqtdesigner.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  flags Feature
 *  enum FeatureFlag { EditFeature, GridFeature, TabOrderFeature, DefaultFeature }
 */

#include <QtCore/QPointer>

#include "hbqtgui.h"

#include <QtDesigner/QDesignerFormWindowInterface>


/*
 * QDesignerFormWindowInterface ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QDesignerFormWindowInterface ()
 *
 */

typedef struct
{
   QPointer< QDesignerFormWindowInterface > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDesignerFormWindowInterface;

HBQT_GC_FUNC( hbqt_gcRelease_QDesignerFormWindowInterface )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDesignerFormWindowInterface( void * pObj, bool bNew )
{
   HBQT_GC_T_QDesignerFormWindowInterface * p = ( HBQT_GC_T_QDesignerFormWindowInterface * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDesignerFormWindowInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesignerFormWindowInterface >( ( QDesignerFormWindowInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesignerFormWindowInterface;
   p->type = HBQT_TYPE_QDesignerFormWindowInterface;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDesignerFormWindowInterface  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDesignerFormWindowInterface", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE )
{
   //hb_retptr( new QDesignerFormWindowInterface() );
}

/*
 * virtual void addResourceFile ( const QString & path ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_ADDRESOURCEFILE )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->addResourceFile( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * virtual QString author () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_AUTHOR )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->author().toUtf8().data() );
   }
}

/*
 * virtual QString comment () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_COMMENT )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->comment().toUtf8().data() );
   }
}

/*
 * virtual QString contents () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_CONTENTS )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->contents().toUtf8().data() );
   }
}

/*
 * virtual QDesignerFormEditorInterface * core () const
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_CORE )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormEditorInterface( ( p )->core(), false ) );
   }
}

/*
 * virtual QDesignerFormWindowCursorInterface * cursor () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_CURSOR )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowCursorInterface( ( p )->cursor(), false ) );
   }
}

/*
 * virtual void emitSelectionChanged () = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_EMITSELECTIONCHANGED )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->emitSelectionChanged();
   }
}

/*
 * virtual QString exportMacro () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_EXPORTMACRO )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->exportMacro().toUtf8().data() );
   }
}

/*
 * virtual Feature features () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_FEATURES )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retni( ( QDesignerFormWindowInterface::Feature ) ( p )->features() );
   }
}

/*
 * virtual QString fileName () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_FILENAME )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
   }
}

/*
 * virtual QPoint grid () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_GRID )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->grid() ), true ) );
   }
}

/*
 * virtual bool hasFeature ( Feature feature ) const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_HASFEATURE )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retl( ( p )->hasFeature( ( QDesignerFormWindowInterface::Feature ) hb_parni( 2 ) ) );
   }
}

/*
 * virtual QStringList includeHints () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_INCLUDEHINTS )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->includeHints() ), true ) );
   }
}

/*
 * virtual bool isDirty () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_ISDIRTY )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retl( ( p )->isDirty() );
   }
}

/*
 * virtual bool isManaged ( QWidget * widget ) const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_ISMANAGED )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retl( ( p )->isManaged( hbqt_par_QWidget( 2 ) ) );
   }
}

/*
 * virtual void layoutDefault ( int * margin, int * spacing ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_LAYOUTDEFAULT )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   int iMargin = 0;
   int iSpacing = 0;

   if( p )
   {
      ( p )->layoutDefault( &iMargin, &iSpacing );
   }

   hb_storni( iMargin, 2 );
   hb_storni( iSpacing, 3 );
}

/*
 * virtual QWidget * mainContainer () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_MAINCONTAINER )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->mainContainer(), false ) );
   }
}

/*
 * virtual QString pixmapFunction () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_PIXMAPFUNCTION )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->pixmapFunction().toUtf8().data() );
   }
}

/*
 * virtual void removeResourceFile ( const QString & path ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_REMOVERESOURCEFILE )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->removeResourceFile( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * virtual QStringList resourceFiles () const = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_RESOURCEFILES )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->resourceFiles() ), true ) );
   }
}

/*
 * virtual void setAuthor ( const QString & author ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETAUTHOR )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->setAuthor( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * virtual void setComment ( const QString & comment ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETCOMMENT )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->setComment( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * virtual void setContents ( QIODevice * device ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETCONTENTS )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->setContents( hbqt_par_QIODevice( 2 ) );
   }
}

/*
 * virtual void setExportMacro ( const QString & exportMacro ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETEXPORTMACRO )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->setExportMacro( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * virtual void setIncludeHints ( const QStringList & includeHints ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETINCLUDEHINTS )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->setIncludeHints( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * virtual void setLayoutDefault ( int margin, int spacing ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETLAYOUTDEFAULT )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->setLayoutDefault( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * virtual void setMainContainer ( QWidget * mainContainer ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETMAINCONTAINER )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->setMainContainer( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * virtual void setPixmapFunction ( const QString & pixmapFunction ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETPIXMAPFUNCTION )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPixmapFunction( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QDesignerFormWindowInterface * findFormWindow ( QWidget * widget )
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_FINDFORMWINDOW )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->findFormWindow( hbqt_par_QWidget( 2 ) ), false ) );
   }
}

/*
 * QDesignerFormWindowInterface * findFormWindow ( QObject * object )
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_FINDFORMWINDOW_1 )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDesignerFormWindowInterface( ( p )->findFormWindow( hbqt_par_QObject( 2 ) ), false ) );
   }
}

/*
 * virtual void clearSelection ( bool update = true ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_CLEARSELECTION )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->clearSelection( hb_parl( 2 ) );
   }
}

/*
 * virtual void manageWidget ( QWidget * widget ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_MANAGEWIDGET )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->manageWidget( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * virtual void selectWidget ( QWidget * widget, bool select = true ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SELECTWIDGET )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->selectWidget( hbqt_par_QWidget( 2 ), hb_parl( 3 ) );
   }
}

/*
 * virtual void setContents ( const QString & contents ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETCONTENTS_1 )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->setContents( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * virtual void setDirty ( bool dirty ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETDIRTY )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->setDirty( hb_parl( 2 ) );
   }
}

/*
 * virtual void setFeatures ( Feature features ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETFEATURES )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->setFeatures( ( QDesignerFormWindowInterface::Feature ) hb_parni( 2 ) );
   }
}

/*
 * virtual void setFileName ( const QString & fileName ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETFILENAME )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFileName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * virtual void setGrid ( const QPoint & grid ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_SETGRID )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->setGrid( *hbqt_par_QPoint( 2 ) );
   }
}

/*
 * virtual void unmanageWidget ( QWidget * widget ) = 0
 */
HB_FUNC( QT_QDESIGNERFORMWINDOWINTERFACE_UNMANAGEWIDGET )
{
   QDesignerFormWindowInterface * p = hbqt_par_QDesignerFormWindowInterface( 1 );
   if( p )
   {
      ( p )->unmanageWidget( hbqt_par_QWidget( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
