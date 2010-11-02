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
 *  enum StandardLocation { DesktopLocation, DocumentsLocation, FontsLocation, ApplicationsLocation, ..., CacheLocation }
 */

/*
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QDesktopServices>


/*
 *
 *
 */

typedef struct
{
   QDesktopServices * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDesktopServices;

HBQT_GC_FUNC( hbqt_gcRelease_QDesktopServices )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDesktopServices( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QDesktopServices * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesktopServices;
   p->type = HBQT_TYPE_QDesktopServices;

   return p;
}

HB_FUNC( QT_QDESKTOPSERVICES )
{
   //__HB_RETPTRGC__( QDesktopServices() );
}

/* QString displayName ( StandardLocation type ) */
HB_FUNC( QT_QDESKTOPSERVICES_DISPLAYNAME )
{
   hb_retstr_utf8( QDesktopServices::displayName( ( QDesktopServices::StandardLocation ) hb_parni( 2 ) ).toUtf8().data() );
}

/* bool openUrl ( const QUrl & url ) */
HB_FUNC( QT_QDESKTOPSERVICES_OPENURL )
{
   hb_retl( QDesktopServices::openUrl( *hbqt_par_QUrl( 2 ) ) );
}

/* void setUrlHandler ( const QString & scheme, QObject * receiver, const char * method ) */
HB_FUNC( QT_QDESKTOPSERVICES_SETURLHANDLER )
{
   void * pText;
   QDesktopServices::setUrlHandler( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ) );
   hb_strfree( pText );
}

/* QString storageLocation ( StandardLocation type ) */
HB_FUNC( QT_QDESKTOPSERVICES_STORAGELOCATION )
{
   hb_retstr_utf8( QDesktopServices::storageLocation( ( QDesktopServices::StandardLocation ) hb_parni( 2 ) ).toUtf8().data() );
}

/* void unsetUrlHandler ( const QString & scheme ) */
HB_FUNC( QT_QDESKTOPSERVICES_UNSETURLHANDLER )
{
   void * pText;
   QDesktopServices::unsetUrlHandler( hb_parstr_utf8( 2, &pText, NULL ) );
   hb_strfree( pText );
}


#endif /* #if QT_VERSION >= 0x040500 */
