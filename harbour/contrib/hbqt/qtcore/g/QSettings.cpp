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

#if QT_VERSION >= 0x040500

/*
 *  enum Format { NativeFormat, IniFormat, InvalidFormat }
 *  enum Scope { UserScope, SystemScope }
 *  enum Status { NoError, AccessError, FormatError }
 */

/*
 *  Constructed[ 31/31 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //Format registerFormat ( const QString & extension, ReadFunc readFunc, WriteFunc writeFunc, Qt::CaseSensitivity caseSensitivity = Qt::CaseSensitive )
 */

#include <QtCore/QPointer>

#include <QtCore/QSettings>
#include <QtCore/QStringList>

/*
 * QSettings ( const QString & organization, const QString & application = QString(), QObject * parent = 0 )
 * QSettings ( Scope scope, const QString & organization, const QString & application = QString(), QObject * parent =  * 0 )
 * QSettings ( Format format, Scope scope, const QString & organization, const QString & application = QString(),  * QObject * parent = 0 )
 * QSettings ( const QString & fileName, Format format, QObject * parent = 0 )
 * QSettings ( QObject * parent = 0 )
 * ~QSettings ()
 */

typedef struct
{
   QPointer< QSettings > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSettings;

HBQT_GC_FUNC( hbqt_gcRelease_QSettings )
{
   QSettings  * ph = NULL;
   HBQT_GC_T_QSettings * p = ( HBQT_GC_T_QSettings * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
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

void * hbqt_gcAllocate_QSettings( void * pObj, bool bNew )
{
   HBQT_GC_T_QSettings * p = ( HBQT_GC_T_QSettings * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSettings ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSettings >( ( QSettings * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSettings;
   p->type = HBQT_TYPE_QSettings;

   return p;
}

HB_FUNC( QT_QSETTINGS )
{
   QSettings * pObj = NULL;

   if( hb_pcount() >= 2 && HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      pObj = new QSettings( hbqt_par_QString( 1 ), hbqt_par_QString( 2 ), 0 ) ;
   }
   else if( hb_pcount() >= 2 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QSettings( hbqt_par_QString( 1 ), ( QSettings::Format ) hb_parni( 2 ) ) ;
   }
   else
   {
      pObj = new QSettings() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QSettings( ( void * ) pObj, true ) );
}

/* QStringList allKeys () const */
HB_FUNC( QT_QSETTINGS_ALLKEYS )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->allKeys() ), true ) );
}

/* QString applicationName () const */
HB_FUNC( QT_QSETTINGS_APPLICATIONNAME )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retstr_utf8( ( p )->applicationName().toUtf8().data() );
}

/* void beginGroup ( const QString & prefix ) */
HB_FUNC( QT_QSETTINGS_BEGINGROUP )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
   {
      void * pText;
      ( p )->beginGroup( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* int beginReadArray ( const QString & prefix ) */
HB_FUNC( QT_QSETTINGS_BEGINREADARRAY )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->beginReadArray( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* void beginWriteArray ( const QString & prefix, int size = -1 ) */
HB_FUNC( QT_QSETTINGS_BEGINWRITEARRAY )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
   {
      void * pText;
      ( p )->beginWriteArray( hb_parstr_utf8( 2, &pText, NULL ), hb_parnidef( 3, -1 ) );
      hb_strfree( pText );
   }
}

/* QStringList childGroups () const */
HB_FUNC( QT_QSETTINGS_CHILDGROUPS )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->childGroups() ), true ) );
}

/* QStringList childKeys () const */
HB_FUNC( QT_QSETTINGS_CHILDKEYS )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->childKeys() ), true ) );
}

/* void clear () */
HB_FUNC( QT_QSETTINGS_CLEAR )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      ( p )->clear();
}

/* bool contains ( const QString & key ) const */
HB_FUNC( QT_QSETTINGS_CONTAINS )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->contains( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* void endArray () */
HB_FUNC( QT_QSETTINGS_ENDARRAY )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      ( p )->endArray();
}

/* void endGroup () */
HB_FUNC( QT_QSETTINGS_ENDGROUP )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      ( p )->endGroup();
}

/* bool fallbacksEnabled () const */
HB_FUNC( QT_QSETTINGS_FALLBACKSENABLED )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retl( ( p )->fallbacksEnabled() );
}

/* QString fileName () const */
HB_FUNC( QT_QSETTINGS_FILENAME )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
}

/* Format format () const */
HB_FUNC( QT_QSETTINGS_FORMAT )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retni( ( QSettings::Format ) ( p )->format() );
}

/* QString group () const */
HB_FUNC( QT_QSETTINGS_GROUP )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retstr_utf8( ( p )->group().toUtf8().data() );
}

/* QTextCodec * iniCodec () const */
HB_FUNC( QT_QSETTINGS_INICODEC )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCodec( ( p )->iniCodec(), false ) );
}

/* bool isWritable () const */
HB_FUNC( QT_QSETTINGS_ISWRITABLE )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retl( ( p )->isWritable() );
}

/* QString organizationName () const */
HB_FUNC( QT_QSETTINGS_ORGANIZATIONNAME )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retstr_utf8( ( p )->organizationName().toUtf8().data() );
}

/* void remove ( const QString & key ) */
HB_FUNC( QT_QSETTINGS_REMOVE )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
   {
      void * pText;
      ( p )->remove( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* Scope scope () const */
HB_FUNC( QT_QSETTINGS_SCOPE )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retni( ( QSettings::Scope ) ( p )->scope() );
}

/* void setArrayIndex ( int i ) */
HB_FUNC( QT_QSETTINGS_SETARRAYINDEX )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      ( p )->setArrayIndex( hb_parni( 2 ) );
}

/* void setFallbacksEnabled ( bool b ) */
HB_FUNC( QT_QSETTINGS_SETFALLBACKSENABLED )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      ( p )->setFallbacksEnabled( hb_parl( 2 ) );
}

/* void setIniCodec ( QTextCodec * codec ) */
HB_FUNC( QT_QSETTINGS_SETINICODEC )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      ( p )->setIniCodec( hbqt_par_QTextCodec( 2 ) );
}

/* void setIniCodec ( const char * codecName ) */
HB_FUNC( QT_QSETTINGS_SETINICODEC_1 )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      ( p )->setIniCodec( ( const char * ) hb_parc( 2 ) );
}

/* void setValue ( const QString & key, const QVariant & value ) */
HB_FUNC( QT_QSETTINGS_SETVALUE )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
   {
      void * pText;
      ( p )->setValue( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QVariant( 3 ) );
      hb_strfree( pText );
   }
}

/* Status status () const */
HB_FUNC( QT_QSETTINGS_STATUS )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retni( ( QSettings::Status ) ( p )->status() );
}

/* void sync () */
HB_FUNC( QT_QSETTINGS_SYNC )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      ( p )->sync();
}

/* QVariant value ( const QString & key, const QVariant & defaultValue = QVariant() ) const */
HB_FUNC( QT_QSETTINGS_VALUE )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->value( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QVariant( 3 ) : QVariant() ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* Format defaultFormat () */
HB_FUNC( QT_QSETTINGS_DEFAULTFORMAT )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      hb_retni( ( QSettings::Format ) ( p )->defaultFormat() );
}

/* void setDefaultFormat ( Format format ) */
HB_FUNC( QT_QSETTINGS_SETDEFAULTFORMAT )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
      ( p )->setDefaultFormat( ( QSettings::Format ) hb_parni( 2 ) );
}

/* void setPath ( Format format, Scope scope, const QString & path ) */
HB_FUNC( QT_QSETTINGS_SETPATH )
{
   QSettings * p = hbqt_par_QSettings( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPath( ( QSettings::Format ) hb_parni( 2 ), ( QSettings::Scope ) hb_parni( 3 ), hb_parstr_utf8( 4, &pText, NULL ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
