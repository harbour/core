/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

#include "hbqt.h"

#ifndef __HBQTCORE_H
#define __HBQTCORE_H

#define hbqt_par_HBEvents( n )                      ( ( HBEvents                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBSlots( n )                       ( ( HBSlots                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractItemModel( n )            ( ( QAbstractItemModel          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractListModel( n )            ( ( QAbstractListModel          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractTableModel( n )           ( ( QAbstractTableModel         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QBitArray( n )                     ( ( QBitArray                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QBuffer( n )                       ( ( QBuffer                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QByteArray( n )                    ( ( QByteArray                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QChar( n )                         ( ( QChar                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QCoreApplication( n )              ( ( QCoreApplication            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDataStream( n )                   ( ( QDataStream                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDate( n )                         ( ( QDate                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDateTime( n )                     ( ( QDateTime                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDir( n )                          ( ( QDir                        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QEvent( n )                        ( ( QEvent                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QEventLoop( n )                    ( ( QEventLoop                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFile( n )                         ( ( QFile                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFileInfo( n )                     ( ( QFileInfo                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QIODevice( n )                     ( ( QIODevice                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLatin1Char( n )                   ( ( QLatin1Char                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLatin1String( n )                 ( ( QLatin1String               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLine( n )                         ( ( QLine                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLineF( n )                        ( ( QLineF                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QList( n )                         ( ( QList< void * >             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLocale( n )                       ( ( QLocale                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMimeData( n )                     ( ( QMimeData                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QModelIndex( n )                   ( ( QModelIndex                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QObject( n )                       ( ( QObject                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPoint( n )                        ( ( QPoint                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPointF( n )                       ( ( QPointF                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QProcess( n )                      ( ( QProcess                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QRect( n )                         ( ( QRect                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QRectF( n )                        ( ( QRectF                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QRegExp( n )                       ( ( QRegExp                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QResource( n )                     ( ( QResource                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSettings( n )                     ( ( QSettings                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSignalMapper( n )                 ( ( QSignalMapper               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSize( n )                         ( ( QSize                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSizeF( n )                        ( ( QSizeF                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStringList( n )                   ( ( QStringList                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextBoundaryFinder( n )           ( ( QTextBoundaryFinder         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextCodec( n )                    ( ( QTextCodec                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextDecoder( n )                  ( ( QTextDecoder                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextEncoder( n )                  ( ( QTextEncoder                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextStream( n )                   ( ( QTextStream                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QThread( n )                       ( ( QThread                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTime( n )                         ( ( QTime                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTimer( n )                        ( ( QTimer                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTranslator( n )                   ( ( QTranslator                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QUrl( n )                          ( ( QUrl                        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QVariant( n )                      ( ( QVariant                    * ) hbqt_gcpointer( n ) )

#endif /* __HBQTCORE_H */
