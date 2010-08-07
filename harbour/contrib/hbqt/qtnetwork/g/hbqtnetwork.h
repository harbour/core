/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

#ifndef __HBQTNETWORK_H
#define __HBQTNETWORK_H

#include "hbqt.h"

HB_EXTERN_BEGIN

extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QFtp );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QHttp );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QHttpHeader );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QHttpRequestHeader );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QHttpResponseHeader );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_QNetworkRequest );

extern HB_EXPORT void * hbqt_gcAllocate_QFtp( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QHttp( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QHttpHeader( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QHttpRequestHeader( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QHttpResponseHeader( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QNetworkRequest( void * pObj, bool bNew );

HB_EXTERN_END

#define hbqt_par_QFtp( n )                                      ( ( QFtp                                        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QHttp( n )                                     ( ( QHttp                                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QHttpHeader( n )                               ( ( QHttpHeader                                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QHttpRequestHeader( n )                        ( ( QHttpRequestHeader                          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QHttpResponseHeader( n )                       ( ( QHttpResponseHeader                         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QNetworkRequest( n )                           ( ( QNetworkRequest                             * ) hbqt_gcpointer( n ) )

#define HBQT_TYPE_QTNETWORK_BASE                                64000

#define HBQT_TYPE_QFtp                                          ( HBQT_TYPE_QTNETWORK_BASE + 1 )
#define HBQT_TYPE_QHttp                                         ( HBQT_TYPE_QTNETWORK_BASE + 2 )
#define HBQT_TYPE_QHttpHeader                                   ( HBQT_TYPE_QTNETWORK_BASE + 3 )
#define HBQT_TYPE_QHttpRequestHeader                            ( HBQT_TYPE_QTNETWORK_BASE + 4 )
#define HBQT_TYPE_QHttpResponseHeader                           ( HBQT_TYPE_QTNETWORK_BASE + 5 )
#define HBQT_TYPE_QNetworkRequest                               ( HBQT_TYPE_QTNETWORK_BASE + 6 )

#endif /* __HBQTNETWORK_H */
