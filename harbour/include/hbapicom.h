/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    serial communication functions and constant values
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_APICOM_H_
#define HB_APICOM_H_

#include "hbcom.ch"
#include "hbapi.h"

HB_EXTERN_BEGIN

#define HB_COM_PORT_MAX       256

#define HB_COM_DEV_NAME_MAX   64

#define HB_COM_ANY            -1
#define HB_COM_DISABLED       0
#define HB_COM_ENABLED        1
#define HB_COM_OPEN           2

extern HB_EXPORT int  hb_comLastNum( void );
extern HB_EXPORT int  hb_comOpen( int iPort );
extern HB_EXPORT int  hb_comClose( int iPort );
extern HB_EXPORT int  hb_comInit( int iPort, int iBaud, int iParity, int iSize, int iStop );
extern HB_EXPORT long hb_comSend( int iPort, const void * data, long len, HB_MAXINT timeout );
extern HB_EXPORT long hb_comRecv( int iPort, void * data, long len, HB_MAXINT timeout );
extern HB_EXPORT void hb_comSetError( int iPort, int iError );
extern HB_EXPORT int  hb_comGetError( int iPort );
extern HB_EXPORT int  hb_comGetOsError( int iPort );
extern HB_EXPORT int  hb_comInputCount( int iPort );
extern HB_EXPORT int  hb_comOutputCount( int iPort );
extern HB_EXPORT int  hb_comFlush( int iPort, int iType );
extern HB_EXPORT int  hb_comMCR( int iPort, int * piValue, int iClr, int iSet );
extern HB_EXPORT int  hb_comMSR( int iPort, int * piValue );
extern HB_EXPORT int  hb_comLSR( int iPort, int * piValue );
extern HB_EXPORT int  hb_comSendBreak( int iPort, int iDurationInMilliSecs );
extern HB_EXPORT int  hb_comFlowControl( int iPort, int *piFlow, int iFlow );
extern HB_EXPORT int  hb_comFlowSet( int iPort, int iFlow );
extern HB_EXPORT int  hb_comFlowChars( int iPort, int iXONchar, int iXOFFchar );
extern HB_EXPORT int  hb_comDiscardChar( int iPort, int iChar );
extern HB_EXPORT int  hb_comErrorChar( int iPort, int iChar );
extern HB_EXPORT int  hb_comOutputState( int iPort );
extern HB_EXPORT int  hb_comInputState( int iPort );
extern HB_EXPORT int  hb_comSetDevice( int iPort, const char * szDevName );
extern HB_EXPORT const char * hb_comGetDevice( int iPort, char * buffer, int size );
extern HB_EXPORT HB_FHANDLE hb_comGetDeviceHandle( int iPort );

HB_EXTERN_END

#endif /* HB_APICOM_H_ */
