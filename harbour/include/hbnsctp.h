/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Not SCTP (Stream Control Transmission Protocol)
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
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

#ifndef HB_NSCTP_H_
#define HB_NSCTP_H_

#include "hbapi.h"
#include "hbsocket.h"
#include "hbnsctp.ch"

HB_EXTERN_BEGIN

typedef struct
{
   HB_SOCKET    sd;
   char *       pSendBuffer;
   HB_SIZE      nSendLen;
   HB_SIZE      nSendPos;
   char *       pRecvBuffer;
   HB_SIZE      nRecvLen;
   HB_SIZE      nRecvSize;
   HB_SIZE      nLimit;
   HB_BOOL      fRecvHasSize;
   int          iError;
} HB_NSCTP, * PHB_NSCTP;

extern HB_EXPORT PHB_NSCTP hb_nsctpCreate( HB_SOCKET sd );
extern HB_EXPORT void      hb_nsctpDestroy( PHB_NSCTP pSocket );
extern HB_EXPORT int       hb_nsctpError( PHB_NSCTP pSocket );
extern HB_EXPORT void      hb_nsctpSetLimit( PHB_NSCTP pSocket, HB_SIZE nLimit );
extern HB_EXPORT HB_BOOL   hb_nsctpSend( PHB_NSCTP pSocket, const void * data, HB_SIZE len, HB_MAXINT timeout );
extern HB_EXPORT HB_BOOL   hb_nsctpRecv( PHB_NSCTP pSocket, void ** data, HB_SIZE * len, HB_MAXINT timeout );
extern HB_EXPORT HB_SIZE   hb_nsctpSendLen( PHB_NSCTP pSocket );
extern HB_EXPORT HB_SIZE   hb_nsctpRecvLen( PHB_NSCTP pSocket );

HB_EXTERN_END

#endif /* HB_NSCTP_H_ */
