/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    constant values for CT3 serial communication com_*() functions
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
 * along with this software; see the file COPYING.txt.  If not, write to
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


/* com_MCR() */
#define MCR_ERROR             0     /* Parameter error */
#define MCR_DTR               1     /* Data terminal ready (DTR) */
#define MCR_RTS               2     /* Request to send (RTS) */
#define MCR_OUT_1             4     /* OUT 1 */
#define MCR_OUT_2             8     /* OUT 2 */
#define MCR_LOOP              16    /* LOOP */


/* com_MSR() */
#define MSR_ERROR             0     /* Parameter error */
#define MSR_DELTA_CTS         1     /* DELTA ready to send (DCTS) */
#define MSR_DELTA_DSR         2     /* DELTA data terminal ready (DDSR) */
#define MSR_TERI              4     /* Trailing edge RING (TERI) */
#define MSR_DELTA_DCD         8     /* DELTA data carrier detected (DDCD) */
#define MSR_CTS               16    /* Clear to send (CTS) */
#define MSR_DSR               32    /* Data terminal ready (DSR) */
#define MSR_RI                64    /* RING indicator (RI) */
#define MSR_DCD               128   /* Data carrier detected (DCD) */


/* com_LSR() */
#define LSR_ERROR             0     /* Parameter error */
#define LSR_DATA_READY        1     /* Data ready */
#define LSR_OVERRUN_ERR       2     /* Overflow error */
#define LSR_PARITY_ERR        4     /* Parity error */
#define LSR_FRAMING_ERR       8     /* Framing error */
#define LSR_BREAK             16    /* BREAK recognized */
#define LSR_TRANS_HOLD_EMPTY  32    /* Transmission holder register empty */
#define LSR_TRANS_EMPTY       64    /* TX shift register empty */


/* com_SMode() */
#define SMODE_EMPTY     1     /* Sending buffer empty */
#define SMODE_SOFT      2     /* Wait for software handshake release(XON) */
#define SMODE_HARD      4     /* Wait for hardware handshake release(CTS) */
#define SMODE_RFLUSH    8     /* Deleted from remote station */


/* com_CRC() polynomials */
#define Parity          3        /* 2^1 + 1^0 */
#define LCR_8           257      /* 2^8 + 1^0 */
#define CRC_12          5011     /* 2^12 + 2^11 + 2^3 + 2^2 + 2^1 + 1^0 */
#define CRC_16_X25      69665    /* 2^16 + 2^12 + 2^5 + 1^0 */
#define CRC_16          98309    /* 2^16 + 2^15 + 2^2 + 1^0 */
