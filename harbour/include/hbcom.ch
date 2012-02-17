/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Serial communication include file
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

/* NOTE: This file is also used by C code. */

#ifndef HB_COM_CH_
#define HB_COM_CH_

/* hb_comFlush() modes */
#define HB_COM_IFLUSH               1
#define HB_COM_OFLUSH               2
#define HB_COM_IOFLUSH              3

/* hb_comMCR() parameters */
#define HB_COM_MCR_DTR              0x01  /* Data terminal ready (DTR) TIOCM_DTR */
#define HB_COM_MCR_RTS              0x02  /* Request to send (RTS)     TIOCM_RTS */
#define HB_COM_MCR_OUT1             0x04  /* OUT 1                     TIOCM_OUT1 */
#define HB_COM_MCR_OUT2             0x08  /* OUT 2                     TIOCM_OUT2 */
#define HB_COM_MCR_LOOP             0x10  /* LOOP                      TIOCM_LOOP */

/* hb_comMSR() parameters */
#define HB_COM_MSR_DELTA_CTS        0x01  /* DELTA ready to send (DCTS) */
#define HB_COM_MSR_DELTA_DSR        0x02  /* DELTA data terminal ready (DDSR) */
#define HB_COM_MSR_TERI             0x04  /* Trailing edge RING (TERI) */
#define HB_COM_MSR_DELTA_DCD        0x08  /* DELTA data carrier detected (DDCD) */
#define HB_COM_MSR_CTS              0x10  /* Clear to send (CTS)         TIOCM_CTS */
#define HB_COM_MSR_DSR              0x20  /* Data terminal ready (DSR)   TIOCM_DSR */
#define HB_COM_MSR_RI               0x40  /* RING indicator (RI)         TIOCM_RI */
#define HB_COM_MSR_DCD              0x80  /* Data carrier detected (DCD) TIOCM_CD */

/* hb_comLSR() parameters */
#define HB_COM_LSR_DATA_READY       0x01  /* Data ready */
#define HB_COM_LSR_OVERRUN_ERR      0x02  /* Overflow error */
#define HB_COM_LSR_PARITY_ERR       0x04  /* Parity error */
#define HB_COM_LSR_FRAMING_ERR      0x08  /* Framing error */
#define HB_COM_LSR_BREAK            0x10  /* BREAK recognized */
#define HB_COM_LSR_TRANS_HOLD_EMPTY 0x20  /* Transmission holder register empty */
#define HB_COM_LSR_TRANS_EMPTY      0x40  /* TX shift register empty */

/* hb_comFlowControl() parameters */
#define HB_COM_FLOW_IRTSCTS         0x01  /* use RTS to stop input */
#define HB_COM_FLOW_ORTSCTS         0x02  /* check CTS on output */
#define HB_COM_FLOW_IDTRDSR         0x04  /* use DTR to stop input */
#define HB_COM_FLOW_ODTRDSR         0x08  /* check DSR on output */
#define HB_COM_FLOW_DCD             0x10  /* respect DCD */
#define HB_COM_FLOW_XOFF            0x20  /* XON/XOFF on input */
#define HB_COM_FLOW_XON             0x40  /* XON/XOFF on output */

/* hb_comFlowSet() parameters */
#define HB_COM_FL_OOFF              0x01
#define HB_COM_FL_OON               0x02
#define HB_COM_FL_IOFF              0x04
#define HB_COM_FL_ION               0x08
#define HB_COM_FL_SOFT              0x10
#define HB_COM_FL_RTSCTS            0x20
#define HB_COM_FL_DTRDSR            0x40
#define HB_COM_FL_DCD               0x80

/* hb_comOutputState() return values */
#define HB_COM_TX_CTS               0x01
#define HB_COM_TX_DSR               0x02
#define HB_COM_TX_DCD               0x04
#define HB_COM_TX_XOFF              0x08
#define HB_COM_TX_EMPTY             0x10
#define HB_COM_TX_RFLUSH            0x20

/* hb_comInputState() return values */
#define HB_COM_RX_XOFF              0x01

/* hb_comGetError() return values */
#define HB_COM_ERR_WRONGPORT        1
#define HB_COM_ERR_CLOSED           2
#define HB_COM_ERR_TIMEOUT          3
#define HB_COM_ERR_NOSUPPORT        4
#define HB_COM_ERR_PARAMVALUE       5
#define HB_COM_ERR_BUSY             6
#define HB_COM_ERR_OTHER            7
#define HB_COM_ERR_ALREADYOPEN      8
#define HB_COM_ERR_IO               9
#define HB_COM_ERR_PIPE             10

#endif /* HB_COM_CH_ */
