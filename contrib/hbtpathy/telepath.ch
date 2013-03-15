/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Telepathy emulation library (header)
 *
 * Copyright 2000, 2001 Dan Levitt <dan@boba-fett.net>
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

#ifndef TELEPATH_CH_
#define TELEPATH_CH_

#define TP_MAXPORTS  8

/* Error code definitions */
#define TE_PARAM           -1           /* Bad parameter */
#define TE_NOPORT          -2           /* No such port */
#define TE_CLOSED          -3           /* Port not open */
#define TE_CONFL           -4           /* IRQ conflict */
#define TE_TMOUT           -5           /* Timeout */
#define TE_NDCD            -6           /* Lost DCD */
#define TE_ESCAPE          -7           /* User escape */
#define TE_LENGTH          -8           /* Length limit */
#define TE_CANCEL          -9           /* Input canceled */
#define TE_NOHDL           -10          /* Out of handles */

#define TE_UCANCEL         -50          /* Canceled by user */
#define TE_RCANCEL         -51          /* Canceled by remote */
#define TE_STARTTM         -52          /* Timeout waiting to start */
#define TE_BLOCKTM         -53          /* Timeout waiting for block */
#define TE_ACKTM           -54          /* Timeout waiting for acknowledge */
#define TE_SENDTM          -55          /* Timeout waiting to send */
#define TE_CLEARTM         -56          /* Timeout waiting for sender to stop */
#define TE_NAK             -57          /* Negative acknowledge */
#define TE_BADACK          -58          /* Bad acknowledge character */
#define TE_BADBLK          -59          /* Bad block format */
#define TE_LONGBLK         -60          /* Long block received */
#define TE_ERRMAX          -61          /* Too many errors */
#define TE_DUPBLK          -62          /* Duplicate block */
#define TE_PROTO           -63          /* Protocol failure */
#define TE_CKSUM           -64          /* Checksum error */
#define TE_HDRTM           -65          /* Timeout waiting for Zmodem header */

#define TE_DISKFULL        -100         /* Disk full */
#define TE_NOFILE          -102         /* File not found */
#define TE_NOPATH          -103         /* Path not found */
#define TE_MFILE           -104         /* Too many open files */
#define TE_ACCESS          -105         /* Access denied */

/* File transfer status */
#define TXS_START          1            /* Start of transfer */
#define TXS_SFILE          2            /* Start of file */
#define TXS_NEWNAME        3            /* File renamed */
#define TXS_SDATA          4            /* Start of file data */
#define TXS_BLOCK          5            /* End of block */
#define TXS_ERROR          6            /* Error */
#define TXS_ABFILE         7            /* Aborting file */
#define TXS_ABORT          8            /* Aborting transfer */
#define TXS_WEND           9            /* Waiting for end of file */
#define TXS_EFILE          10           /* End of file */
#define TXS_END            11           /* End of transfer */

#define TP_32MAGIC         0x2144DF1C

/* Handshaking flags for tp_hshk() */
#define THS_RDSR           1            /* Require DSR */
#define THS_RCTS           2            /* Require CTS */
#define THS_RXOFF          4            /* Respect XON/XOFF */
#define THS_RDCD           8            /* Require DCD */
#define THS_ADTR           16           /* Assert DTR */
#define THS_CDTR           32           /* Flow control with DTR */
#define THS_ARTS           64           /* Assert RTS */
#define THS_CRTS           128          /* Flow control with RTS */
#define THS_SXOFF          256          /* Send XON/XOFF */

#endif /* TELEPATH_CH_ */
