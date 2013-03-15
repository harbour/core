/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for error hanlding
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/* NOTE: This file is also used by C code. */

#ifndef _ERROR_CH
#define _ERROR_CH

/* Severity levels (oError:severity) */
#define ES_WHOCARES     0
#define ES_WARNING      1
#define ES_ERROR        2
#define ES_CATASTROPHIC 3

/* Generic error codes (oError:genCode) */
#define EG_ARG          1
#define EG_BOUND        2
#define EG_STROVERFLOW  3
#define EG_NUMOVERFLOW  4
#define EG_ZERODIV      5
#define EG_NUMERR       6
#define EG_SYNTAX       7
#define EG_COMPLEXITY   8

#define EG_MEM          11
#define EG_NOFUNC       12
#define EG_NOMETHOD     13
#define EG_NOVAR        14
#define EG_NOALIAS      15
#define EG_NOVARMETHOD  16
#define EG_BADALIAS     17
#define EG_DUPALIAS     18

#define EG_CREATE       20
#define EG_OPEN         21
#define EG_CLOSE        22
#define EG_READ         23
#define EG_WRITE        24
#define EG_PRINT        25

#define EG_UNSUPPORTED  30
#define EG_LIMIT        31
#define EG_CORRUPTION   32
#define EG_DATATYPE     33
#define EG_DATAWIDTH    34
#define EG_NOTABLE      35
#define EG_NOORDER      36
#define EG_SHARED       37
#define EG_UNLOCKED     38
#define EG_READONLY     39

#define EG_APPENDLOCK   40
#define EG_LOCK         41

#define EG_DESTRUCTOR   45      /* Harbour special */
#define EG_ARRACCESS    46      /* Harbour special */
#define EG_ARRASSIGN    47      /* Harbour special */
#define EG_ARRDIMENSION 48      /* Harbour special */
#define EG_NOTARRAY     49      /* Harbour special */
#define EG_CONDITION    50      /* Harbour special */

/* Internal errors */
#define HB_EI_ERRUNRECOV        9000 /* "Unrecoverable error %lu: " */
#define HB_EI_ERRRECFAILURE     9001 /* "Error recovery failure" */
#define HB_EI_ERRNOBLOCK        9002 /* "No ErrorBlock() for error" */
#define HB_EI_ERRTOOMANY        9003 /* "Too many recursive error handler calls" */
#define HB_EI_RDDINVALID        9004 /* "RDD invalid or failed to load" */
#define HB_EI_CLSINVMETHOD      9005 /* "Invalid method type from %s" */
#define HB_EI_XGRABALLOC        9006 /* "hb_xgrab can't allocate memory" */
#define HB_EI_XREALLOCNULL      9007 /* "hb_xrealloc called with a NULL pointer" */
#define HB_EI_XREALLOCINV       9008 /* "hb_xrealloc called with an invalid pointer" */
#define HB_EI_XREALLOC          9009 /* "hb_xrealloc can't reallocate memory" */
#define HB_EI_XFREEINV          9010 /* "hb_xfree called with an invalid pointer" */
#define HB_EI_XFREENULL         9011 /* "hb_xfree called with a NULL pointer" */
#define HB_EI_VMBADSTARTUP      9012 /* "Can\'t locate the starting procedure: \'%s\'" */
#define HB_EI_VMNOSTARTUP       9013 /* "No starting procedure" */
#define HB_EI_VMBADOPCODE       9014 /* "Unsupported VM opcode" */
#define HB_EI_VMNOTSYMBOL       9015 /* "Symbol item expected from %s" */
#define HB_EI_VMINVSYMBOL       9016 /* "Invalid symbol type for self from %s" */
#define HB_EI_VMNOTCBLOCK       9017 /* "Codeblock expected from %s" */
#define HB_EI_VMPOPINVITEM      9018 /* "Incorrect item type on the stack trying to pop from %s" */
#define HB_EI_STACKUFLOW        9019 /* "Stack underflow" */
#define HB_EI_ITEMBADCOPY       9020 /* "An item was going to be copied to itself from %s" */
#define HB_EI_MVBADSYMBOL       9021 /* "Invalid symbol item passed as memvar %s" */
#define HB_EI_XMEMOVERFLOW      9022 /* "Memory buffer overflow" */
#define HB_EI_XGRABNULLSIZE     9023 /* "hb_xgrab requested to allocate zero byte" */
#define HB_EI_XREALLOCNULLSIZE  9024 /* "hb_xrealloc requested to resize to zero byte" */
#define HB_EI_XALLOCNULLSIZE    9025 /* "hb_xalloc requested to allocate zero byte" */

#define HB_EI_COMPBADOPCODE     9100 /* "Unsupported VM opcode" */
#define HB_EI_COMPBADOPSIZE     9101 /* "Invalid opcode size" */

#endif /* _ERROR_CH */
