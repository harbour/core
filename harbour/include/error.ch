/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for error hanlding
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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

#define EG_ARRACCESS    46      /* Harbour special */
#define EG_ARRASSIGN    47      /* Harbour special */
#define EG_ARRDIMENSION 48      /* Harbour special */
#define EG_NOTARRAY     49      /* Harbour special */
#define EG_CONDITION    50      /* Harbour special */

/* Internal errors */
#define IE_ERRUNRECOV           9000
#define IE_ERRRECFAILURE        9001
#define IE_ERRNOBLOCK           9002
#define IE_ERRTOOMANY           9003
#define IE_RDDINVALID           9004
#define IE_CLSINVMETHOD         9005
#define IE_XGRABALLOC           9006
#define IE_XREALLOCNULL         9007
#define IE_XREALLOCINV          9008
#define IE_XREALLOC             9009
#define IE_XFREEINV             9010
#define IE_XFREENULL            9011
#define IE_VMBADSTARTUP         9012
#define IE_VMNOSTARTUP          9013
#define IE_VMBADOPCODE          9014
#define IE_VMNOTSYMBOL          9015
#define IE_VMINVSYMBOL          9016
#define IE_VMNOTCBLOCK          9017
#define IE_VMNOTNUMBER          9018
#define IE_VMNOTDOUBLE          9019
#define IE_STACKUFLOW           9020
#define IE_ITEMBADCOPY          9021
#define IE_MVBADSYMBOL          9022

#endif /* _ERROR_CH */
