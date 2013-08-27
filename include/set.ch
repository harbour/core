/*
 * Harbour Project source code:
 * Header file for Set() function
 *
 * Copyright 1999-2003 David G. Holm <dholm@jsd-llc.com>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#ifndef _SET_CH
#define _SET_CH

#define _SET_EXACT            1
#define _SET_FIXED            2
#define _SET_DECIMALS         3
#define _SET_DATEFORMAT       4
#define _SET_EPOCH            5
#define _SET_PATH             6
#define _SET_DEFAULT          7

#define _SET_EXCLUSIVE        8
#define _SET_SOFTSEEK         9
#define _SET_UNIQUE           10
#define _SET_DELETED          11

#define _SET_CANCEL           12
#define _SET_DEBUG            13
#define _SET_TYPEAHEAD        14

#define _SET_COLOR            15
#define _SET_CURSOR           16
#define _SET_CONSOLE          17
#define _SET_ALTERNATE        18
#define _SET_ALTFILE          19
#define _SET_DEVICE           20
#define _SET_EXTRA            21
#define _SET_EXTRAFILE        22
#define _SET_PRINTER          23
#define _SET_PRINTFILE        24
#define _SET_MARGIN           25

#define _SET_BELL             26
#define _SET_CONFIRM          27
#define _SET_ESCAPE           28
#define _SET_INSERT           29
#define _SET_EXIT             30
#define _SET_INTENSITY        31
#define _SET_SCOREBOARD       32
#define _SET_DELIMITERS       33
#define _SET_DELIMCHARS       34

#define _SET_WRAP             35
#define _SET_MESSAGE          36
#define _SET_MCENTER          37
#define _SET_SCROLLBREAK      38

#define _SET_EVENTMASK        39  /* CA-Cl*pper 5.3 compatible */

#define _SET_VIDEOMODE        40  /* CA-Cl*pper 5.3 compatible */

#define _SET_MBLOCKSIZE       41  /* CA-Cl*pper 5.3 compatible */
#define _SET_MFILEEXT         42  /* CA-Cl*pper 5.3 compatible */

#define _SET_STRICTREAD       43  /* CA-Cl*pper 5.3 compatible */
#define _SET_OPTIMIZE         44  /* CA-Cl*pper 5.3 compatible */
#define _SET_AUTOPEN          45  /* CA-Cl*pper 5.3 compatible */
#define _SET_AUTORDER         46  /* CA-Cl*pper 5.3 compatible */
#define _SET_AUTOSHARE        47  /* CA-Cl*pper 5.3 compatible */

#define _SET_COUNT            47

#define _SET_LANGUAGE         100 /* Harbour extension */
#define _SET_IDLEREPEAT       101 /* Harbour extension */
#define _SET_FILECASE         102 /* Harbour extension */
#define _SET_DIRCASE          103 /* Harbour extension */
#define _SET_DIRSEPARATOR     104 /* Harbour extension */
#define _SET_EOF              105 /* Harbour extension */
#define _SET_HARDCOMMIT       106 /* Harbour extension */
#define _SET_FORCEOPT         107 /* Harbour extension */
#define _SET_DBFLOCKSCHEME    108 /* Harbour extension */
#define _SET_DEFEXTENSIONS    109 /* Harbour extension */
#define _SET_EOL              110 /* Harbour extension */
#define _SET_TRIMFILENAME     111 /* Harbour extension */
#define _SET_HBOUTLOG         112 /* Harbour extension */
#define _SET_HBOUTLOGINFO     113 /* Harbour extension */
#define _SET_CODEPAGE         114 /* Harbour extension */
#define _SET_OSCODEPAGE       115 /* Harbour extension */
#define _SET_TIMEFORMAT       116 /* Harbour extension */
#define _SET_DBCODEPAGE       117 /* Harbour extension */

#define HB_SET_BASE           100
#define HB_SET_COUNT          ( _SET_DBCODEPAGE - HB_SET_BASE + 1 )

#endif /* _SET_CH */
