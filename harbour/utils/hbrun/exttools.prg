/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The declarations for the functions/procedures defined in TOOLS.
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#include "hbsetup.ch"

// Files from: tools
//
//
//symbols from file: tools\asciisum.c
//
EXTERNAL GT_ASCIISUM
//
//symbols from file: tools\ascpos.c
//
EXTERNAL GT_ASCPOS
//
//symbols from file: tools\atdiff.c
//
EXTERNAL GT_ATDIFF
//
//symbols from file: tools\chareven.c
//
EXTERNAL GT_CHAREVEN
//
//symbols from file: tools\charmix.c
//
EXTERNAL GT_CHARMIX
//
//symbols from file: tools\charodd.c
//
EXTERNAL GT_CHARODD
//
//symbols from file: tools\chrcount.c
//
EXTERNAL GT_CHRCOUNT
//
//symbols from file: tools\chrfirst.c
//
EXTERNAL GT_CHRFIRST
//
//symbols from file: tools\chrtotal.c
//
EXTERNAL GT_CHRTOTAL
//
//symbols from file: tools\ctchksum.c
//
EXTERNAL CT_CHECKSUM
//
//symbols from file: tools\ctchrmix.c
//
EXTERNAL CT_CHARMIX
//
//symbols from file: tools\ctcrypt.c
//
EXTERNAL CT_CRYPT
//
//symbols from file: tools\dates2.c
//
EXTERNAL AMONTHS
EXTERNAL ADAYS
EXTERNAL ISLEAPYEAR
EXTERNAL DAYSINMONTH
EXTERNAL EOM
EXTERNAL BOM
EXTERNAL WOM
EXTERNAL DOY
EXTERNAL WOY
EXTERNAL EOY
EXTERNAL BOY
EXTERNAL DATETIME
//
//symbols from file: tools\dbftools.c
//
EXTERNAL FIELDTYPE
EXTERNAL FIELDSIZE
EXTERNAL FIELDDECI
//
//symbols from file: tools\hb_f.c
//
EXTERNAL HB_FUSE
EXTERNAL HB_FRECNO
EXTERNAL HB_FSKIP
EXTERNAL HB_FREADLN
EXTERNAL HB_FEOF
EXTERNAL HB_FGOTO
EXTERNAL HB_FGOBOTTOM
EXTERNAL HB_FGOTOP
EXTERNAL HB_FLASTREC
EXTERNAL HB_FSELECT
//
//symbols from file: tools\io.c
//
EXTERNAL CD
EXTERNAL MD
EXTERNAL RD
EXTERNAL DISKUSED
EXTERNAL DISKFREE
EXTERNAL DISKFULL
//
//symbols from file: tools\mathx.c
//
EXTERNAL ACOS
EXTERNAL ASIN
EXTERNAL ATAN
EXTERNAL COS
EXTERNAL COSH
EXTERNAL LOG10
EXTERNAL SIN
EXTERNAL SINH
EXTERNAL TAN
EXTERNAL TANH
EXTERNAL PI
//
//symbols from file: tools\strcount.c
//
EXTERNAL GT_STRCOUNT
//
//symbols from file: tools\strcspn.c
//
EXTERNAL GT_STRCSPN
//
//symbols from file: tools\strdiff.c
//
EXTERNAL GT_STRDIFF
//
//symbols from file: tools\strexpan.c
//
EXTERNAL GT_STREXPAND
//
//symbols from file: tools\strfmt.c
//
EXTERNAL STRFORMAT
//
//symbols from file: tools\stringsx.c
//
EXTERNAL STRTOKEN
EXTERNAL STRDUMP
EXTERNAL ROT13
//
//symbols from file: tools\strleft.c
//
EXTERNAL GT_STRLEFT
//
//symbols from file: tools\strpbrk.c
//
EXTERNAL GT_STRPBRK
//
//symbols from file: tools\strright.c
//
EXTERNAL GT_STRRIGHT
//
//symbols from file: tools\fileread.prg
//
EXTERNAL TFILEREAD
//
//symbols from file: tools\nconvert.prg
//
EXTERNAL ISBIN
EXTERNAL ISOCTAL
EXTERNAL ISDEC
EXTERNAL ISHEXA
EXTERNAL DECTOBIN
EXTERNAL DECTOOCTAL
EXTERNAL DECTOHEXA
EXTERNAL BINTODEC
EXTERNAL OCTALTODEC
EXTERNAL HEXATODEC
//
//symbols from file: tools\numtxten.prg
//
EXTERNAL NUMTOTXTEN
//
//symbols from file: tools\numtxthu.prg
//
EXTERNAL NUMTOTXTHU
//
//symbols from file: tools\stringp.prg
//
EXTERNAL DEFAULT
EXTERNAL TOCHAR
EXTERNAL DEBUG

