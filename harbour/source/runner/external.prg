// $Id$
/*
   Harbour Project source code

   The declarations for all harbour defined functions/procedures.

   Copyright 1999  Ryszard Glab
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
*/

//
// Files from: vm
//
//
//symbols from file: vm/hvm.c
//
EXTERNAL ERRORSYS
EXTERNAL ERRORNEW
EXTERNAL EVAL
EXTERNAL LEN
EXTERNAL EMPTY
EXTERNAL VALTYPE
EXTERNAL ERRORBLOCK
EXTERNAL PROCNAME
EXTERNAL PROCLINE
EXTERNAL __QUIT
EXTERNAL ERRORLEVEL
EXTERNAL PCOUNT
EXTERNAL PVALUE
//
//symbols from file: vm/dynsym.c
//
EXTERNAL __DYNSCOUNT
EXTERNAL __DYNSGETNAME
EXTERNAL __DYNSGETINDEX
//
//symbols from file: vm/initsymb.c
//
//------------------------------------------------------------
// Files from: rtl
//
//
//symbols from file: rtl/arrays.c
//
EXTERNAL AADD
EXTERNAL ACLONE
EXTERNAL ACOPY
EXTERNAL ADEL
EXTERNAL AEVAL
EXTERNAL AFILL
EXTERNAL AINS
EXTERNAL ARRAY
EXTERNAL ASCAN
EXTERNAL ASIZE
EXTERNAL ASORT
EXTERNAL ATAIL
//
//symbols from file: rtl/classes.c
//
EXTERNAL __CLSNEW
EXTERNAL __CLSINST
EXTERNAL __CLSINSTSUPER
EXTERNAL __CLSADDMSG
EXTERNAL __CLSDELMSG
EXTERNAL __CLSMODMSG
EXTERNAL __CLS_CNTCLSDATA
EXTERNAL __CLS_CNTDATA
EXTERNAL __CLS_DECDATA
EXTERNAL __CLS_INCDATA
EXTERNAL __OBJGETCLSNAME
EXTERNAL __OBJCLONE
EXTERNAL __OBJHASMSG
EXTERNAL __OBJSENDMSG
EXTERNAL __OBJGETMSGLIST
EXTERNAL __OBJGETMETHODLIST
EXTERNAL __OBJGETVALUELIST
EXTERNAL __OBJSETVALUELIST
EXTERNAL __OBJHASDATA
EXTERNAL __OBJHASMETHOD
EXTERNAL __OBJADDDATA
EXTERNAL __OBJADDINLINE
EXTERNAL __OBJADDMETHOD
EXTERNAL __OBJDELDATA
EXTERNAL __OBJDELINLINE
EXTERNAL __OBJDELMETHOD
EXTERNAL __OBJMODINLINE
EXTERNAL __OBJMODMETHOD
//
//symbols from file: rtl/codebloc.c
//
//
//symbols from file: rtl/console.c
//
EXTERNAL __ACCEPT
EXTERNAL __EJECT
EXTERNAL COL
EXTERNAL ROW
EXTERNAL DBGSHADOW
EXTERNAL DEVOUT
EXTERNAL DEVOUTPICT
EXTERNAL DEVPOS
EXTERNAL DISPBEGIN
EXTERNAL DISPBOX
EXTERNAL DISPCOUNT
EXTERNAL DISPEND
EXTERNAL DISPOUT
EXTERNAL ISCOLOR
EXTERNAL MAXCOL
EXTERNAL MAXROW
EXTERNAL NOSNOW
EXTERNAL OUTSTD
EXTERNAL OUTERR
EXTERNAL PCOL
EXTERNAL PROW
EXTERNAL RESTSCREEN
EXTERNAL SCROLL
EXTERNAL SAVESCREEN
EXTERNAL SETCURSOR
EXTERNAL SETBLINK
EXTERNAL SETPOS
EXTERNAL SETPRC
EXTERNAL SHADOW
EXTERNAL QOUT
EXTERNAL QQOUT
EXTERNAL __COLORINDEX
//
//symbols from file: rtl/copyfile.c
//
EXTERNAL __COPYFILE
//
//symbols from file: rtl/dates.c
//
EXTERNAL CDOW
EXTERNAL CMONTH
EXTERNAL CTOD
EXTERNAL DATE
EXTERNAL DAY
EXTERNAL DOW
EXTERNAL DTOC
EXTERNAL DTOS
EXTERNAL MONTH
EXTERNAL SECONDS
EXTERNAL STOD
EXTERNAL TIME
EXTERNAL YEAR
//
//symbols from file: rtl/descend.c
//
EXTERNAL DESCEND
//
//symbols from file: rtl/dir.c
//
EXTERNAL DIRECTORY
//
//symbols from file: rtl/do.c
//
EXTERNAL DO
//
//symbols from file: rtl/environ.c
//
EXTERNAL __RUN
EXTERNAL GETENV
EXTERNAL OS
EXTERNAL VERSION
//
//symbols from file: rtl/errorapi.c
//
EXTERNAL __ERRRT_BASE
//
//symbols from file: rtl/extend.c
//
//
//symbols from file: rtl/filesys.c
//
EXTERNAL BIN2I
EXTERNAL BIN2L
EXTERNAL BIN2W
EXTERNAL FCLOSE
EXTERNAL FCREATE
EXTERNAL FERASE
EXTERNAL FERROR
EXTERNAL FILE
EXTERNAL FOPEN
EXTERNAL FREAD
EXTERNAL FREADSTR
EXTERNAL FRENAME
EXTERNAL FSEEK
EXTERNAL FWRITE
EXTERNAL CURDIR
EXTERNAL I2BIN
EXTERNAL L2BIN
EXTERNAL W2BIN
//
//symbols from file: rtl/fm.c
//
//
//symbols from file: rtl/gtapi.c
//
//
//symbols from file: rtl/gtxxx.c
//
//
//symbols from file: rtl/hardcr.c
//
EXTERNAL HARDCR
//
//symbols from file: rtl/inkey.c
//
EXTERNAL __KEYBOARD
EXTERNAL INKEY
EXTERNAL LASTKEY
EXTERNAL NEXTKEY
//
//symbols from file: rtl/itemapi.c
//
//
//symbols from file: rtl/langapi.c
//
//
//symbols from file: rtl/math.c
//
EXTERNAL ABS
EXTERNAL EXP
EXTERNAL INT
EXTERNAL LOG
EXTERNAL MAX
EXTERNAL MIN
EXTERNAL MOD
EXTERNAL ROUND
EXTERNAL SQRT
//
//symbols from file: rtl/memvars.c
//
EXTERNAL __MVPUBLIC
EXTERNAL __MVPRIVATE
EXTERNAL __MVXRELEASE
EXTERNAL __MVRELEASE
EXTERNAL __MVSCOPE
EXTERNAL __MVCLEAR
//
//symbols from file: rtl/msgxxx.c
//
//
//symbols from file: rtl/mtran.c
//
EXTERNAL MEMOTRAN
//
//symbols from file: rtl/set.c
//
EXTERNAL SET
EXTERNAL __SETCENTURY
//
//symbols from file: rtl/setcolor.c
//
EXTERNAL SETCOLOR
EXTERNAL COLORSELECT
//
//symbols from file: rtl/strings.c
//
EXTERNAL ALLTRIM
EXTERNAL ASC
EXTERNAL AT
EXTERNAL CHR
EXTERNAL ISALPHA
EXTERNAL ISDIGIT
EXTERNAL ISUPPER
EXTERNAL ISLOWER
EXTERNAL LEFT
EXTERNAL LOWER
EXTERNAL LTRIM
EXTERNAL PAD
EXTERNAL PADC
EXTERNAL PADL
EXTERNAL PADR
EXTERNAL RAT
EXTERNAL REPLICATE
EXTERNAL RIGHT
EXTERNAL RTRIM
EXTERNAL SPACE
EXTERNAL STR
EXTERNAL STRTRAN
EXTERNAL STUFF
EXTERNAL SUBSTR
EXTERNAL TRIM
EXTERNAL UPPER
EXTERNAL VAL
//
//symbols from file: rtl/transfrm.c
//
EXTERNAL TRANSFORM
//
//symbols from file: rtl/tone.c
//
EXTERNAL TONE
//
//symbols from file: rtl/achoice.prg
//
EXTERNAL ACHOICE
//
//symbols from file: rtl/adir.prg
//
EXTERNAL ADIR
//
//symbols from file: rtl/alert.prg
//
EXTERNAL ALERT
//
//symbols from file: rtl/asort.prg
//
//
//symbols from file: rtl/devoutp.prg
//
//
//symbols from file: rtl/terror.prg
//
//
//symbols from file: rtl/errorsys.prg
//
//
//symbols from file: rtl/menuto.prg
//
EXTERNAL __ATPROMPT
EXTERNAL __MENUTO
//
//symbols from file: rtl/objfunc.prg
//
//
//symbols from file: rtl/readvar.prg
//
EXTERNAL READVAR
//
//symbols from file: rtl/setkey.prg
//
EXTERNAL SETKEY
EXTERNAL SETKEYGET
EXTERNAL SETKEYSAVE
EXTERNAL SETKEYCHECK
//
//symbols from file: rtl/tclass.prg
//
EXTERNAL TCLASS
//
//symbols from file: rtl/tget.prg
//
EXTERNAL GETNEW
EXTERNAL _GET_
//
//symbols from file: rtl/tgetlist.prg
//
EXTERNAL READMODAL
//
//symbols from file: rtl/tbcolumn.prg
//
EXTERNAL TBCOLUMNNEW
//
//symbols from file: rtl/tbrowse.prg
//
EXTERNAL TBROWSENEW
//
//symbols from file: rtl/xsavescr.prg
//
EXTERNAL __XSAVESCREEN
EXTERNAL __XRESTSCREEN
//
//symbols from file: rtl/browdb.prg
//
EXTERNAL TBROWSEDB
//------------------------------------------------------------
// Files from: tools
//
//
//symbols from file: tools/asciisum.c
//
EXTERNAL GT_ASCIISUM
//
//symbols from file: tools/ascpos.c
//
EXTERNAL GT_ASCPOS
//
//symbols from file: tools/atdiff.c
//
EXTERNAL GT_ATDIFF
//
//symbols from file: tools/chareven.c
//
EXTERNAL GT_CHAREVEN
//
//symbols from file: tools/charmix.c
//
EXTERNAL GT_CHARMIX
//
//symbols from file: tools/charodd.c
//
EXTERNAL GT_CHARODD
//
//symbols from file: tools/chrcount.c
//
EXTERNAL GT_CHRCOUNT
//
//symbols from file: tools/chrfirst.c
//
EXTERNAL GT_CHRFIRST
//
//symbols from file: tools/chrtotal.c
//
EXTERNAL GT_CHRTOTAL
//
//symbols from file: tools/dates2.c
//
EXTERNAL ADAYS
EXTERNAL AMONTHS
EXTERNAL BOM
EXTERNAL BOY
EXTERNAL DAYSINMONTH
EXTERNAL DOY
EXTERNAL EOM
EXTERNAL EOY
EXTERNAL ISLEAPYEAR
EXTERNAL WOM
EXTERNAL WOY
//
//symbols from file: tools/datesx.c
//
EXTERNAL DATETIME
//
//symbols from file: tools/debug.c
//
EXTERNAL __ASTATIC
EXTERNAL __STATIC
EXTERNAL __GLOBALSTACKLEN
EXTERNAL __AGLOBALSTACK
EXTERNAL __STACKLEN
EXTERNAL __ASTACK
EXTERNAL __APARAM
//
//symbols from file: tools/hb_f.c
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
//symbols from file: tools/io.c
//
// removed manually
//
//symbols from file: tools/mathx.c
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
//symbols from file: tools/strasint.c
//
//
//symbols from file: tools/strcount.c
//
EXTERNAL GT_STRCOUNT
//
//symbols from file: tools/strcspn.c
//
EXTERNAL GT_STRCSPN
//
//symbols from file: tools/strdiff.c
//
EXTERNAL GT_STRDIFF
//
//symbols from file: tools/strexpan.c
//
EXTERNAL GT_STREXPAND
//
//symbols from file: tools/strfmt.c
//
EXTERNAL STRFORMAT
//
//symbols from file: tools/stringsx.c
//
EXTERNAL STRTOKEN
EXTERNAL STRDUMP
EXTERNAL ROT13
//
//symbols from file: tools/strleft.c
//
EXTERNAL GT_STRLEFT
//
//symbols from file: tools/strpbrk.c
//
EXTERNAL GT_STRPBRK
//
//symbols from file: tools/strright.c
//
EXTERNAL GT_STRRIGHT
//
//symbols from file: tools/fileread.prg
//
EXTERNAL TFILEREAD
//
//symbols from file: tools/numtxten.prg
//
EXTERNAL NUMTOTXTEN
//
//symbols from file: tools/numtxthu.prg
//
//EXTERNAL NUMTOTXTHU	//it uses undefined symbol STRZERO
//
//symbols from file: tools/stringp.prg
//
EXTERNAL DEFAULT
EXTERNAL TOCHAR
EXTERNAL DEBUG
//------------------------------------------------------------
