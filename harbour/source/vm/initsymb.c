/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Forces initialization of runtime support symbols
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

#include "extend.h"
#include "ctoharb.h"

extern HARBOUR HB_AADD( void );
extern HARBOUR HB_ABS( void );
extern HARBOUR HB_ASC( void );
extern HARBOUR HB_AT( void );
extern HARBOUR HB_BOF( void );
extern HARBOUR HB_BREAK( void );
extern HARBOUR HB_CDOW( void );
extern HARBOUR HB_CHR( void );
extern HARBOUR HB_CMONTH( void );
extern HARBOUR HB_COL( void );
extern HARBOUR HB_CTOD( void );
extern HARBOUR HB_DATE( void );
extern HARBOUR HB_DAY( void );
extern HARBOUR HB_DELETED( void );
extern HARBOUR HB_DEVPOS( void );
extern HARBOUR HB_DOW( void );
extern HARBOUR HB_DTOC( void );
extern HARBOUR HB_DTOS( void );
extern HARBOUR HB_EMPTY( void );
extern HARBOUR HB_EOF( void );
extern HARBOUR HB_EXP( void );
extern HARBOUR HB_FCOUNT( void );
extern HARBOUR HB_FIELDNAME( void );
extern HARBOUR HB_FLOCK( void );
extern HARBOUR HB_FOUND( void );
extern HARBOUR HB_INKEY( void );
extern HARBOUR HB_INT( void );
extern HARBOUR HB_LASTREC( void );
extern HARBOUR HB_LEFT( void );
extern HARBOUR HB_LEN( void );
extern HARBOUR HB_LOCK( void );
extern HARBOUR HB_LOG( void );
extern HARBOUR HB_LOWER( void );
extern HARBOUR HB_LTRIM( void );
extern HARBOUR HB_MAX( void );
extern HARBOUR HB_MIN( void );
extern HARBOUR HB_MONTH( void );
extern HARBOUR HB_PCOL( void );
extern HARBOUR HB_PCOUNT( void );
extern HARBOUR HB_PROW( void );
extern HARBOUR HB_RECCOUNT( void );
extern HARBOUR HB_RECNO( void );
extern HARBOUR HB_REPLICATE( void );
extern HARBOUR HB_RLOCK( void );
extern HARBOUR HB_ROUND( void );
extern HARBOUR HB_ROW( void );
extern HARBOUR HB_RTRIM( void );
extern HARBOUR HB_SECONDS( void );
extern HARBOUR HB_SELECT( void );
extern HARBOUR HB_SETPOS( void );
extern HARBOUR HB_SETPOSBS( void );
extern HARBOUR HB_SPACE( void );
extern HARBOUR HB_SQRT( void );
extern HARBOUR HB_STR( void );
extern HARBOUR HB_SUBSTR( void );
extern HARBOUR HB_TIME( void );
extern HARBOUR HB_TRANSFORM( void );
extern HARBOUR HB_TRIM( void );
extern HARBOUR HB_TYPE( void );
extern HARBOUR HB_UPPER( void );
extern HARBOUR HB_VAL( void );
extern HARBOUR HB_WORD( void );
extern HARBOUR HB_YEAR( void );

static HB_SYMB symbols[] = {
   { "AADD"      , FS_PUBLIC, HB_AADD         , 0 },
   { "ABS"       , FS_PUBLIC, HB_ABS          , 0 },
   { "ASC"       , FS_PUBLIC, HB_ASC          , 0 },
   { "AT"        , FS_PUBLIC, HB_AT           , 0 },
   { "BOF"       , FS_PUBLIC, HB_BOF          , 0 },
   { "BREAK"     , FS_PUBLIC, HB_BREAK        , 0 },
   { "CDOW"      , FS_PUBLIC, HB_CDOW         , 0 },
   { "CHR"       , FS_PUBLIC, HB_CHR          , 0 },
   { "CMONTH"    , FS_PUBLIC, HB_CMONTH       , 0 },
   { "COL"       , FS_PUBLIC, HB_COL          , 0 },
   { "CTOD"      , FS_PUBLIC, HB_CTOD         , 0 },
   { "DATE"      , FS_PUBLIC, HB_DATE         , 0 },
   { "DAY"       , FS_PUBLIC, HB_DAY          , 0 },
   { "DELETED"   , FS_PUBLIC, HB_DELETED      , 0 },
   { "DEVPOS"    , FS_PUBLIC, HB_DEVPOS       , 0 },
   { "DOW"       , FS_PUBLIC, HB_DOW          , 0 },
   { "DTOC"      , FS_PUBLIC, HB_DTOC         , 0 },
   { "DTOS"      , FS_PUBLIC, HB_DTOS         , 0 },
   { "EMPTY"     , FS_PUBLIC, HB_EMPTY        , 0 },
   { "EOF"       , FS_PUBLIC, HB_EOF          , 0 },
   { "EXP"       , FS_PUBLIC, HB_EXP          , 0 },
   { "FCOUNT"    , FS_PUBLIC, HB_FCOUNT       , 0 },
   { "FIELDNAME" , FS_PUBLIC, HB_FIELDNAME    , 0 },
   { "FLOCK"     , FS_PUBLIC, HB_FLOCK        , 0 },
   { "FOUND"     , FS_PUBLIC, HB_FOUND        , 0 },
   { "INKEY"     , FS_PUBLIC, HB_INKEY        , 0 },
   { "INT"       , FS_PUBLIC, HB_INT          , 0 },
   { "LASTREC"   , FS_PUBLIC, HB_LASTREC      , 0 },
   { "LEFT"      , FS_PUBLIC, HB_LEFT         , 0 },
   { "LEN"       , FS_PUBLIC, HB_LEN          , 0 },
   { "LOCK"      , FS_PUBLIC, HB_LOCK         , 0 },
   { "LOG"       , FS_PUBLIC, HB_LOG          , 0 },
   { "LOWER"     , FS_PUBLIC, HB_LOWER        , 0 },
   { "LTRIM"     , FS_PUBLIC, HB_LTRIM        , 0 },
   { "MAX"       , FS_PUBLIC, HB_MAX          , 0 },
   { "MIN"       , FS_PUBLIC, HB_MIN          , 0 },
   { "MONTH"     , FS_PUBLIC, HB_MONTH        , 0 },
   { "PCOL"      , FS_PUBLIC, HB_PCOL         , 0 },
   { "PCOUNT"    , FS_PUBLIC, HB_PCOUNT       , 0 },
   { "PROW"      , FS_PUBLIC, HB_PROW         , 0 },
   { "RECCOUNT"  , FS_PUBLIC, HB_RECCOUNT     , 0 },
   { "RECNO"     , FS_PUBLIC, HB_RECNO        , 0 },
   { "REPLICATE" , FS_PUBLIC, HB_REPLICATE    , 0 },
   { "RLOCK"     , FS_PUBLIC, HB_RLOCK        , 0 },
   { "ROUND"     , FS_PUBLIC, HB_ROUND        , 0 },
   { "ROW"       , FS_PUBLIC, HB_ROW          , 0 },
   { "RTRIM"     , FS_PUBLIC, HB_RTRIM        , 0 },
   { "SECONDS"   , FS_PUBLIC, HB_SECONDS      , 0 },
   { "SELECT"    , FS_PUBLIC, HB_SELECT       , 0 },
   { "SETPOS"    , FS_PUBLIC, HB_SETPOS       , 0 },
   { "SETPOSBS"  , FS_PUBLIC, HB_SETPOSBS     , 0 },
   { "SPACE"     , FS_PUBLIC, HB_SPACE        , 0 },
   { "SQRT"      , FS_PUBLIC, HB_SQRT         , 0 },
   { "STR"       , FS_PUBLIC, HB_STR          , 0 },
   { "SUBSTR"    , FS_PUBLIC, HB_SUBSTR       , 0 },
   { "TIME"      , FS_PUBLIC, HB_TIME         , 0 },
   { "TRANSFORM" , FS_PUBLIC, HB_TRANSFORM    , 0 },
   { "TRIM"      , FS_PUBLIC, HB_TRIM         , 0 },
   { "TYPE"      , FS_PUBLIC, HB_TYPE         , 0 },
   { "UPPER"     , FS_PUBLIC, HB_UPPER        , 0 },
   { "VAL"       , FS_PUBLIC, HB_VAL          , 0 },
   { "WORD"      , FS_PUBLIC, HB_WORD         , 0 },
   { "YEAR"      , FS_PUBLIC, HB_YEAR         , 0 }
};

/* Registers runtime support functions symbols */

/* NOTE: The system symbol table with runtime functions HAVE TO be called
         last */

void hb_vmSymbolInit_RT( void )
{
   hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) );
}

