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

#include "hbapi.h"
#include "hbvm.h"

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
   { "AADD"      , _HB_FS_PUBLIC, HB_AADD         , 0 },
   { "ABS"       , _HB_FS_PUBLIC, HB_ABS          , 0 },
   { "ASC"       , _HB_FS_PUBLIC, HB_ASC          , 0 },
   { "AT"        , _HB_FS_PUBLIC, HB_AT           , 0 },
   { "BOF"       , _HB_FS_PUBLIC, HB_BOF          , 0 },
   { "BREAK"     , _HB_FS_PUBLIC, HB_BREAK        , 0 },
   { "CDOW"      , _HB_FS_PUBLIC, HB_CDOW         , 0 },
   { "CHR"       , _HB_FS_PUBLIC, HB_CHR          , 0 },
   { "CMONTH"    , _HB_FS_PUBLIC, HB_CMONTH       , 0 },
   { "COL"       , _HB_FS_PUBLIC, HB_COL          , 0 },
   { "CTOD"      , _HB_FS_PUBLIC, HB_CTOD         , 0 },
   { "DATE"      , _HB_FS_PUBLIC, HB_DATE         , 0 },
   { "DAY"       , _HB_FS_PUBLIC, HB_DAY          , 0 },
   { "DELETED"   , _HB_FS_PUBLIC, HB_DELETED      , 0 },
   { "DEVPOS"    , _HB_FS_PUBLIC, HB_DEVPOS       , 0 },
   { "DOW"       , _HB_FS_PUBLIC, HB_DOW          , 0 },
   { "DTOC"      , _HB_FS_PUBLIC, HB_DTOC         , 0 },
   { "DTOS"      , _HB_FS_PUBLIC, HB_DTOS         , 0 },
   { "EMPTY"     , _HB_FS_PUBLIC, HB_EMPTY        , 0 },
   { "EOF"       , _HB_FS_PUBLIC, HB_EOF          , 0 },
   { "EXP"       , _HB_FS_PUBLIC, HB_EXP          , 0 },
   { "FCOUNT"    , _HB_FS_PUBLIC, HB_FCOUNT       , 0 },
   { "FIELDNAME" , _HB_FS_PUBLIC, HB_FIELDNAME    , 0 },
   { "FLOCK"     , _HB_FS_PUBLIC, HB_FLOCK        , 0 },
   { "FOUND"     , _HB_FS_PUBLIC, HB_FOUND        , 0 },
   { "INKEY"     , _HB_FS_PUBLIC, HB_INKEY        , 0 },
   { "INT"       , _HB_FS_PUBLIC, HB_INT          , 0 },
   { "LASTREC"   , _HB_FS_PUBLIC, HB_LASTREC      , 0 },
   { "LEFT"      , _HB_FS_PUBLIC, HB_LEFT         , 0 },
   { "LEN"       , _HB_FS_PUBLIC, HB_LEN          , 0 },
   { "LOCK"      , _HB_FS_PUBLIC, HB_LOCK         , 0 },
   { "LOG"       , _HB_FS_PUBLIC, HB_LOG          , 0 },
   { "LOWER"     , _HB_FS_PUBLIC, HB_LOWER        , 0 },
   { "LTRIM"     , _HB_FS_PUBLIC, HB_LTRIM        , 0 },
   { "MAX"       , _HB_FS_PUBLIC, HB_MAX          , 0 },
   { "MIN"       , _HB_FS_PUBLIC, HB_MIN          , 0 },
   { "MONTH"     , _HB_FS_PUBLIC, HB_MONTH        , 0 },
   { "PCOL"      , _HB_FS_PUBLIC, HB_PCOL         , 0 },
   { "PCOUNT"    , _HB_FS_PUBLIC, HB_PCOUNT       , 0 },
   { "PROW"      , _HB_FS_PUBLIC, HB_PROW         , 0 },
   { "RECCOUNT"  , _HB_FS_PUBLIC, HB_RECCOUNT     , 0 },
   { "RECNO"     , _HB_FS_PUBLIC, HB_RECNO        , 0 },
   { "REPLICATE" , _HB_FS_PUBLIC, HB_REPLICATE    , 0 },
   { "RLOCK"     , _HB_FS_PUBLIC, HB_RLOCK        , 0 },
   { "ROUND"     , _HB_FS_PUBLIC, HB_ROUND        , 0 },
   { "ROW"       , _HB_FS_PUBLIC, HB_ROW          , 0 },
   { "RTRIM"     , _HB_FS_PUBLIC, HB_RTRIM        , 0 },
   { "SECONDS"   , _HB_FS_PUBLIC, HB_SECONDS      , 0 },
   { "SELECT"    , _HB_FS_PUBLIC, HB_SELECT       , 0 },
   { "SETPOS"    , _HB_FS_PUBLIC, HB_SETPOS       , 0 },
   { "SETPOSBS"  , _HB_FS_PUBLIC, HB_SETPOSBS     , 0 },
   { "SPACE"     , _HB_FS_PUBLIC, HB_SPACE        , 0 },
   { "SQRT"      , _HB_FS_PUBLIC, HB_SQRT         , 0 },
   { "STR"       , _HB_FS_PUBLIC, HB_STR          , 0 },
   { "SUBSTR"    , _HB_FS_PUBLIC, HB_SUBSTR       , 0 },
   { "TIME"      , _HB_FS_PUBLIC, HB_TIME         , 0 },
   { "TRANSFORM" , _HB_FS_PUBLIC, HB_TRANSFORM    , 0 },
   { "TRIM"      , _HB_FS_PUBLIC, HB_TRIM         , 0 },
   { "TYPE"      , _HB_FS_PUBLIC, HB_TYPE         , 0 },
   { "UPPER"     , _HB_FS_PUBLIC, HB_UPPER        , 0 },
   { "VAL"       , _HB_FS_PUBLIC, HB_VAL          , 0 },
   { "WORD"      , _HB_FS_PUBLIC, HB_WORD         , 0 },
   { "YEAR"      , _HB_FS_PUBLIC, HB_YEAR         , 0 }
};

/* Registers runtime support functions symbols */

/* NOTE: The system symbol table with runtime functions HAVE TO be called
         last */

void hb_vmSymbolInit_RT( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmSymbolInit_RT()"));

   hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) );
}

