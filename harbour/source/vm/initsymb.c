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

extern HB_FUNC( AADD );
extern HB_FUNC( ABS );
extern HB_FUNC( ASC );
extern HB_FUNC( AT );
extern HB_FUNC( BOF );
extern HB_FUNC( BREAK );
extern HB_FUNC( CDOW );
extern HB_FUNC( CHR );
extern HB_FUNC( CMONTH );
extern HB_FUNC( COL );
extern HB_FUNC( CTOD );
extern HB_FUNC( DATE );
extern HB_FUNC( DAY );
extern HB_FUNC( DELETED );
extern HB_FUNC( DEVPOS );
extern HB_FUNC( DOW );
extern HB_FUNC( DTOC );
extern HB_FUNC( DTOS );
extern HB_FUNC( EMPTY );
extern HB_FUNC( EOF );
extern HB_FUNC( EXP );
extern HB_FUNC( FCOUNT );
extern HB_FUNC( FIELDNAME );
extern HB_FUNC( FLOCK );
extern HB_FUNC( FOUND );
extern HB_FUNC( INKEY );
extern HB_FUNC( INT );
extern HB_FUNC( LASTREC );
extern HB_FUNC( LEFT );
extern HB_FUNC( LEN );
extern HB_FUNC( LOCK );
extern HB_FUNC( LOG );
extern HB_FUNC( LOWER );
extern HB_FUNC( LTRIM );
extern HB_FUNC( MAX );
extern HB_FUNC( MIN );
extern HB_FUNC( MONTH );
extern HB_FUNC( PCOL );
extern HB_FUNC( PCOUNT );
extern HB_FUNC( PROW );
extern HB_FUNC( RECCOUNT );
extern HB_FUNC( RECNO );
extern HB_FUNC( REPLICATE );
extern HB_FUNC( RLOCK );
extern HB_FUNC( ROUND );
extern HB_FUNC( ROW );
extern HB_FUNC( RTRIM );
extern HB_FUNC( SECONDS );
extern HB_FUNC( SELECT );
extern HB_FUNC( SETPOS );
extern HB_FUNC( SETPOSBS );
extern HB_FUNC( SPACE );
extern HB_FUNC( SQRT );
extern HB_FUNC( STR );
extern HB_FUNC( SUBSTR );
extern HB_FUNC( TIME );
extern HB_FUNC( TRANSFORM );
extern HB_FUNC( TRIM );
extern HB_FUNC( TYPE );
extern HB_FUNC( UPPER );
extern HB_FUNC( VAL );
extern HB_FUNC( WORD );
extern HB_FUNC( YEAR );

static HB_SYMB symbols[] = {
   { "AADD"      , HB_FS_PUBLIC, HB_FUNCNAME( AADD )       , NULL },
   { "ABS"       , HB_FS_PUBLIC, HB_FUNCNAME( ABS )        , NULL },
   { "ASC"       , HB_FS_PUBLIC, HB_FUNCNAME( ASC )        , NULL },
   { "AT"        , HB_FS_PUBLIC, HB_FUNCNAME( AT )         , NULL },
   { "BOF"       , HB_FS_PUBLIC, HB_FUNCNAME( BOF )        , NULL },
   { "BREAK"     , HB_FS_PUBLIC, HB_FUNCNAME( BREAK )      , NULL },
   { "CDOW"      , HB_FS_PUBLIC, HB_FUNCNAME( CDOW )       , NULL },
   { "CHR"       , HB_FS_PUBLIC, HB_FUNCNAME( CHR )        , NULL },
   { "CMONTH"    , HB_FS_PUBLIC, HB_FUNCNAME( CMONTH )     , NULL },
   { "COL"       , HB_FS_PUBLIC, HB_FUNCNAME( COL )        , NULL },
   { "CTOD"      , HB_FS_PUBLIC, HB_FUNCNAME( CTOD )       , NULL },
   { "DATE"      , HB_FS_PUBLIC, HB_FUNCNAME( DATE )       , NULL },
   { "DAY"       , HB_FS_PUBLIC, HB_FUNCNAME( DAY )        , NULL },
   { "DELETED"   , HB_FS_PUBLIC, HB_FUNCNAME( DELETED )    , NULL },
   { "DEVPOS"    , HB_FS_PUBLIC, HB_FUNCNAME( DEVPOS )     , NULL },
   { "DOW"       , HB_FS_PUBLIC, HB_FUNCNAME( DOW )        , NULL },
   { "DTOC"      , HB_FS_PUBLIC, HB_FUNCNAME( DTOC )       , NULL },
   { "DTOS"      , HB_FS_PUBLIC, HB_FUNCNAME( DTOS )       , NULL },
   { "EMPTY"     , HB_FS_PUBLIC, HB_FUNCNAME( EMPTY )      , NULL },
   { "EOF"       , HB_FS_PUBLIC, HB_FUNCNAME( EOF )        , NULL },
   { "EXP"       , HB_FS_PUBLIC, HB_FUNCNAME( EXP )        , NULL },
   { "FCOUNT"    , HB_FS_PUBLIC, HB_FUNCNAME( FCOUNT )     , NULL },
   { "FIELDNAME" , HB_FS_PUBLIC, HB_FUNCNAME( FIELDNAME )  , NULL },
   { "FLOCK"     , HB_FS_PUBLIC, HB_FUNCNAME( FLOCK )      , NULL },
   { "FOUND"     , HB_FS_PUBLIC, HB_FUNCNAME( FOUND )      , NULL },
   { "INKEY"     , HB_FS_PUBLIC, HB_FUNCNAME( INKEY )      , NULL },
   { "INT"       , HB_FS_PUBLIC, HB_FUNCNAME( INT )        , NULL },
   { "LASTREC"   , HB_FS_PUBLIC, HB_FUNCNAME( LASTREC )    , NULL },
   { "LEFT"      , HB_FS_PUBLIC, HB_FUNCNAME( LEFT )       , NULL },
   { "LEN"       , HB_FS_PUBLIC, HB_FUNCNAME( LEN )        , NULL },
   { "LOCK"      , HB_FS_PUBLIC, HB_FUNCNAME( LOCK )       , NULL },
   { "LOG"       , HB_FS_PUBLIC, HB_FUNCNAME( LOG )        , NULL },
   { "LOWER"     , HB_FS_PUBLIC, HB_FUNCNAME( LOWER )      , NULL },
   { "LTRIM"     , HB_FS_PUBLIC, HB_FUNCNAME( LTRIM )      , NULL },
   { "MAX"       , HB_FS_PUBLIC, HB_FUNCNAME( MAX )        , NULL },
   { "MIN"       , HB_FS_PUBLIC, HB_FUNCNAME( MIN )        , NULL },
   { "MONTH"     , HB_FS_PUBLIC, HB_FUNCNAME( MONTH )      , NULL },
   { "PCOL"      , HB_FS_PUBLIC, HB_FUNCNAME( PCOL )       , NULL },
   { "PCOUNT"    , HB_FS_PUBLIC, HB_FUNCNAME( PCOUNT )     , NULL },
   { "PROW"      , HB_FS_PUBLIC, HB_FUNCNAME( PROW )       , NULL },
   { "RECCOUNT"  , HB_FS_PUBLIC, HB_FUNCNAME( RECCOUNT )   , NULL },
   { "RECNO"     , HB_FS_PUBLIC, HB_FUNCNAME( RECNO )      , NULL },
   { "REPLICATE" , HB_FS_PUBLIC, HB_FUNCNAME( REPLICATE )  , NULL },
   { "RLOCK"     , HB_FS_PUBLIC, HB_FUNCNAME( RLOCK )      , NULL },
   { "ROUND"     , HB_FS_PUBLIC, HB_FUNCNAME( ROUND )      , NULL },
   { "ROW"       , HB_FS_PUBLIC, HB_FUNCNAME( ROW )        , NULL },
   { "RTRIM"     , HB_FS_PUBLIC, HB_FUNCNAME( RTRIM )      , NULL },
   { "SECONDS"   , HB_FS_PUBLIC, HB_FUNCNAME( SECONDS )    , NULL },
   { "SELECT"    , HB_FS_PUBLIC, HB_FUNCNAME( SELECT )     , NULL },
   { "SETPOS"    , HB_FS_PUBLIC, HB_FUNCNAME( SETPOS )     , NULL },
   { "SETPOSBS"  , HB_FS_PUBLIC, HB_FUNCNAME( SETPOSBS )   , NULL },
   { "SPACE"     , HB_FS_PUBLIC, HB_FUNCNAME( SPACE )      , NULL },
   { "SQRT"      , HB_FS_PUBLIC, HB_FUNCNAME( SQRT )       , NULL },
   { "STR"       , HB_FS_PUBLIC, HB_FUNCNAME( STR )        , NULL },
   { "SUBSTR"    , HB_FS_PUBLIC, HB_FUNCNAME( SUBSTR )     , NULL },
   { "TIME"      , HB_FS_PUBLIC, HB_FUNCNAME( TIME )       , NULL },
   { "TRANSFORM" , HB_FS_PUBLIC, HB_FUNCNAME( TRANSFORM )  , NULL },
   { "TRIM"      , HB_FS_PUBLIC, HB_FUNCNAME( TRIM )       , NULL },
   { "TYPE"      , HB_FS_PUBLIC, HB_FUNCNAME( TYPE )       , NULL },
   { "UPPER"     , HB_FS_PUBLIC, HB_FUNCNAME( UPPER )      , NULL },
   { "VAL"       , HB_FS_PUBLIC, HB_FUNCNAME( VAL )        , NULL },
   { "WORD"      , HB_FS_PUBLIC, HB_FUNCNAME( WORD )       , NULL },
   { "YEAR"      , HB_FS_PUBLIC, HB_FUNCNAME( YEAR )       , NULL }
};

/* NOTE: The system symbol table with runtime functions HAVE TO be called
         last */

void hb_vmSymbolInit_RT( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmSymbolInit_RT()"));

   hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) );
}

