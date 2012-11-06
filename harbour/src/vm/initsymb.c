/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Forces initialization of runtime support symbols
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

#include "hbapi.h"
#include "hbvm.h"

HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( ABS );
HB_FUNC_EXTERN( ASC );
HB_FUNC_EXTERN( AT );
HB_FUNC_EXTERN( BOF );
HB_FUNC_EXTERN( BREAK );
HB_FUNC_EXTERN( CDOW );
HB_FUNC_EXTERN( CHR );
HB_FUNC_EXTERN( CMONTH );
HB_FUNC_EXTERN( COL );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( DAY );
HB_FUNC_EXTERN( DELETED );
HB_FUNC_EXTERN( DEVPOS );
HB_FUNC_EXTERN( DOW );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DTOS );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( EXP );
HB_FUNC_EXTERN( FCOUNT );
HB_FUNC_EXTERN( FIELDNAME );
HB_FUNC_EXTERN( FLOCK );
HB_FUNC_EXTERN( FOUND );
HB_FUNC_EXTERN( INKEY );
HB_FUNC_EXTERN( INT );
HB_FUNC_EXTERN( LASTREC );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( LOCK );
HB_FUNC_EXTERN( LOG );
HB_FUNC_EXTERN( LOWER );
HB_FUNC_EXTERN( LTRIM );
HB_FUNC_EXTERN( MAX );
HB_FUNC_EXTERN( MIN );
HB_FUNC_EXTERN( MONTH );
HB_FUNC_EXTERN( PCOL );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( PROW );
HB_FUNC_EXTERN( RECCOUNT );
HB_FUNC_EXTERN( RECNO );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( RLOCK );
HB_FUNC_EXTERN( ROUND );
HB_FUNC_EXTERN( ROW );
HB_FUNC_EXTERN( RTRIM );
HB_FUNC_EXTERN( SECONDS );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( SETPOS );
HB_FUNC_EXTERN( SETPOSBS );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( SQRT );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( TIME );
HB_FUNC_EXTERN( TRANSFORM );
HB_FUNC_EXTERN( TRIM );
HB_FUNC_EXTERN( TYPE );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( WORD );
HB_FUNC_EXTERN( YEAR );

static HB_SYMB symbols[] = {
   { "AADD",      { HB_FS_PUBLIC }, { HB_FUNCNAME( AADD )      }, NULL },
   { "ABS",       { HB_FS_PUBLIC }, { HB_FUNCNAME( ABS )       }, NULL },
   { "ASC",       { HB_FS_PUBLIC }, { HB_FUNCNAME( ASC )       }, NULL },
   { "AT",        { HB_FS_PUBLIC }, { HB_FUNCNAME( AT )        }, NULL },
   { "BOF",       { HB_FS_PUBLIC }, { HB_FUNCNAME( BOF )       }, NULL },
   { "BREAK",     { HB_FS_PUBLIC }, { HB_FUNCNAME( BREAK )     }, NULL },
   { "CDOW",      { HB_FS_PUBLIC }, { HB_FUNCNAME( CDOW )      }, NULL },
   { "CHR",       { HB_FS_PUBLIC }, { HB_FUNCNAME( CHR )       }, NULL },
   { "CMONTH",    { HB_FS_PUBLIC }, { HB_FUNCNAME( CMONTH )    }, NULL },
   { "COL",       { HB_FS_PUBLIC }, { HB_FUNCNAME( COL )       }, NULL },
   { "CTOD",      { HB_FS_PUBLIC }, { HB_FUNCNAME( CTOD )      }, NULL },
   { "DATE",      { HB_FS_PUBLIC }, { HB_FUNCNAME( DATE )      }, NULL },
   { "DAY",       { HB_FS_PUBLIC }, { HB_FUNCNAME( DAY )       }, NULL },
   { "DELETED",   { HB_FS_PUBLIC }, { HB_FUNCNAME( DELETED )   }, NULL },
   { "DEVPOS",    { HB_FS_PUBLIC }, { HB_FUNCNAME( DEVPOS )    }, NULL },
   { "DOW",       { HB_FS_PUBLIC }, { HB_FUNCNAME( DOW )       }, NULL },
   { "DTOC",      { HB_FS_PUBLIC }, { HB_FUNCNAME( DTOC )      }, NULL },
   { "DTOS",      { HB_FS_PUBLIC }, { HB_FUNCNAME( DTOS )      }, NULL },
   { "EMPTY",     { HB_FS_PUBLIC }, { HB_FUNCNAME( EMPTY )     }, NULL },
   { "EOF",       { HB_FS_PUBLIC }, { HB_FUNCNAME( EOF )       }, NULL },
   { "EXP",       { HB_FS_PUBLIC }, { HB_FUNCNAME( EXP )       }, NULL },
   { "FCOUNT",    { HB_FS_PUBLIC }, { HB_FUNCNAME( FCOUNT )    }, NULL },
   { "FIELDNAME", { HB_FS_PUBLIC }, { HB_FUNCNAME( FIELDNAME ) }, NULL },
   { "FLOCK",     { HB_FS_PUBLIC }, { HB_FUNCNAME( FLOCK )     }, NULL },
   { "FOUND",     { HB_FS_PUBLIC }, { HB_FUNCNAME( FOUND )     }, NULL },
   { "INKEY",     { HB_FS_PUBLIC }, { HB_FUNCNAME( INKEY )     }, NULL },
   { "INT",       { HB_FS_PUBLIC }, { HB_FUNCNAME( INT )       }, NULL },
   { "LASTREC",   { HB_FS_PUBLIC }, { HB_FUNCNAME( LASTREC )   }, NULL },
   { "LEFT",      { HB_FS_PUBLIC }, { HB_FUNCNAME( LEFT )      }, NULL },
   { "LEN",       { HB_FS_PUBLIC }, { HB_FUNCNAME( LEN )       }, NULL },
   { "LOCK",      { HB_FS_PUBLIC }, { HB_FUNCNAME( LOCK )      }, NULL },
   { "LOG",       { HB_FS_PUBLIC }, { HB_FUNCNAME( LOG )       }, NULL },
   { "LOWER",     { HB_FS_PUBLIC }, { HB_FUNCNAME( LOWER )     }, NULL },
   { "LTRIM",     { HB_FS_PUBLIC }, { HB_FUNCNAME( LTRIM )     }, NULL },
   { "MAX",       { HB_FS_PUBLIC }, { HB_FUNCNAME( MAX )       }, NULL },
   { "MIN",       { HB_FS_PUBLIC }, { HB_FUNCNAME( MIN )       }, NULL },
   { "MONTH",     { HB_FS_PUBLIC }, { HB_FUNCNAME( MONTH )     }, NULL },
   { "PCOL",      { HB_FS_PUBLIC }, { HB_FUNCNAME( PCOL )      }, NULL },
   { "PCOUNT",    { HB_FS_PUBLIC }, { HB_FUNCNAME( PCOUNT )    }, NULL },
   { "PROW",      { HB_FS_PUBLIC }, { HB_FUNCNAME( PROW )      }, NULL },
   { "RECCOUNT",  { HB_FS_PUBLIC }, { HB_FUNCNAME( RECCOUNT )  }, NULL },
   { "RECNO",     { HB_FS_PUBLIC }, { HB_FUNCNAME( RECNO )     }, NULL },
   { "REPLICATE", { HB_FS_PUBLIC }, { HB_FUNCNAME( REPLICATE ) }, NULL },
   { "RLOCK",     { HB_FS_PUBLIC }, { HB_FUNCNAME( RLOCK )     }, NULL },
   { "ROUND",     { HB_FS_PUBLIC }, { HB_FUNCNAME( ROUND )     }, NULL },
   { "ROW",       { HB_FS_PUBLIC }, { HB_FUNCNAME( ROW )       }, NULL },
   { "RTRIM",     { HB_FS_PUBLIC }, { HB_FUNCNAME( RTRIM )     }, NULL },
   { "SECONDS",   { HB_FS_PUBLIC }, { HB_FUNCNAME( SECONDS )   }, NULL },
   { "SELECT",    { HB_FS_PUBLIC }, { HB_FUNCNAME( SELECT )    }, NULL },
   { "SETPOS",    { HB_FS_PUBLIC }, { HB_FUNCNAME( SETPOS )    }, NULL },
   { "SETPOSBS",  { HB_FS_PUBLIC }, { HB_FUNCNAME( SETPOSBS )  }, NULL },
   { "SPACE",     { HB_FS_PUBLIC }, { HB_FUNCNAME( SPACE )     }, NULL },
   { "SQRT",      { HB_FS_PUBLIC }, { HB_FUNCNAME( SQRT )      }, NULL },
   { "STR",       { HB_FS_PUBLIC }, { HB_FUNCNAME( STR )       }, NULL },
   { "SUBSTR",    { HB_FS_PUBLIC }, { HB_FUNCNAME( SUBSTR )    }, NULL },
   { "TIME",      { HB_FS_PUBLIC }, { HB_FUNCNAME( TIME )      }, NULL },
   { "TRANSFORM", { HB_FS_PUBLIC }, { HB_FUNCNAME( TRANSFORM ) }, NULL },
   { "TRIM",      { HB_FS_PUBLIC }, { HB_FUNCNAME( TRIM )      }, NULL },
   { "TYPE",      { HB_FS_PUBLIC }, { HB_FUNCNAME( TYPE )      }, NULL },
   { "UPPER",     { HB_FS_PUBLIC }, { HB_FUNCNAME( UPPER )     }, NULL },
   { "VAL",       { HB_FS_PUBLIC }, { HB_FUNCNAME( VAL )       }, NULL },
   { "WORD",      { HB_FS_PUBLIC }, { HB_FUNCNAME( WORD )      }, NULL },
   { "YEAR",      { HB_FS_PUBLIC }, { HB_FUNCNAME( YEAR )      }, NULL }
};

/* NOTE: The system symbol table with runtime functions HAVE TO be called
         last */

void hb_vmSymbolInit_RT( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSymbolInit_RT()" ) );

   hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ), "initsymb.c", 0, 0 );
}
