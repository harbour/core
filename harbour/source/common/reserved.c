/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Expression Optimizer
 *
 * Copyright 1999 Ryszard Glab
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

/* Table with reserved functions names
 * NOTE: THIS TABLE MUST BE SORTED ALPHABETICALLY
*/
static const char * s_szReservedFun[] = {
   "AADD"      ,
   "ABS"       ,
   "ASC"       ,
   "AT"        ,
   "BOF"       ,
   "BREAK"     ,
   "CDOW"      ,
   "CHR"       ,
   "CMONTH"    ,
   "COL"       ,
   "CTOD"      ,
   "DATE"      ,
   "DAY"       ,
   "DELETED"   ,
   "DEVPOS"    ,
   "DOW"       ,
   "DTOC"      ,
   "DTOS"      ,
   "EMPTY"     ,
   "EOF"       ,
   "EXP"       ,
   "FCOUNT"    ,
   "FIELDNAME" ,
   "FLOCK"     ,
   "FOUND"     ,
   "INKEY"     ,
   "INT"       ,
   "LASTREC"   ,
   "LEFT"      ,
   "LEN"       ,
   "LOCK"      ,
   "LOG"       ,
   "LOWER"     ,
   "LTRIM"     ,
   "MAX"       ,
   "MIN"       ,
   "MONTH"     ,
   "PCOL"      ,
   "PCOUNT"    ,
   "PROW"      ,
   "QSELF"     ,
   "RECCOUNT"  ,
   "RECNO"     ,
   "REPLICATE" ,
   "RLOCK"     ,
   "ROUND"     ,
   "ROW"       ,
   "RTRIM"     ,
   "SECONDS"   ,
   "SELECT"    ,
   "SETPOS"    ,
   "SETPOSBS"  ,
   "SPACE"     ,
   "SQRT"      ,
   "STR"       ,
   "SUBSTR"    ,
   "TIME"      ,
   "TRANSFORM" ,
   "TRIM"      ,
   "TYPE"      ,
   "UPPER"     ,
   "VAL"       ,
   "WORD"      ,
   "YEAR"
};

#define RESERVED_FUNCTIONS  sizeof( s_szReservedFun ) / sizeof( char * )

char * hb_compReservedName( char * szName )
{
   unsigned int wNum = 0;
   int iFound = 1;

   while( wNum < RESERVED_FUNCTIONS && iFound )
   {
      /* Compare first 4 characters
      * If they are the same then compare the whole name
      * SECO() is not allowed because of Clipper function SECONDS()
      * however SECO32() is a valid name.
      */
      iFound = strncmp( szName, s_szReservedFun[ wNum ], 4 );
      if( iFound == 0 )
         iFound = strncmp( szName, s_szReservedFun[ wNum ], strlen( szName ) );
      ++wNum;
   }

   return iFound == 0 ? ( char * ) s_szReservedFun[ wNum - 1 ] : NULL;
}

