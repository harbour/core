/*
 * $Id$
 */

/*
   Harbour Project source code

   Language API

   Copyright (C) 1999  Victor Szel <info@szelvesz.hu>
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

#include "extend.h"
#include "langapi.h"

static HB_LANG langEN = 
{
   "English",			/* Name */
   "EN",			/* RFC ID */
   "437",  		        /* Codepage */

   /* Texts */

   {
      "YYYY/MM/DD",
      "Y",
      "N",
   },

   /* Month names */

   {
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
   },

   /* Day names */

   {
      "Sunday",
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday"
   },

   /* Error description names */

   {
      "Unknown error",
      "Argument error",
      "Bound error",
      "String overflow",
      "Numeric overflow",
      "Divide by zero",
      "Numeric error",
      "Syntax error",
      "Operation too complex",
      "",
      "",
      "Memory low",
      "Undefined function",
      "No exported method",
      "Variable does not exists",
      "Alias does not exists",
      "No exported variable",
      "Incorrect alias name",
      "Duplicated alias name",
      "",
      "Create error",
      "Open error",
      "Close error",
      "Read error",
      "Write error",
      "Print error",
      "",
      "",
      "",
      "",
      "Unsupported operation",
      "Limit exceeded",
      "Index corruption detected",
      "Incorrect type of data",
      "Data width too long",
      "Workarea not in use",
      "Workarea not indexed",
      "Exclusive use required",
      "Lock required",
      "Write not allowed",
      "Append lock failed",
      "Lock failure",
      "",
      "",
      "",
      "Incorrect number of arguments",
      "array access",
      "array assign",
      "array dimension",
      "not an array",
      "conditional"
   },

   /* Internal error names */

   {
      "Can't locate starting procedure",
      "Can't allocate memory (%s)",
      "Can't reallocate memory (%s)",
      "Free called with null pointer", /* DEBUG */
      "Not implemented opcode (%s)",
      "Not implemented (%s)"
   }
};

static PHB_LANG s_langDef = &langEN;
 
void     hb_langDSet             ( PHB_LANG lang )
{
   if( lang )
      s_langDef = lang;
}

PHB_LANG hb_langDGet             ( void )
{
   return s_langDef;
}

char *   hb_langDGetName         ( void )
{
   return s_langDef->szName;
}

char *   hb_langDGetID           ( void )
{
   return s_langDef->szID;
}

char *   hb_langDGetText         ( ULONG ulIndex )
{
   return s_langDef->szTextList[ ( ulIndex < sizeof( s_langDef->szTextList ) / sizeof( s_langDef->szTextList[ 0 ] ) ) ? ulIndex : 0 ];
}

char *   hb_langDGetDayName      ( ULONG ulIndex )
{
   return s_langDef->szDayNameList[ ( ulIndex < sizeof( s_langDef->szDayNameList ) / sizeof( s_langDef->szDayNameList[ 0 ] ) ) ? ulIndex : 0 ];
}

char *   hb_langDGetMonthName    ( ULONG ulIndex )
{
   return s_langDef->szMonthNameList[ ( ulIndex < sizeof( s_langDef->szMonthNameList ) / sizeof( s_langDef->szMonthNameList[ 0 ] ) ) ? ulIndex : 0 ];
}

char *   hb_langDGetErrorDesc    ( ULONG ulIndex )
{
   return s_langDef->szErrorDescList[ ( ulIndex < sizeof( s_langDef->szErrorDescList ) / sizeof( s_langDef->szErrorDescList[ 0 ] ) ) ? ulIndex : 0 ];
}

char *   hb_langDGetErrorIntr    ( ULONG ulIndex )
{
   return s_langDef->szErrorIntrList[ ( ulIndex < sizeof( s_langDef->szErrorIntrList ) / sizeof( s_langDef->szErrorIntrList[ 0 ] ) ) ? ulIndex : 0 ];
}

