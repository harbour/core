/*
 * $Id$
 */

/* This is the header file which is included to every */
/* Harbour generated C source file */

/*
   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
 */

#ifndef HB_VMPUB_H_
#define HB_VMPUB_H_

#include "pcode.h"

/* Dummy definitions */

typedef void * PDYNSYM;

/* Parts copied from hbdefs.h */

typedef unsigned char BYTE;   /* 1 byte unsigned */
typedef unsigned short int WORD;

#ifdef __GNUC__
   #define pascal __attribute__ ((stdcall))
#endif

#ifdef _MSC_VER
   #define HARBOUR void
#else
#ifdef __IBMCPP__
   #define HARBOUR void
#else
   #define HARBOUR void pascal
#endif
#endif
typedef void * PHB_FUNC;

typedef char SYMBOLSCOPE;   /* stores symbol's scope */

/* Parts copied from extend.h */

/* symbol support structure */
typedef struct
{
   char *      szName;  /* the name of the symbol */
   SYMBOLSCOPE cScope;  /* the scope of the symbol */
   PHB_FUNC    pFunPtr; /* function address for function symbol table entries */
   PDYNSYM     pDynSym; /* pointer to its dynamic symbol if defined */
} HB_SYMB, * PHB_SYMB;

extern void VirtualMachine( BYTE * pCode, PHB_SYMB pSymbols );  /* invokes the virtual machine */

/* Harbour Functions scope */
#define FS_PUBLIC       0
#define FS_STATIC       2
#define FS_INIT         8
#define FS_EXIT        16
#define FS_INITEXIT    ( FS_INIT | FS_EXIT )
#define FS_MESSAGE     32
#define FS_MEMVAR     128

/* This should always follow the type declarations */

#include "init.h"

#endif /* HB_VMPUB_H_ */
