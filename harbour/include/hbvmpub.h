/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the generated C language source code
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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

#ifndef HB_VMPUB_H_
#define HB_VMPUB_H_

#include "hbdefs.h"

struct _HB_DYNS;

/* symbol support structure */
typedef struct
{
   char *      szName;  /* the name of the symbol */
   HB_SYMBOLSCOPE cScope;  /* the scope of the symbol */
   PHB_FUNC    pFunPtr; /* function address for function symbol table entries */
   struct _HB_DYNS    *pDynSym; /* pointer to its dynamic symbol if defined */
} HB_SYMB, * PHB_SYMB;

/* dynamic symbol structure */
typedef struct _HB_DYNS
{
   HB_HANDLE hArea;       /* Workarea number */
   HB_HANDLE hMemvar;     /* Index number into memvars ( publics & privates ) array */
   PHB_SYMB  pSymbol;     /* pointer to its relative local symbol */
   PHB_FUNC  pFunPtr;     /* Pointer to the function address */
} HB_DYNS, * PHB_DYNS, * HB_DYNS_PTR;

#define HB_DYNS_FUNC( hbfunc )   BOOL hbfunc( PHB_DYNS pDynSymbol, void * Cargo )
typedef HB_DYNS_FUNC( PHB_DYNS_FUNC );

/* Harbour Functions scope ( HB_SYMBOLSCOPE ) */
/* The underscore prefix is needed to avoid collision with function names
   like FS_INIT() when generating C output. [vszakats] */
#define _HB_FS_PUBLIC   ( ( HB_SYMBOLSCOPE ) 0x00 )
#define _HB_FS_STATIC   ( ( HB_SYMBOLSCOPE ) 0x02 )
#define _HB_FS_INIT     ( ( HB_SYMBOLSCOPE ) 0x08 )
#define _HB_FS_EXIT     ( ( HB_SYMBOLSCOPE ) 0x10 )
#define _HB_FS_INITEXIT ( _HB_FS_INIT | _HB_FS_EXIT )
#define _HB_FS_MESSAGE  ( ( HB_SYMBOLSCOPE ) 0x20 )
#define _HB_FS_MEMVAR   ( ( HB_SYMBOLSCOPE ) 0x80 )

extern void hb_vmExecute( BYTE * pCode, PHB_SYMB pSymbols );  /* invokes the virtual machine */

#endif /* HB_VMPUB_H_ */
