/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compatibility header file for CA-Clipper base definitions
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
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

/* DON'T USE THIS FILE FOR NEW HARBOUR C CODE */

/* This file is provided to support some level of */
/* Harbour compatibility for old Clipper C extension code */

#ifndef _CLIPDEFS_H
#define _CLIPDEFS_H
        
#include "extend.h"

/* Old types */

typedef BYTE    byte;
typedef USHORT  quant;
typedef BOOL    Boolean;

/* New types */

typedef BYTE *  BYTEP;
typedef BYTEP   PBYTE;
typedef BYTEP   BYTEPP;

typedef SHORT * SHORTP;
typedef SHORTP  PSHORT;

typedef USHORT * USHORTP;
typedef USHORTP PUSHORT;

typedef unsigned int WORD;
typedef WORD *  WORDP;
typedef WORDP   PWORD;

typedef LONG *  LONGP;
typedef LONGP   PLONG;

typedef ULONG * ULONGP;
typedef ULONGP  PULONG;

typedef unsigned long DWORD;
typedef DWORD * DWORDP;
typedef DWORDP  PDWORD;

typedef BOOL *  BOOLP;
typedef BOOLP   PBOOL;

typedef void *  NEARP;
typedef NEARP * NEARPP;

typedef void *  FARP;
typedef FARP *  FARPP;

typedef FARP    VOIDP;
typedef FARP    PVOID;

typedef void *  HANDLE;
typedef ERRCODE IHELP;
typedef ERRCODE ICODE;

/* default func ptr -- USHORT return, USHORT param */
typedef USHORT  ( * FUNCP )( USHORT param, ...);
typedef FUNCP * FUNCPP;

#define HIDE    static
#define CLIPPER HARBOUR

#ifndef NIL
   #define NIL     '\0'
#endif
#ifndef NULL
   #define NULL    0
#endif

#endif /* _CLIPDEFS_H */
