/*
 * Harbour Project source code:
 * Compatibility header file for CA-Cl*pper base definitions
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/* DON'T USE THIS FILE FOR NEW HARBOUR C CODE */

/* This file is provided to support some level of */
/* Harbour compatibility for old Clipper C extension code */

#ifndef _CLIPDEFS_H
#define _CLIPDEFS_H

#include "hbapi.h"

/* New types */

#if ! defined( HB_LEGACY_LEVEL4 ) || ! defined( HB_LEGACY_TYPES_ON )
typedef unsigned char BYTE;
#endif
typedef BYTE *  BYTEP;
typedef BYTEP   BYTEPP;
typedef BYTEP   PBYTE;

#if ! defined( HB_LEGACY_LEVEL4 ) || ! defined( HB_LEGACY_TYPES_ON )
typedef short SHORT;
#endif
typedef SHORT * SHORTP;
typedef SHORTP  PSHORT;

#if ! defined( HB_LEGACY_LEVEL4 ) || ! defined( HB_LEGACY_TYPES_ON )
typedef unsigned short USHORT;
#endif
typedef USHORT * USHORTP;
typedef USHORTP PUSHORT;

typedef unsigned int WORD;
typedef WORD *  WORDP;
typedef WORDP   PWORD;

#if ! defined( HB_LEGACY_LEVEL4 ) || ! defined( HB_LEGACY_TYPES_ON )
typedef long LONG;
#endif
typedef LONG *  LONGP;
typedef LONGP   PLONG;

#if ! defined( HB_LEGACY_LEVEL4 ) || ! defined( HB_LEGACY_TYPES_ON )
typedef unsigned long ULONG;
#endif
typedef ULONG * ULONGP;
typedef ULONGP  PULONG;

typedef unsigned long DWORD;
typedef DWORD * DWORDP;
typedef DWORDP  PDWORD;

#if ! defined( HB_LEGACY_LEVEL4 ) || ! defined( HB_LEGACY_TYPES_ON )
#undef BOOL
typedef USHORT BOOL;
#endif
typedef BOOL *  BOOLP;
typedef BOOLP   PBOOL;

typedef void *  NEARP;
typedef NEARP * NEARPP;

typedef void *      FARP;
typedef FARP *      FARPP;
typedef FARP        VOIDP;
typedef FARP        PVOID;
typedef HB_VMHANDLE HANDLE;

#define ERRCODE HB_ERRCODE

typedef ERRCODE IHELP;
typedef ERRCODE ICODE;

/* default func ptr -- USHORT return, USHORT param */
typedef USHORT  ( * FUNCP )( USHORT param, ...);
typedef FUNCP * FUNCPP;

#define HIDE    static
#define CLIPPER HARBOUR

#if ! defined( HB_LEGACY_LEVEL4 ) || ! defined( HB_LEGACY_TYPES_ON )
#  undef FALSE
#  define FALSE      0
#  undef TRUE
#  define TRUE       1
#endif
#ifndef NIL
#  define NIL     '\0'
#endif
#ifndef NULL
#  define NULL    0
#endif

/* Old types */

typedef BYTE    byte;
typedef USHORT  quant;
typedef BOOL    Boolean;

#endif /* _CLIPDEFS_H */
