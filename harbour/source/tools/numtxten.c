/*
 * Harbour Compiler, Build 30b (1999.10.16)
 * Generated C source code
 */

#include "hb_vmpub.h"
#include "init.h"


HARBOUR HB_NUMTOTXTEN( void );
static HARBOUR HB_INT_TO_STRING( void );
extern HARBOUR HB_INT( void );
extern HARBOUR HB_RTRIM( void );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_NUMTXTEN )
{ "NUMTOTXTEN", FS_PUBLIC, HB_NUMTOTXTEN, 0 },
{ "INT_TO_STRING", FS_STATIC, HB_INT_TO_STRING, 0 },
{ "INT", FS_PUBLIC, HB_INT, 0 },
{ "RTRIM", FS_PUBLIC, HB_RTRIM, 0 }
HB_INIT_SYMBOLS_END( hb_vm_SymbolInit_NUMTXTEN )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_vm_SymbolInit_NUMTXTEN
#endif


HARBOUR HB_NUMTOTXTEN( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 1, 1,	/* locals, params */
/* 00006 */ HB_P_LINE, 11, 0,	/* 11 */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
/* 00015 */ HB_P_LINE, 13, 0,	/* 13 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_ZERO,
	HB_P_EXACTLYEQUAL,
	HB_P_JUMPFALSE, 18, 0,	/* 18 (abs: 00041) */
/* 00026 */ HB_P_LINE, 14, 0,	/* 14 */
	HB_P_PUSHSTR, 4, 0,	/* 4 */
	'z', 'e', 'r', 'o', 
	HB_P_RETVALUE,
	HB_P_ENDPROC,
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00041) */
/* 00041 */ HB_P_LINE, 17, 0,	/* 17 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_ZERO,
	HB_P_LESS,
	HB_P_JUMPFALSE, 35, 0,	/* 35 (abs: 00084) */
/* 00052 */ HB_P_LINE, 18, 0,	/* 18 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_NEGATE,
	HB_P_POPLOCAL, 1, 0,	/* NVALUE */
/* 00062 */ HB_P_LINE, 19, 0,	/* 19 */
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'm', 'i', 'n', 'u', 's', ' ', 
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00084) */
/* 00084 */ HB_P_LINE, 22, 0,	/* 22 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 64, 66, 15, 0,	/* 1000000 */
	HB_P_GREATEREQUAL,
	HB_P_JUMPFALSE, 211, 0,	/* 211 (abs: 00307) */
/* 00099 */ HB_P_LINE, 23, 0,	/* 23 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 0, 225, 245, 5,	/* 100000000 */
	HB_P_GREATEREQUAL,
	HB_P_JUMPFALSE, 84, 0,	/* 84 (abs: 00195) */
/* 00114 */ HB_P_LINE, 24, 0,	/* 24 */
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_PUSHSYM, 1, 0,	/* INT_TO_STRING */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 0, 225, 245, 5,	/* 100000000 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_FUNCTION, 1, 0,
	HB_P_PUSHSTR, 9, 0,	/* 9 */
	' ', 'h', 'u', 'n', 'd', 'r', 'e', 'd', ' ', 
	HB_P_PLUS,
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
/* 00160 */ HB_P_LINE, 25, 0,	/* 25 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 0, 225, 245, 5,	/* 100000000 */
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 0, 225, 245, 5,	/* 100000000 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_MULT,
	HB_P_MINUS,
	HB_P_POPLOCAL, 1, 0,	/* NVALUE */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00195) */
/* 00195 */ HB_P_LINE, 27, 0,	/* 27 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 64, 66, 15, 0,	/* 1000000 */
	HB_P_GREATEREQUAL,
	HB_P_JUMPFALSE, 76, 0,	/* 76 (abs: 00283) */
/* 00210 */ HB_P_LINE, 28, 0,	/* 28 */
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_PUSHSYM, 1, 0,	/* INT_TO_STRING */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 64, 66, 15, 0,	/* 1000000 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_FUNCTION, 1, 0,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	' ', 
	HB_P_PLUS,
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
/* 00248 */ HB_P_LINE, 29, 0,	/* 29 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 64, 66, 15, 0,	/* 1000000 */
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 64, 66, 15, 0,	/* 1000000 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_MULT,
	HB_P_MINUS,
	HB_P_POPLOCAL, 1, 0,	/* NVALUE */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00283) */
/* 00283 */ HB_P_LINE, 31, 0,	/* 31 */
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'm', 'i', 'l', 'l', 'i', 'o', 'n', ' ', 
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00307) */
/* 00307 */ HB_P_LINE, 33, 0,	/* 33 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 232, 3, 0, 0,	/* 1000 */
	HB_P_GREATEREQUAL,
	HB_P_JUMPFALSE, 212, 0,	/* 212 (abs: 00531) */
/* 00322 */ HB_P_LINE, 34, 0,	/* 34 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 160, 134, 1, 0,	/* 100000 */
	HB_P_GREATEREQUAL,
	HB_P_JUMPFALSE, 84, 0,	/* 84 (abs: 00418) */
/* 00337 */ HB_P_LINE, 35, 0,	/* 35 */
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_PUSHSYM, 1, 0,	/* INT_TO_STRING */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 160, 134, 1, 0,	/* 100000 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_FUNCTION, 1, 0,
	HB_P_PUSHSTR, 9, 0,	/* 9 */
	' ', 'h', 'u', 'n', 'd', 'r', 'e', 'd', ' ', 
	HB_P_PLUS,
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
/* 00383 */ HB_P_LINE, 36, 0,	/* 36 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 160, 134, 1, 0,	/* 100000 */
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 160, 134, 1, 0,	/* 100000 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_MULT,
	HB_P_MINUS,
	HB_P_POPLOCAL, 1, 0,	/* NVALUE */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00418) */
/* 00418 */ HB_P_LINE, 38, 0,	/* 38 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 232, 3, 0, 0,	/* 1000 */
	HB_P_GREATEREQUAL,
	HB_P_JUMPFALSE, 76, 0,	/* 76 (abs: 00506) */
/* 00433 */ HB_P_LINE, 39, 0,	/* 39 */
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_PUSHSYM, 1, 0,	/* INT_TO_STRING */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 232, 3, 0, 0,	/* 1000 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_FUNCTION, 1, 0,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	' ', 
	HB_P_PLUS,
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
/* 00471 */ HB_P_LINE, 40, 0,	/* 40 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 232, 3, 0, 0,	/* 1000 */
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 232, 3, 0, 0,	/* 1000 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_MULT,
	HB_P_MINUS,
	HB_P_POPLOCAL, 1, 0,	/* NVALUE */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00506) */
/* 00506 */ HB_P_LINE, 42, 0,	/* 42 */
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_PUSHSTR, 9, 0,	/* 9 */
	't', 'h', 'o', 'u', 's', 'a', 'n', 'd', ' ', 
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00531) */
/* 00531 */ HB_P_LINE, 44, 0,	/* 44 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 100, 0, 0, 0,	/* 100 */
	HB_P_GREATEREQUAL,
	HB_P_JUMPFALSE, 84, 0,	/* 84 (abs: 00627) */
/* 00546 */ HB_P_LINE, 45, 0,	/* 45 */
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_PUSHSYM, 1, 0,	/* INT_TO_STRING */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 100, 0, 0, 0,	/* 100 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_FUNCTION, 1, 0,
	HB_P_PUSHSTR, 9, 0,	/* 9 */
	' ', 'h', 'u', 'n', 'd', 'r', 'e', 'd', ' ', 
	HB_P_PLUS,
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
/* 00592 */ HB_P_LINE, 46, 0,	/* 46 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 100, 0, 0, 0,	/* 100 */
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 100, 0, 0, 0,	/* 100 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_MULT,
	HB_P_MINUS,
	HB_P_POPLOCAL, 1, 0,	/* NVALUE */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00627) */
/* 00627 */ HB_P_LINE, 48, 0,	/* 48 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_GREATEREQUAL,
	HB_P_JUMPFALSE, 53, 0,	/* 53 (abs: 00692) */
/* 00642 */ HB_P_LINE, 49, 0,	/* 49 */
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_PUSHSYM, 1, 0,	/* INT_TO_STRING */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_FUNCTION, 1, 0,
	HB_P_FUNCTION, 1, 0,
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
/* 00669 */ HB_P_LINE, 50, 0,	/* 50 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_FUNCTION, 1, 0,
	HB_P_MINUS,
	HB_P_POPLOCAL, 1, 0,	/* NVALUE */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00692) */
/* 00692 */ HB_P_LINE, 53, 0,	/* 53 */
	HB_P_PUSHSYM, 3, 0,	/* RTRIM */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_FUNCTION, 1, 0,
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00707 */
   };

   hb_vmExecute( pcode, symbols );
}

static HARBOUR HB_INT_TO_STRING( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 3, 1,	/* locals, params */
/* 00006 */ HB_P_LINE, 76, 0,	/* 76 */
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	'o', 'n', 'e', 
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	't', 'w', 'o', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	't', 'h', 'r', 'e', 'e', 
	HB_P_PUSHSTR, 4, 0,	/* 4 */
	'f', 'o', 'u', 'r', 
	HB_P_PUSHSTR, 4, 0,	/* 4 */
	'f', 'i', 'v', 'e', 
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	's', 'i', 'x', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	's', 'e', 'v', 'e', 'n', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'e', 'i', 'g', 'h', 't', 
	HB_P_PUSHSTR, 4, 0,	/* 4 */
	'n', 'i', 'n', 'e', 
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	't', 'e', 'n', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'e', 'l', 'e', 'v', 'e', 'n', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	't', 'w', 'e', 'l', 'v', 'e', 
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	't', 'h', 'i', 'r', 't', 'e', 'e', 'n', 
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'f', 'o', 'u', 'r', 't', 'e', 'e', 'n', 
	HB_P_PUSHSTR, 7, 0,	/* 7 */
	'f', 'i', 'f', 't', 'e', 'e', 'n', 
	HB_P_PUSHSTR, 7, 0,	/* 7 */
	's', 'i', 'x', 't', 'e', 'e', 'n', 
	HB_P_PUSHSTR, 9, 0,	/* 9 */
	's', 'e', 'v', 'e', 'n', 't', 'e', 'e', 'n', 
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'e', 'i', 'g', 'h', 't', 'e', 'e', 'n', 
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'n', 'i', 'n', 'e', 't', 'e', 'e', 'n', 
	HB_P_ARRAYGEN, 19, 0,	/* 19 */
	HB_P_POPLOCAL, 3, 0,	/* AARRAY1 */
/* 00178 */ HB_P_LINE, 87, 0,	/* 87 */
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	't', 'e', 'n', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	't', 'w', 'e', 'n', 't', 'y', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	't', 'h', 'i', 'r', 't', 'y', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'f', 'o', 'r', 't', 'y', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'f', 'i', 'f', 't', 'y', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	's', 'i', 'x', 't', 'y', 
	HB_P_PUSHSTR, 7, 0,	/* 7 */
	's', 'e', 'v', 'e', 'n', 't', 'y', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'e', 'i', 'g', 'h', 't', 'y', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'n', 'i', 'n', 'e', 't', 'y', 
	HB_P_ARRAYGEN, 9, 0,	/* 9 */
	HB_P_POPLOCAL, 4, 0,	/* AARRAY2 */
/* 00263 */ HB_P_LINE, 89, 0,	/* 89 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 20, 0, 0, 0,	/* 20 */
	HB_P_LESS,
	HB_P_JUMPFALSE, 19, 0,	/* 19 (abs: 00294) */
/* 00278 */ HB_P_LINE, 90, 0,	/* 90 */
	HB_P_PUSHLOCAL, 3, 0,	/* AARRAY1 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_ARRAYPUSH,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
	HB_P_JUMP, 118, 0,	/* 118 (abs: 00409) */
/* 00294 */ HB_P_LINE, 92, 0,	/* 92 */
	HB_P_PUSHLOCAL, 4, 0,	/* AARRAY2 */
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 10, 0, 0, 0,	/* 10 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_ARRAYPUSH,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
/* 00320 */ HB_P_LINE, 93, 0,	/* 93 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 10, 0, 0, 0,	/* 10 */
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 10, 0, 0, 0,	/* 10 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_MULT,
	HB_P_MINUS,
	HB_P_POPLOCAL, 1, 0,	/* NVALUE */
/* 00352 */ HB_P_LINE, 94, 0,	/* 94 */
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_FUNCTION, 1, 0,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_GREATEREQUAL,
	HB_P_JUMPFALSE, 35, 0,	/* 35 (abs: 00406) */
/* 00374 */ HB_P_LINE, 95, 0,	/* 95 */
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	' ', 
	HB_P_PUSHLOCAL, 3, 0,	/* AARRAY1 */
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_FUNCTION, 1, 0,
	HB_P_ARRAYPUSH,
	HB_P_PLUS,
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CRETVAL */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00406) */
/* 00406 */ HB_P_LINE, 99, 0,	/* 99 */
	HB_P_PUSHLOCAL, 2, 0,	/* CRETVAL */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00414 */
   };

   hb_vmExecute( pcode, symbols );
}

