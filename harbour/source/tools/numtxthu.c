/*
 * Harbour Compiler, Build 30b (1999.10.16)
 * Generated C source code
 */

#include "hb_vmpub.h"
#include "init.h"


HARBOUR HB_NUMTOTXTHU( void );
static HARBOUR HB_NUMTOTXTRAW( void );
extern HARBOUR HB_INT( void );
extern HARBOUR HB_LEN( void );
extern HARBOUR HB_SUBSTR( void );
extern HARBOUR HB_STR( void );
extern HARBOUR HB_STRZERO( void );
extern HARBOUR HB_VAL( void );
extern HARBOUR HB_EMPTY( void );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_NUMTXTHU )
{ "NUMTOTXTHU", FS_PUBLIC, HB_NUMTOTXTHU, 0 },
{ "NUMTOTXTRAW", FS_STATIC, HB_NUMTOTXTRAW, 0 },
{ "INT", FS_PUBLIC, HB_INT, 0 },
{ "LEN", FS_PUBLIC, HB_LEN, 0 },
{ "SUBSTR", FS_PUBLIC, HB_SUBSTR, 0 },
{ "STR", FS_PUBLIC, HB_STR, 0 },
{ "STRZERO", FS_PUBLIC, HB_STRZERO, 0 },
{ "VAL", FS_PUBLIC, HB_VAL, 0 },
{ "EMPTY", FS_PUBLIC, HB_EMPTY, 0 }
HB_INIT_SYMBOLS_END( hb_vm_SymbolInit_NUMTXTHU )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_vm_SymbolInit_NUMTXTHU
#endif


HARBOUR HB_NUMTOTXTHU( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 5, 1,	/* locals, params */
/* 00006 */ HB_P_LINE, 37, 0,	/* 37 */
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	't', 'i', 'z', 'e', 'd', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	's', 'z', 160, 'z', 'a', 'd', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'e', 'z', 'r', 'e', 'd', 
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	't', 161, 'z', 'e', 'z', 'r', 'e', 'd', 
	HB_P_PUSHSTR, 9, 0,	/* 9 */
	's', 'z', 160, 'z', 'e', 'z', 'r', 'e', 'd', 
	HB_P_PUSHSTR, 9, 0,	/* 9 */
	'm', 'i', 'l', 'l', 'i', 'o', 'm', 'o', 'd', 
	HB_P_ARRAYGEN, 6, 0,	/* 6 */
	HB_P_POPLOCAL, 2, 0,	/* ATORT */
/* 00075 */ HB_P_LINE, 41, 0,	/* 41 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_ZERO,
	HB_P_EXACTLYEQUAL,
	HB_P_JUMPFALSE, 19, 0,	/* 19 (abs: 00102) */
/* 00086 */ HB_P_LINE, 42, 0,	/* 42 */
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'n', 'u', 'l', 'l', 'a', 
	HB_P_RETVALUE,
	HB_P_ENDPROC,
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00102) */
/* 00102 */ HB_P_LINE, 45, 0,	/* 45 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_ZERO,
	HB_P_LESS,
	HB_P_JUMPFALSE, 32, 0,	/* 32 (abs: 00142) */
/* 00113 */ HB_P_LINE, 46, 0,	/* 46 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_NEGATE,
	HB_P_POPLOCAL, 1, 0,	/* NVALUE */
/* 00123 */ HB_P_LINE, 47, 0,	/* 47 */
	HB_P_PUSHSTR, 7, 0,	/* 7 */
	'm', 161, 'n', 'u', 's', 'z', ' ', 
	HB_P_POPLOCAL, 3, 0,	/* CRETVAL */
	HB_P_JUMP, 12, 0,	/* 12 (abs: 00151) */
/* 00142 */ HB_P_LINE, 49, 0,	/* 49 */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_POPLOCAL, 3, 0,	/* CRETVAL */
/* 00151 */ HB_P_LINE, 52, 0,	/* 52 */
	HB_P_PUSHLOCAL, 3, 0,	/* CRETVAL */
	HB_P_PUSHSYM, 1, 0,	/* NUMTOTXTRAW */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 2, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_FUNCTION, 1, 0,
	HB_P_DUPLICATE,
	HB_P_POPLOCAL, 4, 0,	/* TMP */
	HB_P_FUNCTION, 1, 0,
	HB_P_PLUS,
	HB_P_POPLOCAL, 3, 0,	/* CRETVAL */
/* 00182 */ HB_P_LINE, 54, 0,	/* 54 */
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLOCAL, 4, 0,	/* TMP */
	HB_P_MINUS,
	HB_P_DUPLICATE,
	HB_P_POPLOCAL, 4, 0,	/* TMP */
	HB_P_ZERO,
	HB_P_GREATER,
	HB_P_DUPLICATE,
	HB_P_JUMPFALSE, 13, 0,	/* 13 (abs: 00212) */
	HB_P_PUSHLOCAL, 4, 0,	/* TMP */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_LESS,
	HB_P_AND,
	HB_P_JUMPFALSE, 207, 0,	/* 207 (abs: 00419) */
/* 00215 */ HB_P_LINE, 55, 0,	/* 55 */
	HB_P_PUSHSYM, 3, 0,	/* LEN */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 4, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 5, 0,	/* STR */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 4, 0,	/* TMP */
	HB_P_PUSHLONG, 8, 0, 0, 0,	/* 8 */
	HB_P_PUSHLONG, 6, 0, 0, 0,	/* 6 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHLONG, 3, 0, 0, 0,	/* 3 */
	HB_P_FUNCTION, 2, 0,
	HB_P_DUPLICATE,
	HB_P_POPLOCAL, 6, 0,	/* TMP2 */
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 5, 0,	/* TMP1 */
	HB_P_PUSHSYM, 4, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 6, 0,	/* TMP2 */
	HB_P_PUSHLOCAL, 5, 0,	/* TMP1 */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	'0', 
	HB_P_EXACTLYEQUAL,
	HB_P_DUPLICATE,
	HB_P_JUMPFALSE, 9, 0,	/* 9 (abs: 00297) */
	HB_P_PUSHLOCAL, 5, 0,	/* TMP1 */
	HB_P_ZERO,
	HB_P_GREATER,
	HB_P_AND,
	HB_P_JUMPFALSE, 16, 0,	/* 16 (abs: 00313) */
/* 00300 */ HB_P_LINE, 59, 0,	/* 59 */
	HB_P_PUSHLOCAL, 5, 0,	/* TMP1 */
	HB_P_DEC,
	HB_P_POPLOCAL, 5, 0,	/* TMP1 */
	HB_P_JUMP, 210, 255,	/* -46 (abs: 00264) */
/* 00313 */ HB_P_LINE, 60, 0,	/* 60 */
	HB_P_PUSHLOCAL, 3, 0,	/* CRETVAL */
	HB_P_PUSHSTR, 7, 0,	/* 7 */
	' ', 'e', 'g', 130, 's', 'z', ' ', 
	HB_P_PUSHSYM, 1, 0,	/* NUMTOTXTRAW */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 4, 0,	/* TMP */
	HB_P_PUSHLONG, 10, 0, 0, 0,	/* 10 */
	HB_P_PUSHLOCAL, 5, 0,	/* TMP1 */
	HB_P_POWER,
	HB_P_MULT,
	HB_P_FUNCTION, 1, 0,
	HB_P_PLUS,
	HB_P_PLUS,
	HB_P_POPLOCAL, 3, 0,	/* CRETVAL */
/* 00354 */ HB_P_LINE, 63, 0,	/* 63 */
	HB_P_PUSHLOCAL, 5, 0,	/* TMP1 */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_GREATEREQUAL,
	HB_P_DUPLICATE,
	HB_P_JUMPFALSE, 18, 0,	/* 18 (abs: 00385) */
	HB_P_PUSHLOCAL, 5, 0,	/* TMP1 */
	HB_P_PUSHSYM, 3, 0,	/* LEN */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 2, 0,	/* ATORT */
	HB_P_FUNCTION, 1, 0,
	HB_P_LESSEQUAL,
	HB_P_AND,
	HB_P_JUMPFALSE, 28, 0,	/* 28 (abs: 00413) */
/* 00388 */ HB_P_LINE, 64, 0,	/* 64 */
	HB_P_PUSHLOCAL, 3, 0,	/* CRETVAL */
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	' ', 
	HB_P_PUSHLOCAL, 2, 0,	/* ATORT */
	HB_P_PUSHLOCAL, 5, 0,	/* TMP1 */
	HB_P_ARRAYPUSH,
	HB_P_PLUS,
	HB_P_PLUS,
	HB_P_POPLOCAL, 3, 0,	/* CRETVAL */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00413) */
/* 00413 */ HB_P_LINE, 65, 0,	/* 65 */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00419) */
/* 00419 */ HB_P_LINE, 68, 0,	/* 68 */
	HB_P_PUSHLOCAL, 3, 0,	/* CRETVAL */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00427 */
   };

   hb_vmExecute( pcode, symbols );
}

static HARBOUR HB_NUMTOTXTRAW( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 6, 1,	/* locals, params */
	HB_P_PUSHLONG, 18, 0, 0, 0,	/* 18 */
	HB_P_ARRAYDIM, 1, 0,	/* 1 */
	HB_P_POPLOCAL, 2, 0,	/* ADIGIT */
/* 00017 */ HB_P_LINE, 74, 0,	/* 74 */
	HB_P_PUSHSYM, 6, 0,	/* STRZERO */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NVALUE */
	HB_P_PUSHLONG, 18, 0, 0, 0,	/* 18 */
	HB_P_FUNCTION, 2, 0,
	HB_P_POPLOCAL, 3, 0,	/* CVALUE */
/* 00038 */ HB_P_LINE, 75, 0,	/* 75 */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_PUSHSTR, 4, 0,	/* 4 */
	'e', 'z', 'e', 'r', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'm', 'i', 'l', 'l', 'i', 162, 
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'm', 'i', 'l', 'l', 'i', 160, 'r', 'd', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'b', 'i', 'l', 'l', 'i', 162, 
	HB_P_PUSHSTR, 10, 0,	/* 10 */
	'e', 'z', 'e', 'r', 'b', 'i', 'l', 'l', 'i', 162, 
	HB_P_ARRAYGEN, 6, 0,	/* 6 */
	HB_P_POPLOCAL, 4, 0,	/* AEGESZ */
/* 00099 */ HB_P_LINE, 77, 0,	/* 77 */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	'e', 'g', 'y', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'k', 'e', 't', 't', 139, 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'h', 160, 'r', 'o', 'm', 
	HB_P_PUSHSTR, 4, 0,	/* 4 */
	'n', 130, 'g', 'y', 
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	148, 't', 
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	'h', 'a', 't', 
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	'h', 130, 't', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'n', 'y', 'o', 'l', 'c', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'k', 'i', 'l', 'e', 'n', 'c', 
	HB_P_ARRAYGEN, 10, 0,	/* 10 */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	'e', 'g', 'y', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'k', 'e', 't', 't', 139, 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'h', 160, 'r', 'o', 'm', 
	HB_P_PUSHSTR, 4, 0,	/* 4 */
	'n', 130, 'g', 'y', 
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	148, 't', 
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	'h', 'a', 't', 
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	'h', 130, 't', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'n', 'y', 'o', 'l', 'c', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'k', 'i', 'l', 'e', 'n', 'c', 
	HB_P_ARRAYGEN, 10, 0,	/* 10 */
	HB_P_ARRAYGEN, 2, 0,	/* 2 */
	HB_P_POPLOCAL, 5, 0,	/* AEGYES */
/* 00246 */ HB_P_LINE, 79, 0,	/* 79 */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	't', 161, 'z', 
	HB_P_PUSHSTR, 4, 0,	/* 4 */
	'h', 163, 's', 'z', 
	HB_P_PUSHSTR, 7, 0,	/* 7 */
	'h', 'a', 'r', 'm', 'i', 'n', 'c', 
	HB_P_PUSHSTR, 7, 0,	/* 7 */
	'n', 'e', 'g', 'y', 'v', 'e', 'n', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	148, 't', 'v', 'e', 'n', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'h', 'a', 't', 'v', 'a', 'n', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'h', 'e', 't', 'v', 'e', 'n', 
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'n', 'y', 'o', 'l', 'c', 'v', 'a', 'n', 
	HB_P_PUSHSTR, 9, 0,	/* 9 */
	'k', 'i', 'l', 'e', 'n', 'c', 'v', 'e', 'n', 
	HB_P_ARRAYGEN, 10, 0,	/* 10 */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	't', 'i', 'z', 'e', 'n', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'h', 'u', 's', 'z', 'o', 'n', 
	HB_P_PUSHSTR, 7, 0,	/* 7 */
	'h', 'a', 'r', 'm', 'i', 'n', 'c', 
	HB_P_PUSHSTR, 7, 0,	/* 7 */
	'n', 'e', 'g', 'y', 'v', 'e', 'n', 
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	148, 't', 'v', 'e', 'n', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'h', 'a', 't', 'v', 'a', 'n', 
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	'h', 'e', 't', 'v', 'e', 'n', 
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'n', 'y', 'o', 'l', 'c', 'v', 'a', 'n', 
	HB_P_PUSHSTR, 9, 0,	/* 9 */
	'k', 'i', 'l', 'e', 'n', 'c', 'v', 'e', 'n', 
	HB_P_ARRAYGEN, 10, 0,	/* 10 */
	HB_P_ARRAYGEN, 2, 0,	/* 2 */
	HB_P_POPLOCAL, 6, 0,	/* ATIZES */
/* 00435 */ HB_P_LINE, 82, 0,	/* 82 */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_POPLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLONG, 18, 0, 0, 0,	/* 18 */
	HB_P_LESSEQUAL,
	HB_P_JUMPFALSE, 60, 0,	/* 60 (abs: 00515) */
/* 00458 */ HB_P_LINE, 83, 0,	/* 83 */
	HB_P_PUSHSYM, 7, 0,	/* VAL */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 4, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 3, 0,	/* CVALUE */
	HB_P_PUSHLONG, 18, 0, 0, 0,	/* 18 */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_MINUS,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_PLUS,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_FUNCTION, 1, 0,
	HB_P_PUSHLOCAL, 2, 0,	/* ADIGIT */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_ARRAYPOP,
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_INC,
	HB_P_POPLOCAL, 7, 0,	/* TMP */
	HB_P_JUMP, 190, 255,	/* -66 (abs: 00446) */
/* 00515 */ HB_P_LINE, 86, 0,	/* 86 */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_POPLOCAL, 3, 0,	/* CVALUE */
/* 00524 */ HB_P_LINE, 87, 0,	/* 87 */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_POPLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLONG, 16, 0, 0, 0,	/* 16 */
	HB_P_PUSHLONG, 3, 0, 0, 0,	/* 3 */
	HB_P_FORTEST,
	HB_P_JUMPFALSE, 59, 1,	/* 315 (abs: 00864) */
/* 00552 */ HB_P_LINE, 88, 0,	/* 88 */
	HB_P_PUSHLOCAL, 2, 0,	/* ADIGIT */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_ARRAYPUSH,
	HB_P_ZERO,
	HB_P_NOTEQUAL,
	HB_P_DUPLICATE,
	HB_P_JUMPTRUE, 19, 0,	/* 19 (abs: 00584) */
	HB_P_PUSHLOCAL, 2, 0,	/* ADIGIT */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_PLUS,
	HB_P_ARRAYPUSH,
	HB_P_ZERO,
	HB_P_NOTEQUAL,
	HB_P_OR,
	HB_P_DUPLICATE,
	HB_P_JUMPTRUE, 19, 0,	/* 19 (abs: 00604) */
	HB_P_PUSHLOCAL, 2, 0,	/* ADIGIT */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLONG, 2, 0, 0, 0,	/* 2 */
	HB_P_PLUS,
	HB_P_ARRAYPUSH,
	HB_P_ZERO,
	HB_P_NOTEQUAL,
	HB_P_OR,
	HB_P_JUMPFALSE, 242, 0,	/* 242 (abs: 00846) */
/* 00607 */ HB_P_LINE, 89, 0,	/* 89 */
	HB_P_PUSHLOCAL, 5, 0,	/* AEGYES */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_EXACTLYEQUAL,
	HB_P_JUMPFALSE, 11, 0,	/* 11 (abs: 00633) */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_JUMP, 8, 0,	/* 8 (abs: 00638) */
	HB_P_PUSHLONG, 2, 0, 0, 0,	/* 2 */
	HB_P_ARRAYPUSH,
	HB_P_PUSHLOCAL, 2, 0,	/* ADIGIT */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_ARRAYPUSH,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_PLUS,
	HB_P_ARRAYPUSH,
	HB_P_PUSHLOCAL, 4, 0,	/* AEGESZ */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_MINUS,
	HB_P_PUSHLONG, 3, 0, 0, 0,	/* 3 */
	HB_P_DIVIDE,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_PLUS,
	HB_P_ARRAYPUSH,
	HB_P_PLUS,
	HB_P_PUSHSYM, 8, 0,	/* EMPTY */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 3, 0,	/* CVALUE */
	HB_P_FUNCTION, 1, 0,
	HB_P_JUMPFALSE, 9, 0,	/* 9 (abs: 00698) */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_JUMP, 7, 0,	/* 7 (abs: 00702) */
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	'-', 
	HB_P_PLUS,
	HB_P_PUSHLOCAL, 3, 0,	/* CVALUE */
	HB_P_PLUS,
	HB_P_POPLOCAL, 3, 0,	/* CVALUE */
/* 00710 */ HB_P_LINE, 90, 0,	/* 90 */
	HB_P_PUSHLOCAL, 6, 0,	/* ATIZES */
	HB_P_PUSHLOCAL, 2, 0,	/* ADIGIT */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_ARRAYPUSH,
	HB_P_ZERO,
	HB_P_EXACTLYEQUAL,
	HB_P_JUMPFALSE, 11, 0,	/* 11 (abs: 00736) */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_JUMP, 8, 0,	/* 8 (abs: 00741) */
	HB_P_PUSHLONG, 2, 0, 0, 0,	/* 2 */
	HB_P_ARRAYPUSH,
	HB_P_PUSHLOCAL, 2, 0,	/* ADIGIT */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_PLUS,
	HB_P_ARRAYPUSH,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_PLUS,
	HB_P_ARRAYPUSH,
	HB_P_PUSHLOCAL, 3, 0,	/* CVALUE */
	HB_P_PLUS,
	HB_P_POPLOCAL, 3, 0,	/* CVALUE */
/* 00769 */ HB_P_LINE, 91, 0,	/* 91 */
	HB_P_PUSHLOCAL, 2, 0,	/* ADIGIT */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLONG, 2, 0, 0, 0,	/* 2 */
	HB_P_PLUS,
	HB_P_ARRAYPUSH,
	HB_P_ZERO,
	HB_P_NOTEQUAL,
	HB_P_JUMPFALSE, 53, 0,	/* 53 (abs: 00840) */
/* 00790 */ HB_P_LINE, 92, 0,	/* 92 */
	HB_P_PUSHLOCAL, 5, 0,	/* AEGYES */
	HB_P_PUSHLONG, 2, 0, 0, 0,	/* 2 */
	HB_P_ARRAYPUSH,
	HB_P_PUSHLOCAL, 2, 0,	/* ADIGIT */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLONG, 2, 0, 0, 0,	/* 2 */
	HB_P_PLUS,
	HB_P_ARRAYPUSH,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_PLUS,
	HB_P_ARRAYPUSH,
	HB_P_PUSHSTR, 4, 0,	/* 4 */
	's', 'z', 160, 'z', 
	HB_P_PLUS,
	HB_P_PUSHLOCAL, 3, 0,	/* CVALUE */
	HB_P_PLUS,
	HB_P_POPLOCAL, 3, 0,	/* CVALUE */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00840) */
/* 00840 */ HB_P_LINE, 93, 0,	/* 93 */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00846) */
/* 00846 */ HB_P_LINE, 94, 0,	/* 94 */
	HB_P_PUSHLOCAL, 7, 0,	/* TMP */
	HB_P_PUSHLONG, 3, 0, 0, 0,	/* 3 */
	HB_P_PLUS,
	HB_P_POPLOCAL, 7, 0,	/* TMP */
	HB_P_JUMP, 186, 254,	/* -326 (abs: 00535) */
/* 00864 */ HB_P_LINE, 97, 0,	/* 97 */
	HB_P_PUSHLOCAL, 3, 0,	/* CVALUE */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00872 */
   };

   hb_vmExecute( pcode, symbols );
}

