/*
 * Harbour Compiler, Build 30b (1999.10.16)
 * Generated C source code
 */

#include "hb_vmpub.h"
#include "init.h"


HARBOUR HB_ISBIN( void );
HARBOUR HB_ISOCTAL( void );
HARBOUR HB_ISDEC( void );
HARBOUR HB_ISHEXA( void );
HARBOUR HB_DECTOBIN( void );
HARBOUR HB_DECTOOCTAL( void );
HARBOUR HB_DECTOHEXA( void );
HARBOUR HB_BINTODEC( void );
HARBOUR HB_OCTALTODEC( void );
HARBOUR HB_HEXATODEC( void );
extern HARBOUR HB_ALLTRIM( void );
extern HARBOUR HB_LEN( void );
extern HARBOUR HB_SUBSTR( void );
extern HARBOUR HB_INT( void );
extern HARBOUR HB_AT( void );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_NCONVERT )
{ "ISBIN", FS_PUBLIC, HB_ISBIN, 0 },
{ "ALLTRIM", FS_PUBLIC, HB_ALLTRIM, 0 },
{ "LEN", FS_PUBLIC, HB_LEN, 0 },
{ "SUBSTR", FS_PUBLIC, HB_SUBSTR, 0 },
{ "ISOCTAL", FS_PUBLIC, HB_ISOCTAL, 0 },
{ "ISDEC", FS_PUBLIC, HB_ISDEC, 0 },
{ "ISHEXA", FS_PUBLIC, HB_ISHEXA, 0 },
{ "DECTOBIN", FS_PUBLIC, HB_DECTOBIN, 0 },
{ "INT", FS_PUBLIC, HB_INT, 0 },
{ "DECTOOCTAL", FS_PUBLIC, HB_DECTOOCTAL, 0 },
{ "DECTOHEXA", FS_PUBLIC, HB_DECTOHEXA, 0 },
{ "BINTODEC", FS_PUBLIC, HB_BINTODEC, 0 },
{ "AT", FS_PUBLIC, HB_AT, 0 },
{ "OCTALTODEC", FS_PUBLIC, HB_OCTALTODEC, 0 },
{ "HEXATODEC", FS_PUBLIC, HB_HEXATODEC, 0 }
HB_INIT_SYMBOLS_END( hb_vm_SymbolInit_NCONVERT )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_vm_SymbolInit_NCONVERT
#endif


HARBOUR HB_ISBIN( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 2, 1,	/* locals, params */
/* 00006 */ HB_P_LINE, 65, 0,	/* 65 */
	HB_P_TRUE,
	HB_P_POPLOCAL, 3, 0,	/* LFLAG */
/* 00013 */ HB_P_LINE, 66, 0,	/* 66 */
	HB_P_PUSHSYM, 1, 0,	/* ALLTRIM */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 1, 0,	/* CSTRING */
/* 00029 */ HB_P_LINE, 67, 0,	/* 67 */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_POPLOCAL, 2, 0,	/* NX */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_PUSHSYM, 2, 0,	/* LEN */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_LESSEQUAL,
	HB_P_JUMPFALSE, 63, 0,	/* 63 (abs: 00117) */
/* 00057 */ HB_P_LINE, 68, 0,	/* 68 */
	HB_P_PUSHSYM, 3, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	'0', '1', 
	HB_P_INSTRING,
	HB_P_NOT,
	HB_P_JUMPFALSE, 19, 0,	/* 19 (abs: 00104) */
/* 00088 */ HB_P_LINE, 69, 0,	/* 69 */
	HB_P_FALSE,
	HB_P_POPLOCAL, 3, 0,	/* LFLAG */
/* 00095 */ HB_P_LINE, 70, 0,	/* 70 */
	HB_P_JUMP, 19, 0,	/* 19 (abs: 00117) */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00104) */
/* 00104 */ HB_P_LINE, 71, 0,	/* 71 */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_INC,
	HB_P_POPLOCAL, 2, 0,	/* NX */
	HB_P_JUMP, 182, 255,	/* -74 (abs: 00040) */
/* 00117 */ HB_P_LINE, 73, 0,	/* 73 */
	HB_P_PUSHLOCAL, 3, 0,	/* LFLAG */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00125 */
   };

   hb_vmExecute( pcode, symbols );
}

HARBOUR HB_ISOCTAL( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 2, 1,	/* locals, params */
	HB_P_TRUE,
	HB_P_POPLOCAL, 3, 0,	/* LFLAG */
/* 00010 */ HB_P_LINE, 106, 0,	/* 106 */
	HB_P_PUSHSYM, 1, 0,	/* ALLTRIM */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 1, 0,	/* CSTRING */
/* 00026 */ HB_P_LINE, 107, 0,	/* 107 */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_POPLOCAL, 2, 0,	/* NX */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_PUSHSYM, 2, 0,	/* LEN */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_LESSEQUAL,
	HB_P_JUMPFALSE, 69, 0,	/* 69 (abs: 00120) */
/* 00054 */ HB_P_LINE, 108, 0,	/* 108 */
	HB_P_PUSHSYM, 3, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'0', '1', '2', '3', '4', '5', '6', '7', 
	HB_P_INSTRING,
	HB_P_NOT,
	HB_P_JUMPFALSE, 19, 0,	/* 19 (abs: 00107) */
/* 00091 */ HB_P_LINE, 109, 0,	/* 109 */
	HB_P_FALSE,
	HB_P_POPLOCAL, 3, 0,	/* LFLAG */
/* 00098 */ HB_P_LINE, 110, 0,	/* 110 */
	HB_P_JUMP, 19, 0,	/* 19 (abs: 00120) */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00107) */
/* 00107 */ HB_P_LINE, 111, 0,	/* 111 */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_INC,
	HB_P_POPLOCAL, 2, 0,	/* NX */
	HB_P_JUMP, 176, 255,	/* -80 (abs: 00037) */
/* 00120 */ HB_P_LINE, 113, 0,	/* 113 */
	HB_P_PUSHLOCAL, 3, 0,	/* LFLAG */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00128 */
   };

   hb_vmExecute( pcode, symbols );
}

HARBOUR HB_ISDEC( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 2, 1,	/* locals, params */
	HB_P_TRUE,
	HB_P_POPLOCAL, 3, 0,	/* LFLAG */
/* 00010 */ HB_P_LINE, 144, 0,	/* 144 */
	HB_P_PUSHSYM, 1, 0,	/* ALLTRIM */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 1, 0,	/* CSTRING */
/* 00026 */ HB_P_LINE, 145, 0,	/* 145 */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_POPLOCAL, 2, 0,	/* NX */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_PUSHSYM, 2, 0,	/* LEN */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_LESSEQUAL,
	HB_P_JUMPFALSE, 71, 0,	/* 71 (abs: 00122) */
/* 00054 */ HB_P_LINE, 146, 0,	/* 146 */
	HB_P_PUSHSYM, 3, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHSTR, 10, 0,	/* 10 */
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 
	HB_P_INSTRING,
	HB_P_NOT,
	HB_P_JUMPFALSE, 19, 0,	/* 19 (abs: 00109) */
/* 00093 */ HB_P_LINE, 147, 0,	/* 147 */
	HB_P_FALSE,
	HB_P_POPLOCAL, 3, 0,	/* LFLAG */
/* 00100 */ HB_P_LINE, 148, 0,	/* 148 */
	HB_P_JUMP, 19, 0,	/* 19 (abs: 00122) */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00109) */
/* 00109 */ HB_P_LINE, 149, 0,	/* 149 */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_INC,
	HB_P_POPLOCAL, 2, 0,	/* NX */
	HB_P_JUMP, 174, 255,	/* -82 (abs: 00037) */
/* 00122 */ HB_P_LINE, 151, 0,	/* 151 */
	HB_P_PUSHLOCAL, 3, 0,	/* LFLAG */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00130 */
   };

   hb_vmExecute( pcode, symbols );
}

HARBOUR HB_ISHEXA( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 2, 1,	/* locals, params */
	HB_P_TRUE,
	HB_P_POPLOCAL, 3, 0,	/* LFLAG */
/* 00010 */ HB_P_LINE, 182, 0,	/* 182 */
	HB_P_PUSHSYM, 1, 0,	/* ALLTRIM */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 1, 0,	/* CSTRING */
/* 00026 */ HB_P_LINE, 183, 0,	/* 183 */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_POPLOCAL, 2, 0,	/* NX */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_PUSHSYM, 2, 0,	/* LEN */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_LESSEQUAL,
	HB_P_JUMPFALSE, 77, 0,	/* 77 (abs: 00128) */
/* 00054 */ HB_P_LINE, 184, 0,	/* 184 */
	HB_P_PUSHSYM, 3, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHSTR, 16, 0,	/* 16 */
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 
	HB_P_INSTRING,
	HB_P_NOT,
	HB_P_JUMPFALSE, 19, 0,	/* 19 (abs: 00115) */
/* 00099 */ HB_P_LINE, 185, 0,	/* 185 */
	HB_P_FALSE,
	HB_P_POPLOCAL, 3, 0,	/* LFLAG */
/* 00106 */ HB_P_LINE, 186, 0,	/* 186 */
	HB_P_JUMP, 19, 0,	/* 19 (abs: 00128) */
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00115) */
/* 00115 */ HB_P_LINE, 187, 0,	/* 187 */
	HB_P_PUSHLOCAL, 2, 0,	/* NX */
	HB_P_INC,
	HB_P_POPLOCAL, 2, 0,	/* NX */
	HB_P_JUMP, 168, 255,	/* -88 (abs: 00037) */
/* 00128 */ HB_P_LINE, 189, 0,	/* 189 */
	HB_P_PUSHLOCAL, 3, 0,	/* LFLAG */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00136 */
   };

   hb_vmExecute( pcode, symbols );
}

HARBOUR HB_DECTOBIN( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 2, 1,	/* locals, params */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_POPLOCAL, 2, 0,	/* CNEWSTRING */
/* 00012 */ HB_P_LINE, 219, 0,	/* 219 */
	HB_P_ZERO,
	HB_P_POPLOCAL, 3, 0,	/* NTEMP */
	HB_P_PUSHLOCAL, 1, 0,	/* NNUMBER */
	HB_P_ZERO,
	HB_P_GREATER,
	HB_P_JUMPFALSE, 83, 0,	/* 83 (abs: 00107) */
/* 00027 */ HB_P_LINE, 221, 0,	/* 221 */
	HB_P_PUSHLOCAL, 1, 0,	/* NNUMBER */
	HB_P_PUSHLONG, 2, 0, 0, 0,	/* 2 */
	HB_P_MODULUS,
	HB_P_POPLOCAL, 3, 0,	/* NTEMP */
/* 00042 */ HB_P_LINE, 222, 0,	/* 222 */
	HB_P_PUSHSYM, 3, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	'0', '1', 
	HB_P_PUSHLOCAL, 3, 0,	/* NTEMP */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_PLUS,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHLOCAL, 2, 0,	/* CNEWSTRING */
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CNEWSTRING */
/* 00078 */ HB_P_LINE, 223, 0,	/* 223 */
	HB_P_PUSHSYM, 8, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NNUMBER */
	HB_P_PUSHLOCAL, 3, 0,	/* NTEMP */
	HB_P_MINUS,
	HB_P_PUSHLONG, 2, 0, 0, 0,	/* 2 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 1, 0,	/* NNUMBER */
	HB_P_JUMP, 171, 255,	/* -85 (abs: 00019) */
/* 00107 */ HB_P_LINE, 225, 0,	/* 225 */
	HB_P_PUSHLOCAL, 2, 0,	/* CNEWSTRING */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00115 */
   };

   hb_vmExecute( pcode, symbols );
}

HARBOUR HB_DECTOOCTAL( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 2, 1,	/* locals, params */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_POPLOCAL, 2, 0,	/* CNEWSTRING */
/* 00012 */ HB_P_LINE, 255, 0,	/* 255 */
	HB_P_ZERO,
	HB_P_POPLOCAL, 3, 0,	/* NTEMP */
	HB_P_PUSHLOCAL, 1, 0,	/* NNUMBER */
	HB_P_ZERO,
	HB_P_GREATER,
	HB_P_JUMPFALSE, 89, 0,	/* 89 (abs: 00113) */
/* 00027 */ HB_P_LINE, 1, 1,	/* 257 */
	HB_P_PUSHLOCAL, 1, 0,	/* NNUMBER */
	HB_P_PUSHLONG, 8, 0, 0, 0,	/* 8 */
	HB_P_MODULUS,
	HB_P_POPLOCAL, 3, 0,	/* NTEMP */
/* 00042 */ HB_P_LINE, 2, 1,	/* 258 */
	HB_P_PUSHSYM, 3, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'0', '1', '2', '3', '4', '5', '6', '7', 
	HB_P_PUSHLOCAL, 3, 0,	/* NTEMP */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_PLUS,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHLOCAL, 2, 0,	/* CNEWSTRING */
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CNEWSTRING */
/* 00084 */ HB_P_LINE, 3, 1,	/* 259 */
	HB_P_PUSHSYM, 8, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NNUMBER */
	HB_P_PUSHLOCAL, 3, 0,	/* NTEMP */
	HB_P_MINUS,
	HB_P_PUSHLONG, 8, 0, 0, 0,	/* 8 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 1, 0,	/* NNUMBER */
	HB_P_JUMP, 165, 255,	/* -91 (abs: 00019) */
/* 00113 */ HB_P_LINE, 5, 1,	/* 261 */
	HB_P_PUSHLOCAL, 2, 0,	/* CNEWSTRING */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00121 */
   };

   hb_vmExecute( pcode, symbols );
}

HARBOUR HB_DECTOHEXA( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 2, 1,	/* locals, params */
	HB_P_PUSHSTR, 0, 0,	/* 0 */
	HB_P_POPLOCAL, 2, 0,	/* CNEWSTRING */
/* 00012 */ HB_P_LINE, 35, 1,	/* 291 */
	HB_P_ZERO,
	HB_P_POPLOCAL, 3, 0,	/* NTEMP */
	HB_P_PUSHLOCAL, 1, 0,	/* NNUMBER */
	HB_P_ZERO,
	HB_P_GREATER,
	HB_P_JUMPFALSE, 97, 0,	/* 97 (abs: 00121) */
/* 00027 */ HB_P_LINE, 37, 1,	/* 293 */
	HB_P_PUSHLOCAL, 1, 0,	/* NNUMBER */
	HB_P_PUSHLONG, 16, 0, 0, 0,	/* 16 */
	HB_P_MODULUS,
	HB_P_POPLOCAL, 3, 0,	/* NTEMP */
/* 00042 */ HB_P_LINE, 38, 1,	/* 294 */
	HB_P_PUSHSYM, 3, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHSTR, 16, 0,	/* 16 */
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 
	HB_P_PUSHLOCAL, 3, 0,	/* NTEMP */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_PLUS,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHLOCAL, 2, 0,	/* CNEWSTRING */
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* CNEWSTRING */
/* 00092 */ HB_P_LINE, 39, 1,	/* 295 */
	HB_P_PUSHSYM, 8, 0,	/* INT */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* NNUMBER */
	HB_P_PUSHLOCAL, 3, 0,	/* NTEMP */
	HB_P_MINUS,
	HB_P_PUSHLONG, 16, 0, 0, 0,	/* 16 */
	HB_P_DIVIDE,
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 1, 0,	/* NNUMBER */
	HB_P_JUMP, 157, 255,	/* -99 (abs: 00019) */
/* 00121 */ HB_P_LINE, 41, 1,	/* 297 */
	HB_P_PUSHLOCAL, 2, 0,	/* CNEWSTRING */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00129 */
   };

   hb_vmExecute( pcode, symbols );
}

HARBOUR HB_BINTODEC( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 4, 1,	/* locals, params */
	HB_P_ZERO,
	HB_P_POPLOCAL, 2, 0,	/* NNUMBER */
	HB_P_ZERO,
	HB_P_POPLOCAL, 3, 0,	/* NX */
/* 00014 */ HB_P_LINE, 71, 1,	/* 327 */
	HB_P_PUSHSYM, 1, 0,	/* ALLTRIM */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 4, 0,	/* CNEWSTRING */
/* 00030 */ HB_P_LINE, 72, 1,	/* 328 */
	HB_P_PUSHSYM, 2, 0,	/* LEN */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 4, 0,	/* CNEWSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 5, 0,	/* NLEN */
/* 00046 */ HB_P_LINE, 73, 1,	/* 329 */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_POPLOCAL, 3, 0,	/* NX */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_PUSHLOCAL, 5, 0,	/* NLEN */
	HB_P_LESSEQUAL,
	HB_P_JUMPFALSE, 73, 0,	/* 73 (abs: 00137) */
/* 00067 */ HB_P_LINE, 74, 1,	/* 330 */
	HB_P_PUSHLOCAL, 2, 0,	/* NNUMBER */
	HB_P_PUSHSYM, 12, 0,	/* AT */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 3, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 4, 0,	/* CNEWSTRING */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	'0', '1', 
	HB_P_FUNCTION, 2, 0,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_MINUS,
	HB_P_PUSHLONG, 2, 0, 0, 0,	/* 2 */
	HB_P_PUSHLOCAL, 5, 0,	/* NLEN */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_MINUS,
	HB_P_POWER,
	HB_P_MULT,
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* NNUMBER */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_INC,
	HB_P_POPLOCAL, 3, 0,	/* NX */
	HB_P_JUMP, 179, 255,	/* -77 (abs: 00057) */
/* 00137 */ HB_P_LINE, 77, 1,	/* 333 */
	HB_P_PUSHLOCAL, 2, 0,	/* NNUMBER */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00145 */
   };

   hb_vmExecute( pcode, symbols );
}

HARBOUR HB_OCTALTODEC( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 4, 1,	/* locals, params */
	HB_P_ZERO,
	HB_P_POPLOCAL, 2, 0,	/* NNUMBER */
	HB_P_ZERO,
	HB_P_POPLOCAL, 3, 0,	/* NX */
/* 00014 */ HB_P_LINE, 107, 1,	/* 363 */
	HB_P_PUSHSYM, 1, 0,	/* ALLTRIM */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 4, 0,	/* CNEWSTRING */
/* 00030 */ HB_P_LINE, 108, 1,	/* 364 */
	HB_P_PUSHSYM, 2, 0,	/* LEN */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 4, 0,	/* CNEWSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 5, 0,	/* NLEN */
/* 00046 */ HB_P_LINE, 109, 1,	/* 365 */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_POPLOCAL, 3, 0,	/* NX */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_PUSHLOCAL, 5, 0,	/* NLEN */
	HB_P_LESSEQUAL,
	HB_P_JUMPFALSE, 79, 0,	/* 79 (abs: 00143) */
/* 00067 */ HB_P_LINE, 110, 1,	/* 366 */
	HB_P_PUSHLOCAL, 2, 0,	/* NNUMBER */
	HB_P_PUSHSYM, 12, 0,	/* AT */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 3, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 4, 0,	/* CNEWSTRING */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'0', '1', '2', '3', '4', '5', '6', '7', 
	HB_P_FUNCTION, 2, 0,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_MINUS,
	HB_P_PUSHLONG, 8, 0, 0, 0,	/* 8 */
	HB_P_PUSHLOCAL, 5, 0,	/* NLEN */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_MINUS,
	HB_P_POWER,
	HB_P_MULT,
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* NNUMBER */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_INC,
	HB_P_POPLOCAL, 3, 0,	/* NX */
	HB_P_JUMP, 173, 255,	/* -83 (abs: 00057) */
/* 00143 */ HB_P_LINE, 113, 1,	/* 369 */
	HB_P_PUSHLOCAL, 2, 0,	/* NNUMBER */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00151 */
   };

   hb_vmExecute( pcode, symbols );
}

HARBOUR HB_HEXATODEC( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 4, 1,	/* locals, params */
	HB_P_ZERO,
	HB_P_POPLOCAL, 2, 0,	/* NNUMBER */
	HB_P_ZERO,
	HB_P_POPLOCAL, 3, 0,	/* NX */
/* 00014 */ HB_P_LINE, 143, 1,	/* 399 */
	HB_P_PUSHSYM, 1, 0,	/* ALLTRIM */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 4, 0,	/* CNEWSTRING */
/* 00030 */ HB_P_LINE, 144, 1,	/* 400 */
	HB_P_PUSHSYM, 2, 0,	/* LEN */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 4, 0,	/* CNEWSTRING */
	HB_P_FUNCTION, 1, 0,
	HB_P_POPLOCAL, 5, 0,	/* NLEN */
/* 00046 */ HB_P_LINE, 145, 1,	/* 401 */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_POPLOCAL, 3, 0,	/* NX */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_PUSHLOCAL, 5, 0,	/* NLEN */
	HB_P_LESSEQUAL,
	HB_P_JUMPFALSE, 87, 0,	/* 87 (abs: 00151) */
/* 00067 */ HB_P_LINE, 146, 1,	/* 402 */
	HB_P_PUSHLOCAL, 2, 0,	/* NNUMBER */
	HB_P_PUSHSYM, 12, 0,	/* AT */
	HB_P_PUSHNIL,
	HB_P_PUSHSYM, 3, 0,	/* SUBSTR */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 4, 0,	/* CNEWSTRING */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_FUNCTION, 3, 0,
	HB_P_PUSHSTR, 16, 0,	/* 16 */
	'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 
	HB_P_FUNCTION, 2, 0,
	HB_P_PUSHLONG, 1, 0, 0, 0,	/* 1 */
	HB_P_MINUS,
	HB_P_PUSHLONG, 16, 0, 0, 0,	/* 16 */
	HB_P_PUSHLOCAL, 5, 0,	/* NLEN */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_MINUS,
	HB_P_POWER,
	HB_P_MULT,
	HB_P_PLUS,
	HB_P_POPLOCAL, 2, 0,	/* NNUMBER */
	HB_P_PUSHLOCAL, 3, 0,	/* NX */
	HB_P_INC,
	HB_P_POPLOCAL, 3, 0,	/* NX */
	HB_P_JUMP, 165, 255,	/* -91 (abs: 00057) */
/* 00151 */ HB_P_LINE, 149, 1,	/* 405 */
	HB_P_PUSHLOCAL, 2, 0,	/* NNUMBER */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00159 */
   };

   hb_vmExecute( pcode, symbols );
}

