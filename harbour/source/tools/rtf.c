/*
 * Harbour Compiler, Build 30b (1999.10.16)
 * Generated C source code
 */

#include "hb_vmpub.h"
#include "init.h"


HARBOUR HB_TRTF( void );
static HARBOUR HB_TRTF_NEW( void );
static HARBOUR HB_TRTF_WRITEHEADER( void );
static HARBOUR HB_TRTF_WRITEPAR( void );
static HARBOUR HB_TRTF_WRITEPARBOLD( void );
static HARBOUR HB_TRTF_WRITETITLE( void );
static HARBOUR HB_TRTF_CLOSE( void );
static HARBOUR HB_TRTF_WRITELINK( void );
static HARBOUR hb_INITSTATICS( void );
extern HARBOUR HB_TCLASS( void );
extern HARBOUR HB_VALTYPE( void );
extern HARBOUR HB_FCREATE( void );
extern HARBOUR HB_FWRITE( void );
extern HARBOUR HB_AT( void );
extern HARBOUR HB_STRTRAN( void );
extern HARBOUR HB_FCLOSE( void );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_RTF )
{ "TRTF", FS_PUBLIC, HB_TRTF, 0 },
{ "TCLASS", FS_PUBLIC, HB_TCLASS, 0 },
{ "NEW", FS_PUBLIC, 0, 0 },
{ "ADDDATA", FS_PUBLIC, 0, 0 },
{ "ADDMETHOD", FS_PUBLIC, 0, 0 },
{ "TRTF_WRITEHEADER", FS_STATIC, HB_TRTF_WRITEHEADER, 0 },
{ "TRTF_NEW", FS_STATIC, HB_TRTF_NEW, 0 },
{ "TRTF_WRITEPAR", FS_STATIC, HB_TRTF_WRITEPAR, 0 },
{ "TRTF_WRITELINK", FS_STATIC, HB_TRTF_WRITELINK, 0 },
{ "TRTF_CLOSE", FS_STATIC, HB_TRTF_CLOSE, 0 },
{ "TRTF_WRITEPARBOLD", FS_STATIC, HB_TRTF_WRITEPARBOLD, 0 },
{ "TRTF_WRITETITLE", FS_STATIC, HB_TRTF_WRITETITLE, 0 },
{ "CREATE", FS_PUBLIC, 0, 0 },
{ "INSTANCE", FS_PUBLIC, 0, 0 },
{ "VALTYPE", FS_PUBLIC, HB_VALTYPE, 0 },
{ "_CFILE", FS_PUBLIC, 0, 0 },
{ "_NHANDLE", FS_PUBLIC, 0, 0 },
{ "FCREATE", FS_PUBLIC, HB_FCREATE, 0 },
{ "CFILE", FS_PUBLIC, 0, 0 },
{ "FWRITE", FS_PUBLIC, HB_FWRITE, 0 },
{ "NHANDLE", FS_PUBLIC, 0, 0 },
{ "AT", FS_PUBLIC, HB_AT, 0 },
{ "STRTRAN", FS_PUBLIC, HB_STRTRAN, 0 },
{ "WRITEPAR", FS_PUBLIC, 0, 0 },
{ "FCLOSE", FS_PUBLIC, HB_FCLOSE, 0 },
{ "(_INITSTATICS)", FS_INIT | FS_EXIT, hb_INITSTATICS, 0}
HB_INIT_SYMBOLS_END( hb_vm_SymbolInit_RTF )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_vm_SymbolInit_RTF
#endif


HARBOUR HB_TRTF( void )
{
   static BYTE pcode[] =
   {
	HB_P_SFRAME, 25, 0,	/* symbol (_INITSTATICS) */
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_PUSHNIL,
	HB_P_EXACTLYEQUAL,
	HB_P_JUMPFALSE, 25, 1,	/* 281 (abs: 00292) */
/* 00014 */ HB_P_LINE, 35, 0,	/* 35 */
	HB_P_PUSHSYM, 1, 0,	/* TCLASS */
	HB_P_PUSHNIL,
	HB_P_FUNCTION, 0, 0,
	HB_P_MESSAGE, 2, 0,	/* NEW */
	HB_P_PUSHSTR, 4, 0,	/* 4 */
	'T', 'R', 'T', 'F', 
	HB_P_FUNCTION, 1, 0,
	HB_P_POPSTATIC, 1, 0,	/* OCLASS */
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_MESSAGE, 3, 0,	/* ADDDATA */
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'c', 'F', 'i', 'l', 'e', 
	HB_P_FUNCTION, 1, 0,
	HB_P_POP,
/* 00058 */ HB_P_LINE, 38, 0,	/* 38 */
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_MESSAGE, 3, 0,	/* ADDDATA */
	HB_P_PUSHSTR, 7, 0,	/* 7 */
	'n', 'H', 'a', 'n', 'd', 'l', 'e', 
	HB_P_FUNCTION, 1, 0,
	HB_P_POP,
/* 00081 */ HB_P_LINE, 39, 0,	/* 39 */
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_MESSAGE, 4, 0,	/* ADDMETHOD */
	HB_P_PUSHSTR, 11, 0,	/* 11 */
	'W', 'r', 'i', 't', 'e', 'H', 'e', 'a', 'd', 'e', 'r', 
	HB_P_PUSHSYM, 5, 0,	/* TRTF_WRITEHEADER */
	HB_P_FUNCPTR,
	HB_P_FUNCTION, 2, 0,
	HB_P_POP,
/* 00112 */ HB_P_LINE, 40, 0,	/* 40 */
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_MESSAGE, 4, 0,	/* ADDMETHOD */
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	'N', 'e', 'w', 
	HB_P_PUSHSYM, 6, 0,	/* TRTF_NEW */
	HB_P_FUNCPTR,
	HB_P_FUNCTION, 2, 0,
	HB_P_POP,
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_MESSAGE, 4, 0,	/* ADDMETHOD */
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'W', 'r', 'i', 't', 'e', 'P', 'a', 'r', 
	HB_P_PUSHSYM, 7, 0,	/* TRTF_WRITEPAR */
	HB_P_FUNCPTR,
	HB_P_FUNCTION, 2, 0,
	HB_P_POP,
/* 00160 */ HB_P_LINE, 43, 0,	/* 43 */
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_MESSAGE, 4, 0,	/* ADDMETHOD */
	HB_P_PUSHSTR, 9, 0,	/* 9 */
	'W', 'r', 'i', 't', 'e', 'L', 'i', 'n', 'k', 
	HB_P_PUSHSYM, 8, 0,	/* TRTF_WRITELINK */
	HB_P_FUNCPTR,
	HB_P_FUNCTION, 2, 0,
	HB_P_POP,
/* 00189 */ HB_P_LINE, 44, 0,	/* 44 */
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_MESSAGE, 4, 0,	/* ADDMETHOD */
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'C', 'l', 'o', 's', 'e', 
	HB_P_PUSHSYM, 9, 0,	/* TRTF_CLOSE */
	HB_P_FUNCPTR,
	HB_P_FUNCTION, 2, 0,
	HB_P_POP,
/* 00214 */ HB_P_LINE, 45, 0,	/* 45 */
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_MESSAGE, 4, 0,	/* ADDMETHOD */
	HB_P_PUSHSTR, 12, 0,	/* 12 */
	'W', 'r', 'i', 't', 'e', 'P', 'a', 'r', 'B', 'o', 'l', 'd', 
	HB_P_PUSHSYM, 10, 0,	/* TRTF_WRITEPARBOLD */
	HB_P_FUNCPTR,
	HB_P_FUNCTION, 2, 0,
	HB_P_POP,
/* 00246 */ HB_P_LINE, 46, 0,	/* 46 */
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_MESSAGE, 4, 0,	/* ADDMETHOD */
	HB_P_PUSHSTR, 10, 0,	/* 10 */
	'W', 'r', 'i', 't', 'e', 'T', 'i', 't', 'l', 'e', 
	HB_P_PUSHSYM, 11, 0,	/* TRTF_WRITETITLE */
	HB_P_FUNCPTR,
	HB_P_FUNCTION, 2, 0,
	HB_P_POP,
/* 00276 */ HB_P_LINE, 47, 0,	/* 47 */
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_MESSAGE, 12, 0,	/* CREATE */
	HB_P_FUNCTION, 0, 0,
	HB_P_POP,
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00292) */
	HB_P_PUSHSTATIC, 1, 0,	/* OCLASS */
	HB_P_MESSAGE, 13, 0,	/* INSTANCE */
	HB_P_FUNCTION, 0, 0,
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00303 */
   };

   hb_vmExecute( pcode, symbols );
}

static HARBOUR HB_TRTF_NEW( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 1, 1,	/* locals, params */
	HB_P_PUSHSELF,
	HB_P_POPLOCAL, 2, 0,	/* SELF */
/* 00010 */ HB_P_LINE, 50, 0,	/* 50 */
	HB_P_PUSHSYM, 14, 0,	/* VALTYPE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CFILE */
	HB_P_FUNCTION, 1, 0,
	HB_P_PUSHNIL,
	HB_P_NOTEQUAL,
	HB_P_DUPLICATE,
	HB_P_JUMPFALSE, 19, 0,	/* 19 (abs: 00045) */
	HB_P_PUSHSYM, 14, 0,	/* VALTYPE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CFILE */
	HB_P_FUNCTION, 1, 0,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	'C', 
	HB_P_EXACTLYEQUAL,
	HB_P_AND,
	HB_P_JUMPFALSE, 51, 0,	/* 51 (abs: 00096) */
/* 00048 */ HB_P_LINE, 51, 0,	/* 51 */
	HB_P_PUSHLOCAL, 2, 0,	/* SELF */
	HB_P_MESSAGE, 15, 0,	/* _CFILE */
	HB_P_PUSHLOCAL, 1, 0,	/* CFILE */
	HB_P_FUNCTION, 1, 0,
	HB_P_POP,
/* 00064 */ HB_P_LINE, 52, 0,	/* 52 */
	HB_P_PUSHLOCAL, 2, 0,	/* SELF */
	HB_P_MESSAGE, 16, 0,	/* _NHANDLE */
	HB_P_PUSHSYM, 17, 0,	/* FCREATE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 2, 0,	/* SELF */
	HB_P_MESSAGE, 18, 0,	/* CFILE */
	HB_P_FUNCTION, 0, 0,
	HB_P_FUNCTION, 1, 0,
	HB_P_FUNCTION, 1, 0,
	HB_P_POP,
	HB_P_JUMP, 3, 0,	/* 3 (abs: 00096) */
/* 00096 */ HB_P_LINE, 54, 0,	/* 54 */
	HB_P_PUSHLOCAL, 2, 0,	/* SELF */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00104 */
   };

   hb_vmExecute( pcode, symbols );
}

static HARBOUR HB_TRTF_WRITEHEADER( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 3, 0,	/* locals, params */
	HB_P_PUSHSELF,
	HB_P_POPLOCAL, 1, 0,	/* SELF */
/* 00010 */ HB_P_LINE, 74, 0,	/* 74 */
	HB_P_PUSHSTR, 188, 1,	/* 444 */
	'{', 92, 'r', 't', 'f', '1', 92, 'a', 'n', 's', 'i', 92, 'p', 'a', 'r', 'd', 92, 'p', 'l', 'a', 'i', 'n', 92, 'f', 's', '2', '0', 13, 10, 92, 'd', 'e', 'f', 'f', '5', '{', 92, 'f', 'o', 'n', 't', 't', 'b', 'l', 13, 10, '{', 92, 'f', '0', 92, 'f', 'r', 'o', 'm', 'a', 'n', ' ', 'T', 'm', 's', ' ', 'R', 'm', 'n', ';', '}', 13, 10, '{', 92, 'f', '1', 92, 'f', 'd', 'e', 'c', 'o', 'r', ' ', 'S', 'y', 'm', 'b', 'o', 'l', ';', '}', 13, 10, '{', 92, 'f', '2', 92, 'f', 's', 'w', 'i', 's', 's', ' ', 'H', 'e', 'l', 'v', ';', '}', 13, 10, '{', 92, 'f', '3', 92, 'f', 'm', 'o', 'd', 'e', 'r', 'n', ' ', 'L', 'i', 'n', 'e', 'P', 'r', 'i', 'n', 't', 'e', 'r', ';', '}', 13, 10, '{', 92, 'f', '4', 92, 'f', 'r', 'o', 'm', 'a', 'n', ' ', 'T', 'e', 'r', 'm', 'i', 'n', 'a', 'l', ';', '}', 13, 10, '{', 92, 'f', '5', 92, 'f', 'r', 'o', 'm', 'a', 'n', ' ', 'T', 'i', 'm', 'e', 's', ' ', 'N', 'e', 'w', ' ', 'R', 'o', 'm', 'a', 'n', ';', '}', 13, 10, '{', 92, 'f', '6', 92, 'f', 's', 'w', 'i', 's', 's', ' ', 'A', 'r', 'i', 'a', 'l', ';', '}', 13, 10, '{', 92, 'f', '7', 92, 'f', 'r', 'o', 'm', 'a', 'n', ' ', 'C', 'G', ' ', 'T', 'i', 'm', 'e', 's', ' ', '(', 'W', 'N', ')', ';', '}', 13, 10, '{', 92, 'f', '8', 92, 'f', 'm', 'o', 'd', 'e', 'r', 'n', ' ', 'C', 'o', 'u', 'r', 'i', 'e', 'r', ';', '}', 13, 10, '{', 92, 'f', '9', 92, 'f', 'm', 'o', 'd', 'e', 'r', 'n', ' ', 'M', 'o', 'd', 'e', 'r', 'n', ';', '}', 13, 10, '{', 92, 'f', '1', '0', 92, 'f', 's', 'c', 'r', 'i', 'p', 't', ' ', 'S', 'c', 'r', 'i', 'p', 't', ';', '}', 13, 10, '{', 92, 'f', '1', '1', 92, 'f', 's', 'w', 'i', 's', 's', ' ', 'U', 'n', 'i', 'v', 'e', 'r', 's', ' ', '(', 'W', 'N', ')', ';', '}', 13, 10, '{', 92, 'f', '1', '2', 92, 'f', 'n', 'i', 'l', ' ', 'W', 'i', 'n', 'g', 'd', 'i', 'n', 'g', 's', ';', '}', 13, 10, '{', 92, 'f', '1', '3', 92, 'f', 's', 'w', 'i', 's', 's', 92, 'f', 'p', 'r', 'q', '2', 92, 'f', 'c', 'h', 'a', 'r', 's', 'e', 't', '2', '3', '8', ' ', 'V', 'e', 'r', 'd', 'a', 'n', 'a', ';', '}', 13, 10, '{', 92, 'f', '1', '4', 92, 'f', 's', 'w', 'i', 's', 's', ' ', 'M', 'S', ' ', 'S', 'a', 'n', 's', ' ', 'S', 'e', 'r', 'I', 'F', ';', '}', 13, 10, '}', ';', 13, 10, 
	HB_P_POPLOCAL, 2, 0,	/* CHEADER */
/* 00463 */ HB_P_LINE, 93, 0,	/* 93 */
	HB_P_PUSHSTR, 149, 1,	/* 405 */
	'{', 92, 'c', 'o', 'l', 'o', 'r', 't', 'b', 'l', ';', 13, 10, 92, 'r', 'e', 'd', '0', 92, 'g', 'r', 'e', 'e', 'n', '0', 92, 'b', 'l', 'u', 'e', '0', ';', 13, 10, 92, 'r', 'e', 'd', '0', 92, 'g', 'r', 'e', 'e', 'n', '0', 92, 'b', 'l', 'u', 'e', '1', '2', '8', ';', 13, 10, 92, 'r', 'e', 'd', '0', 92, 'g', 'r', 'e', 'e', 'n', '1', '2', '8', 92, 'b', 'l', 'u', 'e', '1', '2', '8', ';', 13, 10, 92, 'r', 'e', 'd', '0', 92, 'g', 'r', 'e', 'e', 'n', '1', '2', '8', 92, 'b', 'l', 'u', 'e', '0', ';', 13, 10, 92, 'r', 'e', 'd', '1', '2', '8', 92, 'g', 'r', 'e', 'e', 'n', '0', 92, 'b', 'l', 'u', 'e', '0', ';', 13, 10, 92, 'r', 'e', 'd', '1', '2', '8', 92, 'g', 'r', 'e', 'e', 'n', '0', 92, 'b', 'l', 'u', 'e', '1', '2', '8', ';', 13, 10, 92, 'r', 'e', 'd', '1', '2', '8', 92, 'g', 'r', 'e', 'e', 'n', '1', '2', '8', 92, 'b', 'l', 'u', 'e', '0', ';', 13, 10, 92, 'r', 'e', 'd', '1', '2', '8', 92, 'g', 'r', 'e', 'e', 'n', '1', '2', '8', 92, 'b', 'l', 'u', 'e', '1', '2', '8', ';', 13, 10, 92, 'r', 'e', 'd', '6', '4', 92, 'g', 'r', 'e', 'e', 'n', '6', '4', 92, 'b', 'l', 'u', 'e', '6', '4', ';', 13, 10, 92, 'r', 'e', 'd', '0', 92, 'g', 'r', 'e', 'e', 'n', '0', 92, 'b', 'l', 'u', 'e', '2', '5', '5', ';', 13, 10, 92, 'r', 'e', 'd', '0', 92, 'g', 'r', 'e', 'e', 'n', '2', '5', '5', 92, 'b', 'l', 'u', 'e', '2', '5', '5', ';', 13, 10, 92, 'r', 'e', 'd', '0', 92, 'g', 'r', 'e', 'e', 'n', '2', '5', '5', 92, 'b', 'l', 'u', 'e', '0', ';', 13, 10, 92, 'r', 'e', 'd', '2', '5', '5', 92, 'g', 'r', 'e', 'e', 'n', '0', 92, 'b', 'l', 'u', 'e', '0', ';', 13, 10, 92, 'r', 'e', 'd', '1', '9', '2', 92, 'g', 'r', 'e', 'e', 'n', '1', '9', '2', 92, 'b', 'l', 'u', 'e', '1', '9', '2', ';', 13, 10, 92, 'r', 'e', 'd', '2', '5', '5', 92, 'g', 'r', 'e', 'e', 'n', '2', '5', '5', 92, 'b', 'l', 'u', 'e', '0', ';', 13, 10, 92, 'r', 'e', 'd', '2', '5', '5', 92, 'g', 'r', 'e', 'e', 'n', '2', '5', '5', 92, 'b', 'l', 'u', 'e', '2', '5', '5', ';', 13, 10, '}', 13, 10, 
	HB_P_POPLOCAL, 3, 0,	/* CCOLORTABLE */
/* 00877 */ HB_P_LINE, 95, 0,	/* 95 */
	HB_P_PUSHSYM, 19, 0,	/* FWRITE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* SELF */
	HB_P_MESSAGE, 20, 0,	/* NHANDLE */
	HB_P_FUNCTION, 0, 0,
	HB_P_PUSHLOCAL, 2, 0,	/* CHEADER */
	HB_P_DO, 2, 0,
/* 00899 */ HB_P_LINE, 97, 0,	/* 97 */
	HB_P_PUSHSYM, 19, 0,	/* FWRITE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* SELF */
	HB_P_MESSAGE, 20, 0,	/* NHANDLE */
	HB_P_FUNCTION, 0, 0,
	HB_P_PUSHLOCAL, 3, 0,	/* CCOLORTABLE */
	HB_P_DO, 2, 0,
/* 00921 */ HB_P_LINE, 98, 0,	/* 98 */
	HB_P_PUSHLOCAL, 1, 0,	/* SELF */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00929 */
   };

   hb_vmExecute( pcode, symbols );
}

static HARBOUR HB_TRTF_WRITEPAR( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 1, 1,	/* locals, params */
	HB_P_PUSHSELF,
	HB_P_POPLOCAL, 2, 0,	/* SELF */
/* 00010 */ HB_P_LINE, 101, 0,	/* 101 */
	HB_P_PUSHSYM, 19, 0,	/* FWRITE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 2, 0,	/* SELF */
	HB_P_MESSAGE, 20, 0,	/* NHANDLE */
	HB_P_FUNCTION, 0, 0,
	HB_P_PUSHSTR, 32, 0,	/* 32 */
	92, 'p', 'a', 'r', 'd', '{', 92, 'p', 'l', 'a', 'i', 'n', 92, 'c', 'f', '1', 92, 'f', '2', 92, 'f', 's', '5', '0', 92, 'i', 92, 'b', 92, 'q', 'c', ' ', 
	HB_P_PUSHLOCAL, 1, 0,	/* CPAR */
	HB_P_PLUS,
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	' ', '}', 92, 'p', 'a', 'r', 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	13, 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	10, 
	HB_P_PLUS,
	HB_P_DO, 2, 0,
/* 00088 */ HB_P_LINE, 102, 0,	/* 102 */
	HB_P_PUSHLOCAL, 2, 0,	/* SELF */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00096 */
   };

   hb_vmExecute( pcode, symbols );
}

static HARBOUR HB_TRTF_WRITEPARBOLD( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 1, 1,	/* locals, params */
	HB_P_PUSHSELF,
	HB_P_POPLOCAL, 2, 0,	/* SELF */
/* 00010 */ HB_P_LINE, 104, 0,	/* 104 */
	HB_P_PUSHSYM, 19, 0,	/* FWRITE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 2, 0,	/* SELF */
	HB_P_MESSAGE, 20, 0,	/* NHANDLE */
	HB_P_FUNCTION, 0, 0,
	HB_P_PUSHSTR, 16, 0,	/* 16 */
	92, 'p', 'a', 'r', 'd', '{', 92, 'c', 'f', '1', 92, 'f', 's', '2', '0', ' ', 
	HB_P_PUSHLOCAL, 1, 0,	/* CPAR */
	HB_P_PLUS,
	HB_P_PUSHSTR, 6, 0,	/* 6 */
	' ', '}', 92, 'p', 'a', 'r', 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	13, 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	10, 
	HB_P_PLUS,
	HB_P_DO, 2, 0,
/* 00072 */ HB_P_LINE, 105, 0,	/* 105 */
	HB_P_PUSHLOCAL, 2, 0,	/* SELF */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00080 */
   };

   hb_vmExecute( pcode, symbols );
}

static HARBOUR HB_TRTF_WRITETITLE( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 4, 2,	/* locals, params */
	HB_P_PUSHSELF,
	HB_P_POPLOCAL, 3, 0,	/* SELF */
/* 00010 */ HB_P_LINE, 110, 0,	/* 110 */
	HB_P_PUSHSYM, 21, 0,	/* AT */
	HB_P_PUSHNIL,
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	'(', ')', 
	HB_P_PUSHLOCAL, 1, 0,	/* CTITLE */
	HB_P_FUNCTION, 2, 0,
	HB_P_POPLOCAL, 5, 0,	/* NPOS */
/* 00031 */ HB_P_LINE, 111, 0,	/* 111 */
	HB_P_PUSHLOCAL, 5, 0,	/* NPOS */
	HB_P_ZERO,
	HB_P_GREATER,
	HB_P_JUMPFALSE, 32, 0,	/* 32 (abs: 00071) */
/* 00042 */ HB_P_LINE, 112, 0,	/* 112 */
	HB_P_PUSHSYM, 22, 0,	/* STRTRAN */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CTITLE */
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	'(', ')', 
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	'x', 'x', 
	HB_P_FUNCTION, 3, 0,
	HB_P_POPLOCAL, 4, 0,	/* CTEMP */
	HB_P_JUMP, 12, 0,	/* 12 (abs: 00080) */
/* 00071 */ HB_P_LINE, 114, 0,	/* 114 */
	HB_P_PUSHLOCAL, 1, 0,	/* CTITLE */
	HB_P_POPLOCAL, 4, 0,	/* CTEMP */
/* 00080 */ HB_P_LINE, 120, 0,	/* 120 */
	HB_P_PUSHSTR, 40, 0,	/* 40 */
	'{', 92, 'f', '2', 13, 10, ' ', ' ', '#', '{', 92, 'f', 'o', 'o', 't', 'n', 'o', 't', 'e', ' ', 92, 'p', 'a', 'r', 'd', 92, 'p', 'l', 'a', 'i', 'n', ' ', 92, 'f', 's', '2', '0', ' ', '#', ' ', 
	HB_P_PUSHLOCAL, 4, 0,	/* CTEMP */
	HB_P_PLUS,
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	' ', '}', 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	13, 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	10, 
	HB_P_PLUS,
	HB_P_PUSHSTR, 34, 0,	/* 34 */
	' ', ' ', '$', '{', 92, 'f', 'o', 'o', 't', 'n', 'o', 't', 'e', ' ', 92, 'p', 'a', 'r', 'd', 92, 'p', 'l', 'a', 'i', 'n', ' ', 92, 'f', 's', '2', '0', ' ', '$', ' ', 
	HB_P_PLUS,
	HB_P_PUSHLOCAL, 2, 0,	/* CTOPIC */
	HB_P_PLUS,
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	' ', '}', 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	13, 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	10, 
	HB_P_PLUS,
	HB_P_PUSHSTR, 34, 0,	/* 34 */
	' ', ' ', 'K', '{', 92, 'f', 'o', 'o', 't', 'n', 'o', 't', 'e', ' ', 92, 'p', 'a', 'r', 'd', 92, 'p', 'l', 'a', 'i', 'n', ' ', 92, 'f', 's', '2', '0', ' ', 'K', ' ', 
	HB_P_PLUS,
	HB_P_PUSHLOCAL, 2, 0,	/* CTOPIC */
	HB_P_PLUS,
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	' ', '}', 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	13, 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	10, 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	'}', 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	13, 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	10, 
	HB_P_PLUS,
	HB_P_POPLOCAL, 6, 0,	/* CWRITE */
/* 00280 */ HB_P_LINE, 122, 0,	/* 122 */
	HB_P_PUSHSYM, 19, 0,	/* FWRITE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 3, 0,	/* SELF */
	HB_P_MESSAGE, 20, 0,	/* NHANDLE */
	HB_P_FUNCTION, 0, 0,
	HB_P_PUSHLOCAL, 6, 0,	/* CWRITE */
	HB_P_DO, 2, 0,
/* 00302 */ HB_P_LINE, 123, 0,	/* 123 */
	HB_P_PUSHLOCAL, 3, 0,	/* SELF */
	HB_P_MESSAGE, 23, 0,	/* WRITEPAR */
	HB_P_PUSHLOCAL, 2, 0,	/* CTOPIC */
	HB_P_FUNCTION, 1, 0,
	HB_P_POP,
/* 00318 */ HB_P_LINE, 124, 0,	/* 124 */
	HB_P_PUSHLOCAL, 3, 0,	/* SELF */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00326 */
   };

   hb_vmExecute( pcode, symbols );
}

static HARBOUR HB_TRTF_CLOSE( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 1, 0,	/* locals, params */
	HB_P_PUSHSELF,
	HB_P_POPLOCAL, 1, 0,	/* SELF */
/* 00010 */ HB_P_LINE, 126, 0,	/* 126 */
	HB_P_PUSHSYM, 19, 0,	/* FWRITE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* SELF */
	HB_P_MESSAGE, 20, 0,	/* NHANDLE */
	HB_P_FUNCTION, 0, 0,
	HB_P_PUSHSTR, 3, 0,	/* 3 */
	'}', 13, 10, 
	HB_P_DO, 2, 0,
/* 00035 */ HB_P_LINE, 127, 0,	/* 127 */
	HB_P_PUSHSYM, 19, 0,	/* FWRITE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* SELF */
	HB_P_MESSAGE, 20, 0,	/* NHANDLE */
	HB_P_FUNCTION, 0, 0,
	HB_P_PUSHSTR, 7, 0,	/* 7 */
	92, 'p', 'a', 'g', 'e', 13, 10, 
	HB_P_DO, 2, 0,
/* 00064 */ HB_P_LINE, 128, 0,	/* 128 */
	HB_P_PUSHSYM, 24, 0,	/* FCLOSE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* SELF */
	HB_P_MESSAGE, 20, 0,	/* NHANDLE */
	HB_P_FUNCTION, 0, 0,
	HB_P_DO, 1, 0,
/* 00083 */ HB_P_LINE, 129, 0,	/* 129 */
	HB_P_PUSHLOCAL, 1, 0,	/* SELF */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00091 */
   };

   hb_vmExecute( pcode, symbols );
}

static HARBOUR HB_TRTF_WRITELINK( void )
{
   static BYTE pcode[] =
   {
	HB_P_FRAME, 1, 1,	/* locals, params */
	HB_P_PUSHSELF,
	HB_P_POPLOCAL, 2, 0,	/* SELF */
/* 00010 */ HB_P_LINE, 132, 0,	/* 132 */
	HB_P_PUSHSYM, 19, 0,	/* FWRITE */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 2, 0,	/* SELF */
	HB_P_MESSAGE, 20, 0,	/* NHANDLE */
	HB_P_FUNCTION, 0, 0,
	HB_P_PUSHSTR, 35, 0,	/* 35 */
	92, 'p', 'a', 'r', 'd', '{', 92, 'c', 'f', '1', 92, 'f', 's', '2', '0', ' ', 'S', 'e', 'e', ' ', 'A', 'l', 's', 'o', ' ', '{', 92, 'f', '2', 92, 'u', 'l', 'd', 'b', ' ', 
	HB_P_PUSHLOCAL, 1, 0,	/* CLINK */
	HB_P_PLUS,
	HB_P_PUSHSTR, 8, 0,	/* 8 */
	'}', '{', 92, 'v', 92, 'f', '2', ' ', 
	HB_P_PLUS,
	HB_P_PUSHSYM, 22, 0,	/* STRTRAN */
	HB_P_PUSHNIL,
	HB_P_PUSHLOCAL, 1, 0,	/* CLINK */
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	'(', ')', 
	HB_P_PUSHSTR, 2, 0,	/* 2 */
	'x', 'x', 
	HB_P_FUNCTION, 3, 0,
	HB_P_PLUS,
	HB_P_PUSHSTR, 5, 0,	/* 5 */
	'}', 92, 'p', 'a', 'r', 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	13, 
	HB_P_PLUS,
	HB_P_PUSHSTR, 1, 0,	/* 1 */
	10, 
	HB_P_PLUS,
	HB_P_DO, 2, 0,
/* 00123 */ HB_P_LINE, 133, 0,	/* 133 */
	HB_P_PUSHLOCAL, 2, 0,	/* SELF */
	HB_P_RETVALUE,
	HB_P_ENDPROC
/* 00131 */
   };

   hb_vmExecute( pcode, symbols );
}

static HARBOUR hb_INITSTATICS( void )
{
   static BYTE pcode[] =
   {
	HB_P_STATICS, 25, 0, 1, 0,	/* symbol (_INITSTATICS), 1 statics */
	HB_P_SFRAME, 25, 0,	/* symbol (_INITSTATICS) */
/* 00008 */ HB_P_ENDPROC
   };

   hb_vmExecute( pcode, symbols );
}

