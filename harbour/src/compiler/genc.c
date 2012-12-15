/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler C source generation
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

#include "hbcomp.h"
#include "hbdate.h"
#include "hbassert.h"

static void hb_compGenCReadable( HB_COMP_DECL, PFUNCTION pFunc, FILE * yyc );
static void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc );
static void hb_compGenCFunc( FILE * yyc, const char * cDecor, const char * szName, HB_BOOL fStrip, int iFuncSuffix );
static void hb_writeEndInit( HB_COMP_DECL, FILE * yyc, const char * szModulname, const char * szSourceFile );

/* helper structure to pass information */
typedef struct HB_stru_genc_info
{
   HB_COMP_DECL;
   FILE *  yyc;
   HB_BOOL bVerbose;
   HB_SIZE nEndBlockPos;
} HB_GENC_INFO, * HB_GENC_INFO_PTR;

#define HB_GENC_FUNC( func )  HB_PCODE_FUNC( func, HB_GENC_INFO_PTR )
typedef HB_GENC_FUNC( HB_GENC_FUNC_ );
typedef HB_GENC_FUNC_ * HB_GENC_FUNC_PTR;

static void hb_compDumpFindCFunc( HB_COMP_DECL )
{
   PHB_HINLINE pInline;

   pInline = HB_COMP_PARAM->inlines.pFirst;
   while( pInline )
   {
      if( pInline->pCode && ! pInline->szName )
      {
         const char * pszCCode = ( const char * ) pInline->pCode;
         char         ch;
         int          len;
         while( ( ch = *pszCCode++ ) != 0 )
         {
            if( HB_ISFIRSTIDCHAR( ch ) )
            {
               if( ch == 'H' && strncmp( pszCCode, "B_FUNC_STATIC", 13 ) == 0 )
               {
                  pszCCode += 13;
                  while( HB_ISSPACE( *pszCCode ) )
                     ++pszCCode;
                  if( *pszCCode == '(' )
                  {
                     ++pszCCode;
                     while( HB_ISSPACE( *pszCCode ) )
                        ++pszCCode;
                     if( HB_ISFIRSTIDCHAR( *pszCCode ) )
                     {
                        const char * pszName = pszCCode++;

                        while( HB_ISNEXTIDCHAR( *pszCCode ) )
                           ++pszCCode;
                        len = ( int ) ( pszCCode - pszName );
                        while( HB_ISSPACE( *pszCCode ) )
                           ++pszCCode;
                        if( *pszCCode == ')' )
                        {
                           char * name = hb_strndup( pszName, len );
                           hb_compFunctionMarkStatic( HB_COMP_PARAM, name );
                           hb_xfree( name );
                        }
                     }
                  }
               }
               while( HB_ISNEXTIDCHAR( *pszCCode ) )
                  ++pszCCode;
            }
            else if( ch == '/' && *pszCCode == '*' )
            {
               pszCCode++;
               while( *pszCCode )
               {
                  if( *pszCCode++ == '*' )
                  {
                     if( *pszCCode++ == '/' )
                        break;
                  }
               }
            }
            else if( ch == '/' && *pszCCode == '/' )
            {
               do
               {
                  ++pszCCode;
               }
               while( *pszCCode && *pszCCode != '\n' );
            }
            else if( ch == '"' || ch == '\'' )
            {
               while( *pszCCode )
               {
                  if( *pszCCode == '\\' )
                  {
                     pszCCode++;
                     if( *pszCCode )
                        pszCCode++;
                  }
                  else if( *pszCCode++ == ch )
                     break;
               }
            }
         }
      }
      pInline = pInline->pNext;
   }
}

static void hb_compGenCStdHeaders( HB_COMP_DECL, FILE * yyc, HB_BOOL fHbInLine )
{
   fprintf( yyc, "#include \"hbvmpub.h\"\n" );

   if( HB_COMP_PARAM->iGenCOutput != HB_COMPGENC_COMPACT )
      fprintf( yyc, "#include \"hbpcode.h\"\n" );

   fprintf( yyc, "#include \"hbinit.h\"\n" );

   if( HB_COMP_PARAM->iGenCOutput == HB_COMPGENC_REALCODE )
      fprintf( yyc, "#include \"hbxvm.h\"\n" );

   if( fHbInLine )
   {
      fprintf( yyc, "#include \"hbapi.h\"\n" );
      fprintf( yyc, "#include \"hbstack.h\"\n" );
      fprintf( yyc, "#include \"hbapierr.h\"\n" );
      fprintf( yyc, "#include \"hbapiitm.h\"\n" );
      fprintf( yyc, "#include \"hbvm.h\"\n" );
      fprintf( yyc, "#include \"hbapicls.h\"\n" );
      fprintf( yyc, "#include \"hboo.ch\"\n" );
   }
}

static void hb_compFuncUsed( HB_COMP_DECL, PHB_HSYMBOL pSym )
{
   if( ( pSym->cScope & HB_FS_USED ) == 0 )
      hb_compGenWarning( HB_COMP_PARAM, hb_comp_szWarnings, 'W', HB_COMP_WARN_STATIC_FUNC_UNUSED, pSym->szName, NULL );
}

void hb_compGenCCode( HB_COMP_DECL, PHB_FNAME pFileName )       /* generates the C language output */
{
   char        szFileName[ HB_PATH_MAX ];
   PHB_HSYMBOL pSym;
   PFUNCTION   pFunc;
   PHB_HINLINE pInline;
   FILE *      yyc; /* file handle for C output */
   HB_BOOL     fHasHbInline = HB_FALSE;
   int         iFuncSuffix;

   hb_fsFNameMerge( szFileName, pFileName );
   if( ! pFileName->szExtension )
      pFileName->szExtension = ".c";
   hb_fsFNameMerge( szFileName, pFileName );

   yyc = hb_fopen( szFileName, "w" );
   if( ! yyc )
   {
      hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! HB_COMP_PARAM->fQuiet )
   {
      char buffer[ 80 + HB_PATH_MAX - 1 ];
      hb_snprintf( buffer, sizeof( buffer ),
                   "Generating C source output to \'%s\'... ", szFileName );
      hb_compOutStd( HB_COMP_PARAM, buffer );
   }

   {
      char * szCmp = hb_verCompiler();
      char * szHrb = hb_verHarbour();

      fprintf( yyc, "/*\n * %s\n", szHrb );
      fprintf( yyc, " * %s\n", szCmp );
      fprintf( yyc, " * Generated C source from \"%s\"\n */\n\n", HB_COMP_PARAM->szFile );

      hb_xfree( szCmp );
      hb_xfree( szHrb );
   }

   pFunc = HB_COMP_PARAM->functions.pFirst;
   while( pFunc &&
          ( ( pFunc->funFlags & FUN_FILE_DECL ) != 0 ||
            pFunc == HB_COMP_PARAM->pInitFunc ||
            pFunc == HB_COMP_PARAM->pLineFunc ) )
      pFunc = pFunc->pNext;

   if( pFunc )
   {
      hb_compDumpFindCFunc( HB_COMP_PARAM );

      pInline = HB_COMP_PARAM->inlines.pFirst;
      while( pInline )
      {
         if( pInline->szName )
         {
            fHasHbInline = HB_TRUE;
            break;
         }
         pInline = pInline->pNext;
      }

      hb_compGenCStdHeaders( HB_COMP_PARAM, yyc, fHasHbInline );

      fprintf( yyc, "\n\n" );

      /* write functions prototypes */
      pSym = HB_COMP_PARAM->symbols.pFirst;
      while( pSym )
      {
         if( pSym->iFunc )
         {
            if( pSym->szName[ 0 ] == '(' )
            {
               fprintf( yyc, "HB_FUNC_INIT%s();\n",
                        ! memcmp( pSym->szName + 1, "_INITLINES", 10 ) ?
                        "LINES" : "STATICS" );
            }
            else if( pSym->cScope & HB_FS_LOCAL ) /* is it a function defined in this module */
            {
               iFuncSuffix = pSym->pFunc ? pSym->pFunc->iFuncSuffix : 0;
               if( pSym->cScope & HB_FS_INIT )
                  hb_compGenCFunc( yyc, "HB_FUNC_INIT( %s );\n", pSym->szName, HB_TRUE, iFuncSuffix );
               else if( pSym->cScope & HB_FS_EXIT )
                  hb_compGenCFunc( yyc, "HB_FUNC_EXIT( %s );\n", pSym->szName, HB_TRUE, iFuncSuffix );
               else if( pSym->cScope & HB_FS_STATIC )
               {
                  hb_compGenCFunc( yyc, "HB_FUNC_STATIC( %s );\n", pSym->szName, HB_FALSE, iFuncSuffix );
                  hb_compFuncUsed( HB_COMP_PARAM, pSym );
               }
               else
                  hb_compGenCFunc( yyc, "HB_FUNC( %s );\n", pSym->szName, HB_FALSE, iFuncSuffix );
            }
            else if( ( pSym->cScope & HB_FS_DEFERRED ) == 0 ) /* it's not a function declared as dynamic */
               hb_compGenCFunc( yyc, "HB_FUNC_EXTERN( %s );\n", pSym->szName, HB_FALSE, 0 );
         }
         pSym = pSym->pNext;
      }

      /* writes the symbol table */
      /* Generate the wrapper that will initialize local symbol table
       */
      hb_strncpyUpper( szFileName, pFileName->szName, sizeof( szFileName ) - 1 );
      /* replace non ID characters in name of local symbol table by '_' */
      {
         int iLen = ( int ) strlen( szFileName ), i;

         for( i = 0; i < iLen; i++ )
         {
            char c = szFileName[ i ];
            if( ! HB_ISNEXTIDCHAR( c ) )
               szFileName[ i ] = '_';
         }
      }
      fprintf( yyc, "\n\nHB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_%s%s )\n", HB_COMP_PARAM->szPrefix, szFileName );

      pSym = HB_COMP_PARAM->symbols.pFirst;
      while( pSym )
      {
         if( pSym->szName[ 0 ] == '(' )
         {
            /* Since the normal function cannot be INIT and EXIT at the same time
             * we are using these two bits to mark the special function used to
             * initialize static variables or debugging info about valid stop lines
             */
            fprintf( yyc, "{ \"%s\", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INIT%s}, NULL }",
                     pSym->szName, ! memcmp( pSym->szName + 1, "_INITLINES", 10 ) ?
                     "LINES" : "STATICS" ); /* NOTE: "hb_" intentionally in lower case */
         }
         else
         {
            fprintf( yyc, "{ \"%s\", {", pSym->szName );

            if( pSym->cScope & HB_FS_STATIC )
               fprintf( yyc, "HB_FS_STATIC" );
            else if( pSym->cScope & HB_FS_INIT )
               fprintf( yyc, "HB_FS_INIT" );
            else if( pSym->cScope & HB_FS_EXIT )
               fprintf( yyc, "HB_FS_EXIT" );
            else
               fprintf( yyc, "HB_FS_PUBLIC" );

            if( pSym->cScope & VS_MEMVAR )
               fprintf( yyc, " | HB_FS_MEMVAR" );

            if( pSym->cScope & HB_FS_MESSAGE )
               fprintf( yyc, " | HB_FS_MESSAGE" );

            if( ( pSym->cScope & HB_FS_FIRST ) && ( ! HB_COMP_PARAM->fNoStartUp ) )
               fprintf( yyc, " | HB_FS_FIRST" );

            /* specify the function address if it is a defined function or an
               external called function */
            if( pSym->cScope & HB_FS_LOCAL ) /* is it a function defined in this module */
            {
               fprintf( yyc, " | HB_FS_LOCAL" );

               iFuncSuffix = pSym->pFunc ? pSym->pFunc->iFuncSuffix : 0;
               if( pSym->cScope & HB_FS_INIT )
                  hb_compGenCFunc( yyc, "}, {HB_INIT_FUNCNAME( %s )}, NULL }", pSym->szName, HB_TRUE, iFuncSuffix );
               else if( pSym->cScope & HB_FS_EXIT )
                  hb_compGenCFunc( yyc, "}, {HB_EXIT_FUNCNAME( %s )}, NULL }", pSym->szName, HB_TRUE, iFuncSuffix );
               else
                  hb_compGenCFunc( yyc, "}, {HB_FUNCNAME( %s )}, NULL }", pSym->szName, HB_FALSE, iFuncSuffix );
            }
            else if( pSym->cScope & HB_FS_DEFERRED ) /* is it a function declared as dynamic */
               fprintf( yyc, " | HB_FS_DEFERRED}, {NULL}, NULL }" );
            else if( pSym->iFunc )                   /* is it a function called from this module */
               hb_compGenCFunc( yyc, "}, {HB_FUNCNAME( %s )}, NULL }", pSym->szName, HB_FALSE, 0 );
            else
               fprintf( yyc, "}, {NULL}, NULL }" );   /* memvar | alias | message */
         }

         if( pSym != HB_COMP_PARAM->symbols.pLast )
            fprintf( yyc, ",\n" );

         pSym = pSym->pNext;
      }

      hb_writeEndInit( HB_COMP_PARAM, yyc, szFileName, HB_COMP_PARAM->szFile );

      /* Generate functions data
       */
      pFunc = HB_COMP_PARAM->functions.pFirst;
      while( pFunc )
      {
         if( ( pFunc->funFlags & FUN_FILE_DECL ) == 0 )
         {
            /* Is it _STATICS$ - static initialization function */
            if( pFunc == HB_COMP_PARAM->pInitFunc )
               fprintf( yyc, "HB_FUNC_INITSTATICS()\n" );
            /* Is it an (_INITLINES) function */
            else if( pFunc == HB_COMP_PARAM->pLineFunc )
               fprintf( yyc, "HB_FUNC_INITLINES()\n" );
            /* Is it an INIT FUNCTION/PROCEDURE */
            else if( pFunc->cScope & HB_FS_INIT )
               hb_compGenCFunc( yyc, "HB_FUNC_INIT( %s )\n", pFunc->szName, HB_TRUE, pFunc->iFuncSuffix );
            /* Is it an EXIT FUNCTION/PROCEDURE */
            else if( pFunc->cScope & HB_FS_EXIT )
               hb_compGenCFunc( yyc, "HB_FUNC_EXIT( %s )\n", pFunc->szName, HB_TRUE, pFunc->iFuncSuffix );
            /* Is it a STATIC FUNCTION/PROCEDURE */
            else if( pFunc->cScope & HB_FS_STATIC )
               hb_compGenCFunc( yyc, "HB_FUNC_STATIC( %s )\n", pFunc->szName, HB_FALSE, pFunc->iFuncSuffix );
            else /* Then it must be PUBLIC FUNCTION/PROCEDURE */
               hb_compGenCFunc( yyc, "HB_FUNC( %s )\n", pFunc->szName, HB_FALSE, pFunc->iFuncSuffix );

            if( HB_COMP_PARAM->iGenCOutput == HB_COMPGENC_REALCODE )
               hb_compGenCRealCode( HB_COMP_PARAM, pFunc, yyc );
            else
            {
               if( HB_COMP_PARAM->iGenCOutput == HB_COMPGENC_COMPACT )
                  hb_compGenCCompact( pFunc, yyc );
               else
                  hb_compGenCReadable( HB_COMP_PARAM, pFunc, yyc );
            }
            fprintf( yyc, "\n" );
         }
         pFunc = pFunc->pNext;
      }

      /* Generate C inline functions
       */
      pInline = HB_COMP_PARAM->inlines.pFirst;
      while( pInline )
      {
         if( pInline->pCode )
         {
            fprintf( yyc, "#line %i ", pInline->iLine );
            hb_compGenCString( yyc, ( const HB_BYTE * ) pInline->szFileName,
                               strlen( pInline->szFileName ) );
            fprintf( yyc, "\n" );

            if( pInline->szName )
               hb_compGenCFunc( yyc, "HB_FUNC_STATIC( %s )\n", pInline->szName, HB_FALSE, 0 );

            fprintf( yyc, "%s", pInline->pCode );
         }
         pInline = pInline->pNext;
      }
   }
   else
   {
      pInline = HB_COMP_PARAM->inlines.pFirst;
      while( pInline )
      {
         if( pInline->pCode )
         {
            if( ! fHasHbInline )
            {
               hb_compGenCStdHeaders( HB_COMP_PARAM, yyc, HB_FALSE );
               fHasHbInline = HB_TRUE;
            }
            fprintf( yyc, "#line %i ", pInline->iLine );
            hb_compGenCString( yyc, ( const HB_BYTE * ) pInline->szFileName,
                               strlen( pInline->szFileName ) );
            fprintf( yyc, "\n" );

            if( pInline->szName )
               hb_compGenCFunc( yyc, "HB_FUNC_STATIC( %s )\n", pInline->szName, HB_FALSE, 0 );

            fprintf( yyc, "%s", pInline->pCode );
         }
         pInline = pInline->pNext;
      }
      if( ! fHasHbInline )
         fprintf( yyc, "\n/* Empty source file */\n" );
   }

   fclose( yyc );

   if( ! HB_COMP_PARAM->fQuiet )
      hb_compOutStd( HB_COMP_PARAM, "Done.\n" );
}

static void hb_writeEndInit( HB_COMP_DECL, FILE * yyc, const char * szModulname, const char * szSourceFile )
{
/*
   HB_SYMBOL_UNUSED( szSourceFile );
   fprintf( yyc, "\nHB_INIT_SYMBOLS_END( hb_vm_SymbolInit_%s%s )\n\n",
                 HB_COMP_PARAM->szPrefix, szModulname );
 */
   fprintf( yyc,
            "\nHB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_%s%s, ",
            HB_COMP_PARAM->szPrefix, szModulname );
   hb_compGenCString( yyc, ( const HB_BYTE * ) szSourceFile, strlen( szSourceFile ) );
   fprintf( yyc, ", 0x%lx, 0x%04x )\n\n", 0L, HB_PCODE_VER );

   fprintf( yyc,
            "#if defined( HB_PRAGMA_STARTUP )\n"
            "   #pragma startup hb_vm_SymbolInit_%s%s\n"
            "#elif defined( HB_DATASEG_STARTUP )\n"
            "   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_%s%s )\n"
            "   #include \"hbiniseg.h\"\n"
            "#endif\n\n",
            HB_COMP_PARAM->szPrefix, szModulname,
            HB_COMP_PARAM->szPrefix, szModulname );
}

static void hb_compGenCFunc( FILE * yyc, const char * cDecor, const char * szName,
                             HB_BOOL fStrip, int iFuncSuffix )
{
   int i = 0;

   while( cDecor[ i ] )
   {
      if( cDecor[ i ] == '%' && cDecor[ i + 1 ] == 's' )
      {
         const char * tmp = szName;
         char         c;

         while( ( c = *tmp++ ) != 0 )
         {
            if( HB_ISNEXTIDCHAR( c ) )
               fputc( ( HB_UCHAR ) c, yyc );
            else if( ! fStrip || c != '$' || *tmp != 0 )
            {
               /* 'x' is used to force unique name and eliminate possible
                * collisions with other function names.
                */
               fprintf( yyc, "x%02x", ( HB_UCHAR ) c );
            }
         }
         if( iFuncSuffix )
            fprintf( yyc, "v%d", iFuncSuffix );
         i += 2;
      }
      else
      {
         fputc( ( HB_UCHAR ) cDecor[ i ], yyc );
         i++;
      }
   }
}

static void hb_compGenCByteStr( FILE * yyc, const HB_BYTE * pText, HB_SIZE nLen )
{
   HB_SIZE nPos;

   for( nPos = 0; nPos < nLen; nPos++ )
   {
      HB_BYTE uchr = ( HB_BYTE ) pText[ nPos ];
      /*
       * NOTE: After optimization some CHR(n) can be converted
       *    into a string containing nonprintable characters.
       *
       * TODO: add switch to use hexadecimal format "%#04x"
       */
      fprintf( yyc, ( uchr < ( HB_BYTE ) ' ' || uchr >= 127 || uchr == '\\' ||
                      uchr == '\'' ) ? "%i, " : "\'%c\', ", uchr );
   }
}

static void hb_compGenCLocalName( PFUNCTION pFunc, int iLocal, HB_SIZE nPCodePos, HB_GENC_INFO_PTR cargo )
{
   /* Variable with negative order are local variables
    * referenced in a codeblock -handle it with care
    */

   if( cargo->nEndBlockPos > nPCodePos )
   {
      /* we are accesing variables within a codeblock */
      /* the names of codeblock variable are lost     */
      if( iLocal < 0 )
         fprintf( cargo->yyc, "\t/* localvar%i */", -iLocal );
      else
         fprintf( cargo->yyc, "\t/* codeblockvar%i */", iLocal );
   }
   else
   {
      const char * szName = hb_compLocalVariableName( pFunc, ( HB_USHORT ) iLocal );

      if( szName )
         fprintf( cargo->yyc, "\t/* %s */", szName );
      else
         fprintf( cargo->yyc, "\t/* localvar%i */", iLocal );
   }
}

static void hb_compGenCStaticName( HB_USHORT uiStatic, HB_GENC_INFO_PTR cargo )
{
   const char * szName = hb_compStaticVariableName( cargo->HB_COMP_PARAM, uiStatic );

   if( szName )
      fprintf( cargo->yyc, "\t/* %s */", szName );
   else
      fprintf( cargo->yyc, "\t/* staticvar%hu */", uiStatic );
}

static HB_GENC_FUNC( hb_p_and )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_AND,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypush )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPUSH,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypushref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPUSHREF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_dec )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DEC,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraydim )
{
   fprintf( cargo->yyc, "\tHB_P_ARRAYDIM, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_divide )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DIVIDE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_do )
{
   fprintf( cargo->yyc, "\tHB_P_DO, %i, %i,\n",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_doshort )
{
   fprintf( cargo->yyc, "\tHB_P_DOSHORT, %i,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_duplicate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DUPLICATE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_duplunref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DUPLUNREF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushunref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHUNREF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_swap )
{
   fprintf( cargo->yyc, "\tHB_P_SWAP, %i,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_equal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_EQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_exactlyequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_EXACTLYEQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_endblock )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ENDBLOCK,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_endproc )
{
   if( nPCodePos + 1 == pFunc->nPCodePos )
      fprintf( cargo->yyc, "\tHB_P_ENDPROC\n" );
   else
      fprintf( cargo->yyc, "\tHB_P_ENDPROC,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_false )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FALSE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_fortest )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FORTEST,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_frame )
{
   fprintf( cargo->yyc, "\tHB_P_FRAME, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* locals, params */" );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_funcptr )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FUNCPTR,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_function )
{
   fprintf( cargo->yyc, "\tHB_P_FUNCTION, %i, %i,\n",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_functionshort )
{
   fprintf( cargo->yyc, "\tHB_P_FUNCTIONSHORT, %i,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_arraygen )
{
   fprintf( cargo->yyc, "\tHB_P_ARRAYGEN, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_hashgen )
{
   fprintf( cargo->yyc, "\tHB_P_HASHGEN, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_greater )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_GREATER,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_greaterequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_GREATEREQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_inc )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_INC,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_instring )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_INSTRING,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_jumpnear )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPNEAR, %i,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );

      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %05" HB_PFS "i) */", nOffset, ( HB_ISIZ ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_jump )
{
   fprintf( cargo->yyc, "\tHB_P_JUMP, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %05" HB_PFS "i) */", nOffset, ( HB_ISIZ ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFAR, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %08" HB_PFS "i) */", nOffset, ( HB_ISIZ ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_jumpfalsenear )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSENEAR, %i,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %05" HB_PFS "i) */", nOffset, ( HB_ISIZ ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_jumpfalse )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSE, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %05" HB_PFS "i) */", nOffset, ( HB_ISIZ ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfalsefar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSEFAR, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %08" HB_PFS "i) */", nOffset, ( HB_ISIZ ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_jumptruenear )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUENEAR, %i,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %05" HB_PFS "i) */", nOffset, ( HB_ISIZ ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_jumptrue )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUE, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %05" HB_PFS "i) */", nOffset, ( HB_ISIZ ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumptruefar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUEFAR, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %08" HB_PFS "i) */", nOffset, ( HB_ISIZ ) ( nPCodePos + nOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_less )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_LESS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_lessequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_LESSEQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_line )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05" HB_PFS "i */ ", nPCodePos );
   else
      fprintf( cargo->yyc, "\t" );
   fprintf( cargo->yyc, "HB_P_LINE, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_localname )
{
   HB_SIZE nStart = nPCodePos;

   fprintf( cargo->yyc, "\tHB_P_LOCALNAME, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + nPCodePos + 3 );
   fprintf( cargo->yyc, "\n" );
   nPCodePos += 3;
   while( pFunc->pCode[ nPCodePos ] )
   {
      char chr = pFunc->pCode[ nPCodePos++ ];
      if( chr == '\'' || chr == '\\' )
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return nPCodePos - nStart + 1;
}

static HB_GENC_FUNC( hb_p_macropop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPOP, %i,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropopaliased )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPOPALIASED, %i,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropush )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSH, %i,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHREF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macrodo )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACRODO, %i, %i,\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_macrofunc )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROFUNC, %i, %i,\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_macrosend )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROSEND, %i, %i,\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_macroarraygen )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROARRAYGEN, %i, %i,\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_macropushlist )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHLIST, %i,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushindex )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHINDEX,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macropushpare )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHPARE, %i,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushaliased )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHALIASED, %i,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macrosymbol )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROSYMBOL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macrotext )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROTEXT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_message )
{
   fprintf( cargo->yyc, "\tHB_P_MESSAGE, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_minus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MINUS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_modulename )
{
   HB_SIZE nStart = nPCodePos;

   fprintf( cargo->yyc, "\tHB_P_MODULENAME," );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + nPCodePos + 1 );
   fprintf( cargo->yyc, "\n" );
   nPCodePos++;
   while( pFunc->pCode[ nPCodePos ] )
   {
      char chr = pFunc->pCode[ nPCodePos++ ];
      if( chr == '\'' || chr == '\\' )
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return nPCodePos - nStart + 1;
}

static HB_GENC_FUNC( hb_p_modulus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MODULUS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_mult )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MULT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_negate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NEGATE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_not )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NOT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_notequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NOTEQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_or )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_OR,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_parameter )
{
   fprintf( cargo->yyc, "\tHB_P_PARAMETER, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_plus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PLUS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_popalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POPALIAS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_popaliasedfield )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDFIELD, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDFIELDNEAR, %i,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_popaliasedvar )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDVAR, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popfield )
{
   fprintf( cargo->yyc, "\tHB_P_POPFIELD, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_poplocal )
{
   fprintf( cargo->yyc, "\tHB_P_POPLOCAL, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      int iVar = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      hb_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_poplocalnear )
{
   fprintf( cargo->yyc, "\tHB_P_POPLOCALNEAR, %i,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      int iVar = ( signed char ) pFunc->pCode[ nPCodePos + 1 ];
      hb_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_popmemvar )
{
   fprintf( cargo->yyc, "\tHB_P_POPMEMVAR, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popstatic )
{
   fprintf( cargo->yyc, "\tHB_P_POPSTATIC, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      hb_compGenCStaticName( HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), cargo );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popvariable )
{
   fprintf( cargo->yyc, "\tHB_P_POPVARIABLE, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_power )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POWER,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHALIAS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushaliasedfield )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDFIELD, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDFIELDNEAR, %i,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushaliasedvar )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDVAR, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushblockshort )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHBLOCKSHORT, %i,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */",
               pFunc->pCode[ nPCodePos + 1 ] );
   fprintf( cargo->yyc, "\n" );

   if( cargo->nEndBlockPos < nPCodePos )
      cargo->nEndBlockPos = nPCodePos + pFunc->pCode[ nPCodePos + 1 ] - 1;

   return 2;
}

static HB_GENC_FUNC( hb_p_pushblock )
{
   HB_USHORT wVar, w;
   HB_SIZE   nStart = nPCodePos;

   fprintf( cargo->yyc, "\tHB_P_PUSHBLOCK, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */",
               HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );

   w = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 3 ] );
   fprintf( cargo->yyc, "\t%i, %i,",
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of local parameters (%i) */", w );
   fprintf( cargo->yyc, "\n" );

   wVar = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 5 ] );
   fprintf( cargo->yyc, "\t%i, %i,",
            pFunc->pCode[ nPCodePos + 5 ],
            pFunc->pCode[ nPCodePos + 6 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of local variables (%i) */", wVar );
   fprintf( cargo->yyc, "\n" );

   nPCodePos += 7;  /* codeblock size + number of parameters + number of local variables */
   /* create the table of referenced local variables */
   while( wVar-- )
   {
      fprintf( cargo->yyc, "\t%i, %i,",
               pFunc->pCode[ nPCodePos ],
               pFunc->pCode[ nPCodePos + 1 ] );
      /* NOTE:
       * When a codeblock is used to initialize a static variable
       * the names of local variables cannot be determined
       * because at the time of C code generation we don't know
       * in which function was defined this local variable
       */
      if( cargo->bVerbose && ( pFunc->cScope & HB_FS_INITEXIT ) != HB_FS_INITEXIT )
      {
         w = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos ] );
         hb_compGenCLocalName( pFunc, w, nPCodePos, cargo );
      }
      fprintf( cargo->yyc, "\n" );
      nPCodePos += 2;
   }

   if( cargo->nEndBlockPos < nStart )
      cargo->nEndBlockPos = nStart + HB_PCODE_MKUSHORT( &pFunc->pCode[ nStart + 1 ] ) - 1;

   return nPCodePos - nStart;
}

static HB_GENC_FUNC( hb_p_pushblocklarge )
{
   HB_USHORT wVar, w;
   HB_SIZE   nStart = nPCodePos;

   fprintf( cargo->yyc, "\tHB_P_PUSHBLOCKLARGE, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %lu */",
               HB_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );

   w = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 4 ] );
   fprintf( cargo->yyc, "\t%i, %i,",
            pFunc->pCode[ nPCodePos + 4 ],
            pFunc->pCode[ nPCodePos + 5 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of local parameters (%i) */", w );
   fprintf( cargo->yyc, "\n" );

   wVar = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 6 ] );
   fprintf( cargo->yyc, "\t%i, %i,",
            pFunc->pCode[ nPCodePos + 6 ],
            pFunc->pCode[ nPCodePos + 7 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of local variables (%i) */", wVar );
   fprintf( cargo->yyc, "\n" );

   nPCodePos += 8;  /* codeblock size + number of parameters + number of local variables */
   /* create the table of referenced local variables */
   while( wVar-- )
   {
      fprintf( cargo->yyc, "\t%i, %i,",
               pFunc->pCode[ nPCodePos ],
               pFunc->pCode[ nPCodePos + 1 ] );
      /* NOTE:
       * When a codeblock is used to initialize a static variable
       * the names of local variables cannot be determined
       * because at the time of C code generation we don't know
       * in which function was defined this local variable
       */
      if( cargo->bVerbose && ( pFunc->cScope & HB_FS_INITEXIT ) != HB_FS_INITEXIT )
      {
         w = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos ] );
         hb_compGenCLocalName( pFunc, w, nPCodePos, cargo );
      }
      fprintf( cargo->yyc, "\n" );
      nPCodePos += 2;
   }

   if( cargo->nEndBlockPos < nStart )
      cargo->nEndBlockPos = nStart + HB_PCODE_MKUINT24( &pFunc->pCode[ nStart + 1 ] ) - 1;

   return nPCodePos - nStart;
}

static HB_GENC_FUNC( hb_p_pushdouble )
{
   int i;

   fprintf( cargo->yyc, "\tHB_P_PUSHDOUBLE," );
   ++nPCodePos;
   for( i = 0; i < ( int ) ( sizeof( double ) + sizeof( HB_BYTE ) + sizeof( HB_BYTE ) ); ++i )
   {
      fprintf( cargo->yyc, " %i,", ( HB_UCHAR ) pFunc->pCode[ nPCodePos + i ] );
   }
   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %.*f, %d, %d */",
               ( HB_UCHAR ) pFunc->pCode[ nPCodePos + sizeof( double ) + sizeof( HB_BYTE ) ],
               HB_PCODE_MKDOUBLE( &pFunc->pCode[ nPCodePos ] ),
               ( HB_UCHAR ) pFunc->pCode[ nPCodePos + sizeof( double ) ],
               ( HB_UCHAR ) pFunc->pCode[ nPCodePos + sizeof( double ) + sizeof( HB_BYTE ) ] );
   }
   fprintf( cargo->yyc, "\n" );

   return sizeof( double ) + sizeof( HB_BYTE ) + sizeof( HB_BYTE ) + 1;
}

static HB_GENC_FUNC( hb_p_pushfield )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHFIELD, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushbyte )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHBYTE, %i,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", ( signed char ) pFunc->pCode[ nPCodePos + 1 ] );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushint )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHINT, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlocal )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCAL, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      int iVar = ( int ) HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      hb_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlocalnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALNEAR, %i,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      int iVar = ( signed char ) pFunc->pCode[ nPCodePos + 1 ];
      hb_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALREF, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      int iVar = ( int ) HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      hb_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlong )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLONG, %i, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %li */", HB_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_pushlonglong )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLONGLONG, %i, %i, %i, %i, %i, %i, %i, %i, ",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ],
            pFunc->pCode[ nPCodePos + 5 ],
            pFunc->pCode[ nPCodePos + 6 ],
            pFunc->pCode[ nPCodePos + 7 ],
            pFunc->pCode[ nPCodePos + 8 ] );
   if( cargo->bVerbose )
   {
#ifdef HB_LONG_LONG_OFF
      fprintf( cargo->yyc, "\t/* %lf */", HB_PCODE_MKLONGLONG( &pFunc->pCode[ nPCodePos + 1 ] ) );
#else
      char szBuf[ 24 ];
      fprintf( cargo->yyc, "\t/* %s */", hb_numToStr( szBuf, sizeof( szBuf ),
                                                      HB_PCODE_MKLONGLONG( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
#endif
   }
   fprintf( cargo->yyc, "\n" );

   return 9;
}

static HB_GENC_FUNC( hb_p_pushmemvar )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHMEMVAR, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushmemvarref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHMEMVARREF, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushnil )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHNIL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushself )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHSELF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushstatic )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSTATIC, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      hb_compGenCStaticName( HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), cargo );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushstaticref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSTATICREF, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      hb_compGenCStaticName( HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), cargo );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushstrshort )
{
   HB_USHORT wLen = pFunc->pCode[ nPCodePos + 1 ];

   fprintf( cargo->yyc, "\tHB_P_PUSHSTRSHORT, %i,", pFunc->pCode[ nPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", wLen );

   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      hb_compGenCByteStr( cargo->yyc, &pFunc->pCode[ nPCodePos + 2 ], wLen );
   }
   fprintf( cargo->yyc, "\n" );
   return wLen + 2;
}

static HB_GENC_FUNC( hb_p_pushstr )
{
   HB_USHORT wLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

   fprintf( cargo->yyc, "\tHB_P_PUSHSTR, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", wLen );

   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      hb_compGenCByteStr( cargo->yyc, &pFunc->pCode[ nPCodePos + 3 ], wLen );
   }
   fprintf( cargo->yyc, "\n" );
   return wLen + 3;
}

static HB_GENC_FUNC( hb_p_pushstrlarge )
{
   HB_SIZE nLen = HB_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] );

   fprintf( cargo->yyc, "\tHB_P_PUSHSTRLARGE, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %" HB_PFS "u */", nLen );

   if( nLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      hb_compGenCByteStr( cargo->yyc, &pFunc->pCode[ nPCodePos + 4 ], nLen );
   }
   fprintf( cargo->yyc, "\n" );
   return nLen + 4;
}

static HB_GENC_FUNC( hb_p_pushstrhidden )
{
   HB_USHORT wLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 2 ] );

   fprintf( cargo->yyc, "\tHB_P_PUSHSTRHIDDEN, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", wLen );

   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      hb_compGenCByteStr( cargo->yyc, &pFunc->pCode[ nPCodePos + 4 ], wLen );
   }
   fprintf( cargo->yyc, "\n" );
   return wLen + 4;
}

static HB_GENC_FUNC( hb_p_pushsym )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSYM, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushsymnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSYMNEAR, %i,",
            pFunc->pCode[ nPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, pFunc->pCode[ nPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushfuncsym )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHFUNCSYM, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushvariable )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHVARIABLE, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_retvalue )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_RETVALUE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_send )
{
   fprintf( cargo->yyc, "\tHB_P_SEND, %i, %i,\n",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_sendshort )
{
   fprintf( cargo->yyc, "\tHB_P_SENDSHORT, %i,\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushovarref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHOVARREF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_seqblock )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_SEQBLOCK,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_seqbegin )
{
   fprintf( cargo->yyc, "\tHB_P_SEQBEGIN, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %08" HB_PFS "i) */", nOffset, nPCodePos + nOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_seqend )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05" HB_PFS "i */ ", nPCodePos );
   else
      fprintf( cargo->yyc, "\t" );
   fprintf( cargo->yyc, "HB_P_SEQEND, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %08" HB_PFS "i) */", nOffset, nPCodePos + nOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_seqrecover )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_SEQRECOVER,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_seqalways )
{
   fprintf( cargo->yyc, "\tHB_P_SEQALWAYS, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %08" HB_PFS "i) */", nOffset, nPCodePos + nOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_alwaysbegin )
{
   HB_SYMBOL_UNUSED( pFunc );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05" HB_PFS "i */ ", nPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "HB_P_ALWAYSBEGIN, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      HB_ISIZ nOffset = HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
      fprintf( cargo->yyc, "\t/* %" HB_PFS "i (abs: %08" HB_PFS "i) */", nOffset, nPCodePos + nOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_alwaysend )
{
   HB_SYMBOL_UNUSED( pFunc );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05" HB_PFS "i */ ", nPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "HB_P_ALWAYSEND,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_sframe )
{
   fprintf( cargo->yyc, "\tHB_P_SFRAME, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* symbol (_INITSTATICS) */" );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_statics )
{
   fprintf( cargo->yyc, "\tHB_P_STATICS, %i, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* symbol (_INITSTATICS), %i statics */", HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
   fprintf( cargo->yyc, "\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_staticname )
{
   HB_SIZE nStart = nPCodePos;

   fprintf( cargo->yyc, "\tHB_P_STATICNAME, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + nPCodePos + 4 );
   fprintf( cargo->yyc, "\n" );

   nPCodePos += 4;
   while( pFunc->pCode[ nPCodePos ] )
   {
      char chr = pFunc->pCode[ nPCodePos++ ];
      if( chr == '\'' || chr == '\\' )
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return nPCodePos - nStart + 1;
}

static HB_GENC_FUNC( hb_p_threadstatics )
{
   HB_USHORT w = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), u;

   fprintf( cargo->yyc, "\tHB_P_THREADSTATICS, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of thread static variables: %i */", w );
   fprintf( cargo->yyc, "\n" );

   nPCodePos += 3;
   for( u = 0; u < w; ++u )
   {
      fprintf( cargo->yyc, "\t%i, %i,",
               pFunc->pCode[ nPCodePos ],
               pFunc->pCode[ nPCodePos + 1 ] );
      if( cargo->bVerbose )
         hb_compGenCStaticName( HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos ] ), cargo );
      fprintf( cargo->yyc, "\n" );
      nPCodePos += 2;
   }

   return ( ( HB_SIZE ) w << 1 ) + 3;
}

static HB_GENC_FUNC( hb_p_swapalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_SWAPALIAS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_true )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_TRUE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_one )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ONE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_zero )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ZERO,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_noop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NOOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_dummy )
{
   HB_SYMBOL_UNUSED( cargo );
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumstart )
{
   fprintf( cargo->yyc, "\tHB_P_ENUMSTART, %i, %i,\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_enumnext )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );
   fprintf( cargo->yyc, "\tHB_P_ENUMNEXT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumprev )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );
   fprintf( cargo->yyc, "\tHB_P_ENUMPREV,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumend )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );
   fprintf( cargo->yyc, "\tHB_P_ENUMEND,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_switch )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05" HB_PFS "i */ ", nPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "HB_P_SWITCH, %i, %i,", pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i*/", HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   }

   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushdate )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHDATE, %i, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ] );
   if( cargo->bVerbose )
   {
      int  year, month, day;
      char date[ 9 ];

      hb_dateDecode( HB_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ), &year, &month, &day );
      hb_dateStrPut( date, year, month, day );
      date[ 8 ] = '\0';
      fprintf( cargo->yyc, "\t/* %s */", date );
   }
   fprintf( cargo->yyc, "\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_pushtimestamp )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHTIMESTAMP, %i, %i, %i, %i, %i, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ],
            pFunc->pCode[ nPCodePos + 5 ],
            pFunc->pCode[ nPCodePos + 6 ],
            pFunc->pCode[ nPCodePos + 7 ],
            pFunc->pCode[ nPCodePos + 8 ] );
   if( cargo->bVerbose )
   {
      char timestamp[ 24 ];

      hb_timeStampStr( timestamp,
                       HB_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ),
                       HB_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 5 ] ) );
      fprintf( cargo->yyc, "\t/* %s */", timestamp );
   }
   fprintf( cargo->yyc, "\n" );

   return 9;
}

static HB_GENC_FUNC( hb_p_localnearaddint )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARADDINT, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( signed char ) pFunc->pCode[ nPCodePos + 1 ];
      hb_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
      fprintf( cargo->yyc, "/* %i */", HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 2 ] ) );
   }
   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_localaddint )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALADDINT, %i, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ],
            pFunc->pCode[ nPCodePos + 4 ] );

   if( cargo->bVerbose )
   {
      int iVar = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      hb_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
      fprintf( cargo->yyc, "/* %i */", HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
   }
   fprintf( cargo->yyc, "\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_localinc )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALINC, %i, %i,", pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      int iVar = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      hb_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_localdec )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALDEC, %i, %i,", pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      int iVar = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      hb_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_localincpush )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALINCPUSH, %i, %i,", pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      int iVar = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
      hb_compGenCLocalName( pFunc, iVar, nPCodePos, cargo );
   }
   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pluseqpop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PLUSEQPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_minuseqpop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MINUSEQPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_multeqpop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MULTEQPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_diveqpop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DIVEQPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_modeqpop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MODEQPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_expeqpop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_EXPEQPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_inceqpop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_INCEQPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_deceqpop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DECEQPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pluseq )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PLUSEQ,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_minuseq )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MINUSEQ,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_multeq )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MULTEQ,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_diveq )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DIVEQ,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_modeq )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MODEQ,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_expeq )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_EXPEQ,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_inceq )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_INCEQ,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_deceq )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DECEQ,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_withobjectstart )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_WITHOBJECTSTART,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_withobjectmessage )
{
   fprintf( cargo->yyc, "\tHB_P_WITHOBJECTMESSAGE, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolName( cargo->HB_COMP_PARAM, HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_withobjectend )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_WITHOBJECTEND,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_vframe )
{
   fprintf( cargo->yyc, "\tHB_P_VFRAME, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* locals, params */" );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_largeframe )
{
   fprintf( cargo->yyc, "\tHB_P_LARGEFRAME, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* locals, params */" );
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_largevframe )
{
   fprintf( cargo->yyc, "\tHB_P_LARGEVFRAME, %i, %i, %i,",
            pFunc->pCode[ nPCodePos + 1 ],
            pFunc->pCode[ nPCodePos + 2 ],
            pFunc->pCode[ nPCodePos + 3 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* locals, params */" );
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_pushvparams )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHVPARAMS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushaparams )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHAPARAMS,\n" );
   return 1;
}

/* NOTE: The  order of functions have to match the order of opcodes
 *       mnemonics
 */
static const HB_GENC_FUNC_PTR s_verbose_table[] = {
   hb_p_and,
   hb_p_arraypush,
   hb_p_arraypop,
   hb_p_arraydim,
   hb_p_arraygen,
   hb_p_equal,
   hb_p_endblock,
   hb_p_endproc,
   hb_p_exactlyequal,
   hb_p_false,
   hb_p_fortest,
   hb_p_function,
   hb_p_functionshort,
   hb_p_frame,
   hb_p_funcptr,
   hb_p_greater,
   hb_p_greaterequal,
   hb_p_dec,
   hb_p_divide,
   hb_p_do,
   hb_p_doshort,
   hb_p_duplicate,
   hb_p_pushtimestamp,
   hb_p_inc,
   hb_p_instring,
   hb_p_jumpnear,
   hb_p_jump,
   hb_p_jumpfar,
   hb_p_jumpfalsenear,
   hb_p_jumpfalse,
   hb_p_jumpfalsefar,
   hb_p_jumptruenear,
   hb_p_jumptrue,
   hb_p_jumptruefar,
   hb_p_lessequal,
   hb_p_less,
   hb_p_line,
   hb_p_localname,
   hb_p_macropop,
   hb_p_macropopaliased,
   hb_p_macropush,
   hb_p_macroarraygen,
   hb_p_macropushlist,
   hb_p_macropushindex,
   hb_p_macropushpare,
   hb_p_macropushaliased,
   hb_p_macrosymbol,
   hb_p_macrotext,
   hb_p_message,
   hb_p_minus,
   hb_p_modulus,
   hb_p_modulename,
   /* start: pcodes generated by macro compiler */
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   /* end: */
   hb_p_mult,
   hb_p_negate,
   hb_p_noop,
   hb_p_not,
   hb_p_notequal,
   hb_p_or,
   hb_p_parameter,
   hb_p_plus,
   hb_p_pop,
   hb_p_popalias,
   hb_p_popaliasedfield,
   hb_p_popaliasedfieldnear,
   hb_p_popaliasedvar,
   hb_p_popfield,
   hb_p_poplocal,
   hb_p_poplocalnear,
   hb_p_popmemvar,
   hb_p_popstatic,
   hb_p_popvariable,
   hb_p_power,
   hb_p_pushalias,
   hb_p_pushaliasedfield,
   hb_p_pushaliasedfieldnear,
   hb_p_pushaliasedvar,
   hb_p_pushblock,
   hb_p_pushblockshort,
   hb_p_pushfield,
   hb_p_pushbyte,
   hb_p_pushint,
   hb_p_pushlocal,
   hb_p_pushlocalnear,
   hb_p_pushlocalref,
   hb_p_pushlong,
   hb_p_pushmemvar,
   hb_p_pushmemvarref,
   hb_p_pushnil,
   hb_p_pushdouble,
   hb_p_pushself,
   hb_p_pushstatic,
   hb_p_pushstaticref,
   hb_p_pushstr,
   hb_p_pushstrshort,
   hb_p_pushsym,
   hb_p_pushsymnear,
   hb_p_pushvariable,
   hb_p_retvalue,
   hb_p_send,
   hb_p_sendshort,
   hb_p_seqbegin,
   hb_p_seqend,
   hb_p_seqrecover,
   hb_p_sframe,
   hb_p_statics,
   hb_p_staticname,
   hb_p_swapalias,
   hb_p_true,
   hb_p_zero,
   hb_p_one,
   hb_p_macrofunc,
   hb_p_macrodo,
   /* start: more pcodes generated by macro compiler */
   hb_p_dummy,
   /* end: */
   hb_p_localnearaddint,
   hb_p_macropushref,
   hb_p_pushlonglong,
   hb_p_enumstart,
   hb_p_enumnext,
   hb_p_enumprev,
   hb_p_enumend,
   hb_p_switch,
   hb_p_pushdate,
   /* optimalization of inlined math operations (+=, -= */
   hb_p_pluseqpop,
   hb_p_minuseqpop,
   hb_p_multeqpop,
   hb_p_diveqpop,
   hb_p_pluseq,
   hb_p_minuseq,
   hb_p_multeq,
   hb_p_diveq,
   hb_p_withobjectstart,
   hb_p_withobjectmessage,
   hb_p_withobjectend,
   hb_p_macrosend,
   hb_p_pushovarref,
   hb_p_arraypushref,
   hb_p_vframe,
   hb_p_largeframe,
   hb_p_largevframe,
   hb_p_pushstrhidden,
   hb_p_localaddint,
   hb_p_modeqpop,
   hb_p_expeqpop,
   hb_p_modeq,
   hb_p_expeq,
   hb_p_duplunref,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_pushblocklarge,
   hb_p_pushstrlarge,
   hb_p_swap,
   hb_p_pushvparams,
   hb_p_pushunref,
   hb_p_seqalways,
   hb_p_alwaysbegin,
   hb_p_alwaysend,
   hb_p_deceqpop,
   hb_p_inceqpop,
   hb_p_deceq,
   hb_p_inceq,
   hb_p_localdec,
   hb_p_localinc,
   hb_p_localincpush,
   hb_p_pushfuncsym,
   hb_p_hashgen,
   hb_p_seqblock,
   hb_p_threadstatics,
   hb_p_pushaparams
};

static void hb_compGenCReadable( HB_COMP_DECL, PFUNCTION pFunc, FILE * yyc )
{
   const HB_GENC_FUNC_PTR * pFuncTable = s_verbose_table;
   HB_GENC_INFO genc_info;

   /* Make sure that table is correct */
   assert( HB_P_LAST_PCODE == sizeof( s_verbose_table ) / sizeof( HB_GENC_FUNC_PTR ) );

   genc_info.HB_COMP_PARAM = HB_COMP_PARAM;
   genc_info.nEndBlockPos  = 0;
   genc_info.bVerbose      = ( HB_COMP_PARAM->iGenCOutput == HB_COMPGENC_VERBOSE );
   genc_info.yyc = yyc;

   fprintf( yyc, "{\n   static const HB_BYTE pcode[] =\n   {\n" );
   hb_compPCodeEval( pFunc, ( const HB_PCODE_FUNC_PTR * ) pFuncTable, ( void * ) &genc_info );

   if( genc_info.bVerbose )
      fprintf( yyc, "/* %05" HB_PFS "i */\n", pFunc->nPCodePos );
   fprintf( yyc, "   };\n\n" );
   fprintf( yyc, "   hb_vmExecute( pcode, symbols );\n}\n" );
}

static void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc )
{
   HB_SIZE nPCodePos = 0;
   int     nChar;

   fprintf( yyc, "{\n\tstatic const HB_BYTE pcode[] =\n\t{\n\t\t" );

   nChar = 0;
   while( nPCodePos < pFunc->nPCodePos )
   {
      ++nChar;

      if( nChar > 1 )
         fprintf( yyc, "," );

      if( nChar == 15 )
      {
         fprintf( yyc, "\n\t\t" );
         nChar = 1;
      }

      /* Displaying as decimal is more compact than hex */
      fprintf( yyc, "%d", ( int ) pFunc->pCode[ nPCodePos++ ] );
   }

   if( nChar != 0 )
      fprintf( yyc, "\n" );

   fprintf( yyc, "\t};\n\n" );
   fprintf( yyc, "\thb_vmExecute( pcode, symbols );\n}\n" );
}
