/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler parse errors & warnings messages
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
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

/* Table with parse errors */
const char * const hb_comp_szErrors[] =
{
   "Statement not allowed outside of procedure or function",
   "Redefinition of procedure or function '%s'",
   "Duplicate variable declaration '%s'",
   "%s declaration follows executable statement",
   "Outer codeblock variable '%s' is out of reach",
   "Invalid numeric format '.'",
   "Unterminated string '%s'",
   "Redefinition of predefined function %s as '%s'",
   "Illegal variable '%s' initializer '%s'",
   "ENDIF does not match IF",
   "ENDDO does not match WHILE",
   "ENDCASE does not match DO CASE",
   "NEXT does not match FOR",
   "ELSE does not match IF",
   "ELSEIF does not match IF",
   "Syntax error '%s'",
   "Unclosed control structure '%s'",
   "%s statement with no loop in sight",
   "Syntax error '%s' in '%s'",
   "Incomplete statement or unbalanced delimiters",
   "Incorrect number of arguments in %s %s",
   "Invalid lvalue '%s'",
   "Invalid use of '@' (pass by reference) '%s'",
   "Formal parameters already declared",
   "Invalid %s from within of SEQUENCE code",
   "Unterminated array index",
   "Could not allocate %s byte(s)",
   "Could not reallocate %s byte(s)",
   "Freeing a NULL memory pointer",
   "Syntax error \"%s at '%s'\"",
   "Jump offset too long",
   "Can't create output file '%s'",
   "Can't create preprocessed output file '%s'",
   "Bad command line option '%s'",
   "Bad command line parameter '%s'",
   "Invalid filename '%s'",
   "Mayhem in CASE handler",
   "Operation not supported for data type '%s'",
   "Invalid alias expression '%s'",
   "Invalid array index expression '%s'",
   "Bound error '%s'",
   "Macro of declared symbol '%s'",
   "Invalid selector '%s' in send",
   "ANNOUNCEd procedure '%s' must be a public symbol",
   "Jump PCode not found",
   "CASE or OTHERWISE does not match DO CASE",
   "Code block contains both macro and declared symbol references '%s'",
   "GET contains complex macro",
   "Unterminated inline block in function '%s'",
   "Too many inline blocks %s",
   "Inline C requires C output generation, use -gc[n]",
   "Too many local variables [%s] or parameters [%s]",
   "Too many enumerate variables in FOR EACH loop",
   "Incorrect number of enumerate variables",
   "CASE requires either numeric or string constant",
   "String too long for SWITCH",
   "Invalid date constant '%s'",
   "Invalid timestamp constant '%s'",
   "Memory buffer overflow",
   "Memory corruption detected",
   "Implicit send operator with no WITH OBJECT in sight",
   "Input buffer overflow",
   "Unsupported output language option",
   "String too long",
   "Code block size too big",
   "%s not declared with variable number of parameters",
   "Can't find %s file",
   "Invalid ALWAYS after %s in RECOVER code",
   "File write error",
   "Duplicate case value",
   /* Some historical, funny sounding error messages from original CA-Cl*pper.
      They serve no purpose whatsoever. [vszakats] */
   "END wreaks terrible vengeance on control stack",
   "Control level closure leaves gaping wound in control stack",
   "Ford Maverick error number",
   "Something terrible has happened"
};

/* Table with parse warnings */
/* NOTE: The first character stores the warning's level that triggers this
 * warning. The warning's level is set by -w<n> command line option.
 */
const char * const hb_comp_szWarnings[] =
{
   "1Ambiguous reference '%s'",
   "1Ambiguous reference, assuming memvar '%s'",
   "2Variable '%s' declared but not used in function '%s'",
   "2Codeblock parameter '%s' declared but not used in function '%s'",
   "1RETURN statement with no return value in function",
   "1Procedure returns value",
   "1Function '%s' does not end with RETURN statement",
   "3Incompatible type in assignment to '%s', expected '%s'",
   "3Incompatible operand type '%s', expected '%s'",
   "3Incompatible operand types '%s' and '%s'",
   "4Suspicious type in assignment to '%s', expected '%s'",
   "4Suspicious operand type 'unknown', expected '%s'",
   "3Can't use array index with non-array",
   "3Incompatible return type '%s', expected '%s'",
   "4Suspicious return type '%s', expected '%s'",
   "3Invalid number of parameters %s, expected %s",
   "3Incompatible parameter '%s', expected '%s'",
   "4Suspicious parameter '%s', expected '%s'",
   "3Duplicate declaration of %s '%s'",
   "3Function '%s' conflicting with its declaration",
   "3Variable '%s' used but never initialized",
   "3Value of Variable '%s' never used",
   "3Incompatible type in assignment to declared array element expected '%s'",
   "4Suspicious type in assignment to declared array element expected '%s'",
   "3Class '%s' not known in declaration of '%s'",
   "3Message '%s' not known in class '%s'",
   "1Meaningless use of expression '%s'",
   "2Unreachable code",
   "1Redundant 'ANNOUNCE %s' statement ignored",
   "1Duplicate variable '%s' in nested FOR loop",
   "1Invalid variable '%s' for enumerator message",
   "3Variable '%s' is assigned but not used in function '%s'",
   "3Variable '%s' is never assigned in function '%s'",
   "2STATIC Function '%s' defined but never used"
};

static void hb_compDispMessage( HB_COMP_DECL, char cPrefix, int iValue,
                                const char * szText,
                                const char * szPar1, const char * szPar2 )
{
   HB_COMP_PARAM->outMsgFunc( HB_COMP_PARAM, HB_COMP_PARAM->iErrorFmt,
                              HB_COMP_PARAM->currLine, HB_COMP_PARAM->currModule,
                              cPrefix, iValue, szText, szPar1, szPar2 );
}

void hb_compGenError( HB_COMP_DECL, const char * const szErrors[],
                      char cPrefix, int iError,
                      const char * szError1, const char * szError2 )
{
   if( ! HB_COMP_PARAM->fExit && ( cPrefix == 'F' || ! HB_COMP_PARAM->fError ) )
   {
      PHB_HFUNC pFunc = HB_COMP_PARAM->functions.pLast;

      hb_compDispMessage( HB_COMP_PARAM, cPrefix, iError,
                          szErrors[ iError - 1 ], szError1, szError2 );

      HB_COMP_PARAM->iErrorCount++;
      HB_COMP_PARAM->fError = HB_TRUE;
      while( pFunc )
      {
         pFunc->bError = HB_TRUE;
         pFunc = pFunc->pOwner;
      }
      /* fatal error - exit immediately */
      if( cPrefix == 'F' )
         HB_COMP_PARAM->fExit = HB_TRUE;
   }
}

void hb_compGenWarning( HB_COMP_DECL, const char * const szWarnings[],
                        char cPrefix, int iWarning,
                        const char * szWarning1, const char * szWarning2 )
{
   const char * szText = szWarnings[ iWarning - 1 ];

   if( ! HB_COMP_PARAM->fExit && ( ( int ) ( szText[ 0 ] - '0' ) <= HB_COMP_PARAM->iWarnings ) )
   {
      hb_compDispMessage( HB_COMP_PARAM, cPrefix, iWarning,
                          szText + 1, szWarning1, szWarning2 );

      HB_COMP_PARAM->fAnyWarning = HB_TRUE;    /* report warnings at exit */
   }
}

PHB_EXPR hb_compErrorLValue( HB_COMP_DECL, PHB_EXPR pExpr )
{
   const char * szDesc = hb_compExprDescription( pExpr );

   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_INVALID_LVALUE, szDesc, NULL );
   return pExpr;
}

PHB_EXPR hb_compErrorIndex( HB_COMP_DECL, PHB_EXPR pExpr )
{
   const char * szDesc = hb_compExprDescription( pExpr );

   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_INVALID_INDEX, szDesc, NULL );
   return pExpr;
}

PHB_EXPR hb_compErrorBound( HB_COMP_DECL, PHB_EXPR pExpr )
{
   const char * szDesc = hb_compExprDescription( pExpr );

   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_INVALID_BOUND, szDesc, NULL );
   return pExpr;
}

PHB_EXPR hb_compErrorAlias( HB_COMP_DECL, PHB_EXPR pExpr )
{
   const char * szDesc = hb_compExprDescription( pExpr );

   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_INVALID_ALIAS, szDesc, NULL );
   return pExpr;
}

PHB_EXPR hb_compErrorStatic( HB_COMP_DECL, const char * szVarName, PHB_EXPR pExpr )
{
   const char * szDesc = hb_compExprDescription( pExpr );

   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_ILLEGAL_INIT, szVarName, szDesc );
   return pExpr;
}

PHB_EXPR hb_compWarnMeaningless( HB_COMP_DECL, PHB_EXPR pExpr )
{
   if( ! HB_COMP_PARAM->fMeaningful )
   {
      const char * szDesc = hb_compExprDescription( pExpr );
      hb_compGenWarning( HB_COMP_PARAM, hb_comp_szWarnings, 'W', HB_COMP_WARN_MEANINGLESS, szDesc, NULL );
   }
   return pExpr;
}

void hb_compErrorCodeblock( HB_COMP_DECL, const char * szBlock )
{
   HB_BOOL fError = HB_COMP_PARAM->fError;

   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_BLOCK, szBlock, NULL );
   HB_COMP_PARAM->fError = fError; /* restore error flag for this line */
}

void hb_compErrorMacro( HB_COMP_DECL, const char * szText )
{
   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_BAD_MACRO, szText, NULL );
}

PHB_EXPR hb_compErrorRefer( HB_COMP_DECL, PHB_EXPR pExpr, const char * szDesc )
{
   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_INVALID_REFER, szDesc, NULL );
   return pExpr;
}

void hb_compErrorVParams( HB_COMP_DECL, const char * szFuncOrBlock )
{
   hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_NOT_VPARAMS, szFuncOrBlock, NULL );
}
