/*
 * $Id$
 */

#include "external.ch"

FUNCTION Main( cHRBFile, cPar1, cPar2, cPar3, cPar4, cPar5, cPar6, cPar7, cPar8, cPar9 )
   LOCAL xRetVal

   IF Empty( cHRBFile )
      ?? "Syntax: runner <hrbfile.hrb> [parameter]"
      ?
   ELSE
      xRetVal := __hrbRun( cHRBFile, cPar1, cPar2, cPar3, cPar4, cPar5, cPar6, cPar7, cPar8, cPar9 )
   ENDIF

RETURN xRetVal

