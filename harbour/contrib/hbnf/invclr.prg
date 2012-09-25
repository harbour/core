/*
 * $Id$
 */

/*
 * File......: invclr.prg
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:44   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:00   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:30   GLENN
 * Nanforum Toolkit
 *
 */

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := iif(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := iif(<ParamN> == NIL,<DefN>,<ParamN>)]

#define     NULL    ""

FUNCTION FT_INVCLR(cDsrdColor)

   LOCAL cBackground, ;                 // The Background Color, New Foreground
         cForeground, ;                 // The Foreground Color, New Background
         cModifiers                     // Any Color Modifiers (+*)

   DEFAULT cDsrdColor TO SETCOLOR()
                                        // Remove Anything Past 1st Color
   cDsrdColor := LEFT(cDsrdColor, AT(",", cDsrdColor+",")-1)

                                        // Get Any Modifiers
   cModifiers := iif("*" $ cDsrdColor, "*", NULL) + ;
                 iif("+" $ cDsrdColor, "+", NULL)

                                        // Separate the Fore/Background Colors
   cForeground := ALLTRIM(LEFT(cDsrdColor,   AT("/", cDsrdColor) - 1))
   cBackground := ALLTRIM(SUBSTR(cDsrdColor, AT("/", cDsrdColor) + 1))

   RETURN STRTRAN(STRTRAN(cBackground, "+"), "*") + cModifiers + "/" + ;
          STRTRAN(STRTRAN(cForeground, "+"), "*")
