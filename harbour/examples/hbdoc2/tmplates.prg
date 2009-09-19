/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Document generator, templates
 *
 * Copyright 2009 April White <april users.sourceforge.net>
 * www - http://www.harbour-project.org
 *
 * Portions of this project are based on hbdoc
 *    Copyright 1999-2003 Luiz Rafael Culik <culikr@uol.com.br>
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#include "hbdoc2.ch"

/*
#xcommand TEXT TO VAR <var> => #pragma __stream|<var>:=%s
#xcommand TEXT INTO <v> => #pragma __text|<v>+=%s+HB_OSNEWLINE();<v>:=""

text into p_hsTemplates
this is line 1
this is line 2
endtext
? asc(substr(p_hsTemplates, -2))
?
*/

INIT PROCEDURE Templates()
   LOCAL aSubCategories := { ;
      "Application", ;
      "Array", ;
      "Classes", ;
      "Conversion", ;
      "Database", ;
      "Date/Time", ;
      "Environment", ;
      "Error", ;
      "Events", ;
      "Execute and execution", ; /* replace w/ "Environment"? */
      "Extend", ;
      "FileSys", ;
      "Fixed memory", ;
      "Garbage collector", ;
      "Hash table", ;
      "Idle states", ;
      "Internal", ;
      "Item", ;
      "Language and Nation", ;
      "Legacy", ;
      "Macro", ;
      "Math", ;
      "Objects", ;
      "Printer", ;
      "RDD", ;
      "Strings", ;
      "Terminal", ;
      "Undocumented", ;
      "User interface", ;
      "Variable management", ;
      "Virtual machine", ;
   }

   PUBLIC p_aCategories := { ;
                  { "API", AClone( aSubCategories ), Array( Len( aSubCategories ) ) }, ;
                  { "C level API", AClone( aSubCategories ), Array( Len( aSubCategories ) ) }, ;
                  { "C level API compatability", AClone( aSubCategories ), Array( Len( aSubCategories ) ) }, ;
                  { "Class", { ;
                        "", ;
                        "Access", ;
                        "Assign", ;
                        "Data", ;
                        "Method", ;
                        "Destructor", ;
                        "Var", ;
                        }, Array( 7 ) }, ;
                  { "Command", AClone( aSubCategories ), Array( Len( aSubCategories ) ) }, ;
                  /* { "Compile time errors", { {} } }, */ ;
                  { "Document", { "" }, Array( 1 ) }, ;
                  { "Run time errors", { "" }, Array( 1 ) }, ;
               }

   PUBLIC p_aCompliance := { ;
      "C", ;
      "C(array)", ;
      "C(menu)", ;
      "C52S", ;
      "C52U", ;
      "C53", ;
      "FS", ;
      "H", ;
      "NA", ;
      "XPP" ;
   }

   PUBLIC p_aPlatforms := { ;
      "All", ;
      "All(64K)", ;
      "All(GT)", ;
      "All(LFN)", ;
      "Linux(GT)", ;
      "OS2(GT)", ;
      "Win(GT)", ;
      "BSD", ;
      "DARWIN", ;
      "DOS", ;
      "HPUX", ;
      "LINUX", ;
      "OS2", ;
      "SUNOS", ;
      "Unix", ;
      "Win", ;
      "Win32", ;
      "Win64", ;
      "WinCE" ;
   }

   PUBLIC p_hsTemplates := HB_Hash()

   p_hsTemplates[ "orderby" ] := { ;
      "DOC", ;
      "TEMPLATE", ;
      "NAME", ;
      "CATEGORY", ;
      "SUBCATEGORY", ;
      "ONELINER", ;
      "SYNTAX", ;
      "ARGUMENTS", ;
      "RETURNS", ;
      "DESCRIPTION", ;
      "DATALINK", ;
      "DATANOLINK", ;
      "METHODSLINK", ;
      "METHODSNOLINK", ;
      "EXAMPLES", ;
      "TESTS", ;
      "STATUS", ;
      "COMPLIANCE", ;
      "PLATFORMS", ;
      "FILES", ;
      "SEEALSO", ;
      "END" ;
   }

   p_hsTemplates[ "Template" ] := HB_Hash( ;
      "DOC", { TPL_START }, ;
      "TEMPLATE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, { "Template" } }, ;
      "END", { TPL_END } ;
   )

   p_hsTemplates[ "Document" ] := HB_Hash( ;
      "DOC", { TPL_START }, ;
      "TEMPLATE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, { "Document" } }, ;
      "NAME", { TPL_REQUIRED }, ;
      "CATEGORY", { TPL_REQUIRED }, ;
      "SUBCATEGORY", { TPL_OPTIONAL }, ;
      "ONELINER", { TPL_REQUIRED }, ;
      "DESCRIPTION", { TPL_REQUIRED }, ;
      "PLATFORMS", { TPL_OPTIONAL + TPL_CONSTRAINTLIST, p_aPlatforms }, ;
      "FILES", { TPL_OPTIONAL }, ;
      "SEEALSO", { TPL_OPTIONAL }, ;
      "END", { TPL_END } ;
   )

   p_hsTemplates[ "Function" ] := HB_Hash( ;
      "DOC", { TPL_START }, ;
      "TEMPLATE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, { "Function" } }, ;
      "NAME", { TPL_REQUIRED }, ;
      "CATEGORY", { TPL_REQUIRED }, ;
      "SUBCATEGORY", { TPL_REQUIRED }, ;
      "ONELINER", { TPL_REQUIRED }, ;
      "SYNTAX", { TPL_OPTIONAL }, ;
      "ARGUMENTS", { TPL_OPTIONAL }, ;
      "RETURNS", { TPL_OPTIONAL }, ;
      "DESCRIPTION", { TPL_OPTIONAL }, ;
      "EXAMPLES", { TPL_PREFORMATTED + TPL_OPTIONAL }, ;
      "TESTS", { TPL_PREFORMATTED + TPL_OPTIONAL }, ;
      "STATUS", { TPL_OPTIONAL }, ;
      "COMPLIANCE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, p_aCompliance }, ;
      "PLATFORMS", { TPL_OPTIONAL + TPL_CONSTRAINTLIST, p_aPlatforms }, ;
      "FILES", { TPL_OPTIONAL }, ;
      "SEEALSO", { TPL_OPTIONAL }, ;
      "END", { TPL_END } ;
   )

   p_hsTemplates[ "Procedure" ] := HB_Hash( ;
      "DOC", { TPL_START }, ;
      "TEMPLATE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, { "Procedure" } }, ;
      "NAME", { TPL_REQUIRED }, ;
      "CATEGORY", { TPL_REQUIRED }, ;
      "SUBCATEGORY", { TPL_REQUIRED }, ;
      "ONELINER", { TPL_REQUIRED }, ;
      "SYNTAX", { TPL_OPTIONAL }, ;
      "ARGUMENTS", { TPL_OPTIONAL }, ;
      "DESCRIPTION", { TPL_OPTIONAL }, ;
      "EXAMPLES", { TPL_PREFORMATTED + TPL_OPTIONAL }, ;
      "TESTS", { TPL_PREFORMATTED + TPL_OPTIONAL }, ;
      "STATUS", { TPL_OPTIONAL }, ;
      "COMPLIANCE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, p_aCompliance }, ;
      "PLATFORMS", { TPL_OPTIONAL + TPL_CONSTRAINTLIST, p_aPlatforms }, ;
      "FILES", { TPL_OPTIONAL }, ;
      "SEEALSO", { TPL_OPTIONAL }, ;
      "END", { TPL_END } ;
   )

   p_hsTemplates[ "Command" ] := HB_Hash( ;
      "DOC", { TPL_START }, ;
      "TEMPLATE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, { "Command" } }, ;
      "NAME", { TPL_REQUIRED }, ;
      "CATEGORY", { TPL_REQUIRED }, ;
      "SUBCATEGORY", { TPL_REQUIRED }, ;
      "ONELINER", { TPL_REQUIRED }, ;
      "SYNTAX", { TPL_REQUIRED }, ;
      "ARGUMENTS", { TPL_REQUIRED }, ;
      "DESCRIPTION", { TPL_REQUIRED }, ;
      "EXAMPLES", { TPL_PREFORMATTED + TPL_OPTIONAL }, ;
      "TESTS", { TPL_PREFORMATTED + TPL_OPTIONAL }, ;
      "STATUS", { TPL_OPTIONAL }, ;
      "COMPLIANCE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, p_aCompliance }, ;
      "PLATFORMS", { TPL_OPTIONAL + TPL_CONSTRAINTLIST, p_aPlatforms }, ;
      "FILES", { TPL_OPTIONAL }, ;
      "SEEALSO", { TPL_OPTIONAL }, ;
      "END", { TPL_END } ;
   )

   p_hsTemplates[ "Class" ] := HB_Hash( ;
      "DOC", { TPL_START }, ;
      "TEMPLATE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, { "Class" } }, ;
      "NAME", { TPL_REQUIRED }, ;
      "CATEGORY", { TPL_REQUIRED }, ;
      "SUBCATEGORY", { TPL_REQUIRED }, ;
      "ONELINER", { TPL_REQUIRED }, ;
  /*     "CONSTRUCTOR", { TPL_REQUIRED }, */ ;
      "SYNTAX", { TPL_REQUIRED }, ;
      "ARGUMENTS", { TPL_REQUIRED }, ;
      "RETURNS", { TPL_REQUIRED }, ;
      "DESCRIPTION", { TPL_REQUIRED }, ;
      "DATALINK", { TPL_REQUIRED }, ;
      "DATANOLINK", { TPL_REQUIRED }, ;
      "METHODSLINK", { TPL_REQUIRED }, ;
      "METHODSNOLINK", { TPL_REQUIRED }, ;
      "EXAMPLES", { TPL_PREFORMATTED + TPL_OPTIONAL }, ;
      "TESTS", { TPL_PREFORMATTED + TPL_OPTIONAL }, ;
      "STATUS", { TPL_OPTIONAL }, ;
      "COMPLIANCE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, p_aCompliance }, ;
      "PLATFORMS", { TPL_OPTIONAL + TPL_CONSTRAINTLIST, p_aPlatforms }, ;
      "FILES", { TPL_OPTIONAL }, ;
      "SEEALSO", { TPL_OPTIONAL }, ;
      "END", { TPL_END } ;
   )

   p_hsTemplates[ "Class method" ] := HB_Hash( ;
      "DOC", { TPL_START }, ;
      "TEMPLATE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, { "Class method" } }, ;
      "NAME", { TPL_REQUIRED }, ;
      "CATEGORY", { TPL_REQUIRED }, ;
      "ONELINER", { TPL_REQUIRED }, ;
      "SYNTAX", { TPL_REQUIRED }, ;
      "ARGUMENTS", { TPL_REQUIRED }, ;
      "RETURNS", { TPL_REQUIRED }, ;
      "DESCRIPTION", { TPL_REQUIRED }, ;
      "END", { TPL_END } ;
   )

   //~ VAR
   //~ *METHOD
   //~ *DATA
   //~ ACCESS
   //~ ASSIGN
   //~ FRIEND

   p_hsTemplates[ "Class data" ] := HB_Hash( ;
      "DOC", { TPL_START }, ;
      "TEMPLATE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, { "Class data" } }, ;
      "NAME", { TPL_REQUIRED }, ;
  /*     "DATA", { TPL_REQUIRED }, */ ;
      "SYNTAX", { TPL_REQUIRED }, ;
      "CATEGORY", { TPL_REQUIRED }, ;
      "ONELINER", { TPL_REQUIRED }, ;
      "DESCRIPTION", { TPL_REQUIRED }, ;
      "END", { TPL_END } ;
   )

   p_hsTemplates[ "Run time error" ] := HB_Hash( ;
      "DOC", { TPL_START }, ;
      "TEMPLATE", { TPL_REQUIRED + TPL_CONSTRAINTLIST, { "Run time error" } }, ;
      "NAME", { TPL_REQUIRED }, ;
      "CATEGORY", { TPL_REQUIRED }, ;
      "ONELINER", { TPL_REQUIRED }, ;
      "DESCRIPTION", { TPL_REQUIRED }, ;
      "EXAMPLES", { TPL_PREFORMATTED + TPL_OPTIONAL }, ;
      "COMPLIANCE", { TPL_OPTIONAL + TPL_CONSTRAINTLIST, p_aCompliance }, ;
      "SEEALSO", { TPL_OPTIONAL }, ;
      "END", { TPL_END } ;
   )

   PUBLIC p_aConversionList := { ;
      38, "amp", ;
      34, "quot", ;
      60, "lt", ;
      62, "gt", ;
      129, "#129", ;
      130, "#130", ;
      131, "#131", ;
      132, "#132", ;
      133, "#133", ;
      134, "#134", ;
      135, "#135", ;
      136, "#136", ;
      137, "#137", ;
      138, "#138", ;
      139, "#139", ;
      140, "#140", ;
      141, "#141", ;
      142, "#142", ;
      143, "#143", ;
      144, "#144", ;
      145, "#145", ;
      146, "#146", ;
      147, "#147", ;
      148, "#148", ;
      149, "#149", ;
      150, "#150", ;
      151, "#151", ;
      152, "#152", ;
      153, "#153", ;
      154, "#154", ;
      155, "#155", ;
      156, "#156", ;
      157, "#157", ;
      158, "#158", ;
      159, "#159", ;
      160, "nbsp", ;
      161, "iexcl", ;
      162, "#162", ;
      163, "#163", ;
      164, "#164", ;
      165, "#165", ;
      166, "#166", ;
      167, "#167", ;
      168, "#168", ;
      169, "#169", ;
      170, "ordf", ;
      171, "#171", ;
      172, "#172", ;
      173, "#173", ;
      174, "#174", ;
      175, "#175", ;
      176, "#176", ;
      177, "#177", ;
      178, "#178", ;
      179, "#179", ;
      180, "#180", ;
      181, "#181", ;
      182, "#182", ;
      183, "#183", ;
      184, "#184", ;
      185, "#185", ;
      186, "ordm", ;
      187, "#187", ;
      188, "#188", ;
      189, "#189", ;
      190, "#190", ;
      191, "iquest", ;
      192, "Agrave", ;
      193, "Aacute", ;
      194, "Acirc", ;
      195, "Atilde", ;
      196, "Auml", ;
      197, "Aring", ;
      198, "AElig", ;
      199, "Ccedil", ;
      200, "Egrave", ;
      201, "Eacute", ;
      202, "Ecirc", ;
      203, "Euml", ;
      204, "Igrave", ;
      205, "Iacute", ;
      206, "Icirc", ;
      207, "Iuml", ;
      208, "ETH", ;
      209, "Ntilde", ;
      210, "Ograve", ;
      211, "Oacute", ;
      212, "Ocirc", ;
      213, "Otilde", ;
      214, "Ouml", ;
      215, "—", ;
      216, "Oslash", ;
      217, "Ugrave", ;
      218, "Uacute", ;
      219, "Ucirc", ;
      220, "Uuml", ;
      221, "Yacute", ;
      222, "THORN", ;
      223, "szlig", ;
      224, "#224", ;
      225, "aacute", ;
      226, "¢", ;
      227, "atilde", ;
      228, "auml", ;
      229, "aring", ;
      230, "aelig", ;
      231, "§", ;
      232, "¨", ;
      233, "©", ;
      234, "ª", ;
      235, "«", ;
      236, "igrave", ;
      237, "iacute", ;
      238, "®", ;
      239, "¯", ;
      240, "eth", ;
      241, "ntilde", ;
      242, "ograve", ;
      243, "oacute", ;
      244, "´", ;
      245, "otilde", ;
      246, "ouml", ;
      247, "·", ;
      248, "oslash", ;
      249, "¹", ;
      250, "uacute", ;
      251, "»", ;
      252, "¼", ;
      253, "yacute", ;
      254, "thorn", ;
      255, "yuml;" ;
   }

RETURN


PROCEDURE ShowTemplatesHelp( cTemplate )
   LOCAL idxTemplates, nFrom := 1, nTo := Len( p_hsTemplates )
   LOCAL cDelimiter := p_hsSwitches[ "DELIMITER" ]
   LOCAL aKeyValue
   LOCAL cSection
   LOCAL nFlags
   LOCAL nSections
   LOCAL idxSection

   IF .NOT. Empty( cTemplate ) .AND. cTemplate != "Template"
      IF HB_HHasKey( p_hsTemplates, cTemplate )
         nFrom := nTo := HB_HPos( p_hsTemplates, cTemplate )
      ELSE
         ShowHelp( "Unknown template '" + cTemplate + "'" )
         RETURN
      ENDIF
   ENDIF

   FOR idxTemplates := nFrom TO nTo
      aKeyValue := HB_HPairAt( p_hsTemplates, idxTemplates )
      IF aKeyValue[ 1 ] != "Template"
         AAdd( aKeyValue, {} )
         HB_HEval( aKeyValue[ 2 ], {|k,v| AAdd( ATail( aKeyValue ), { cDelimiter + k + cDelimiter, v[ 1 ], HB_AScan( p_hsTemplates[ "orderby" ], k ) } ) } )
         ASort( ATail( aKeyValue ), , , {|lv,rv| lv[ 3 ] < rv[ 3 ] } )
         nSections := Len( ATail( aKeyValue ) )

         FOR idxSection := 1 TO nSections
            cSection := ATail( aKeyValue )[ idxSection ][ 1 ]
            nFlags := ATail( aKeyValue )[ idxSection ][ 2 ]
            ShowSubHelp( IIf( idxSection == 1, "/", " " ) + "*  " + cSection, 1, 0 )
            IF cSection == cDelimiter + "TEMPLATE" + cDelimiter
               ShowSubHelp( " *       " + aKeyValue[ 1 ], 1, 0 )
            ELSEIF 1 < idxSection .AND. idxSection < nSections
               ShowSubHelp( " *       " + IIf( HB_BitAnd( nFlags, TPL_REQUIRED ) == TPL_REQUIRED, "<required>", "" ), 1, 0 )
            ELSEIF idxSection == nSections
               ShowSubHelp( " */", 1, 0 )
            ENDIF
         NEXT
         ShowSubHelp( "", 1, 0 )
      ENDIF
   NEXT

   RETURN

PROCEDURE ShowComplianceHelp()
   LOCAL idx

   FOR idx := 1 TO Len( p_aCompliance )
      ShowSubHelp( p_aCompliance[ idx ], 1, 0, idx )
      ShowSubHelp( Decode( "Compliance", NIL, p_aCompliance[ idx ] ), 1, 6, idx )
      OutStd( HB_OSNewLine() )
   NEXT

PROCEDURE ShowPlatformsHelp
   LOCAL idx

   FOR idx := 1 TO Len( p_aPlatforms )
      ShowSubHelp( p_aPlatforms[ idx ], 1, 0, idx )
      ShowSubHelp( Decode( "Platforms", NIL, p_aPlatforms[ idx ] ), 1, 6, idx )
      OutStd( HB_OSNewLine() )
   NEXT
