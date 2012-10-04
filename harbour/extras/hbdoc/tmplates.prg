/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Document generator, templates
 *
 * Copyright 2009 April White <april users.sourceforge.net>
 * www - http://harbour-project.org
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

#include "hbdoc.ch"

#include "hbclass.ch"

/* a class that will hold one entry */
CREATE CLASS Entry

   EXPORTED:

   CLASSDATA Fields AS ARRAY INIT { ;
      { "DOC",          "Doc" }, ;
      { "TEMPLATE",     "Template" }, ;
      { "NAME",         "" }, ;
      { "CATEGORY",     "Category" }, ;
      { "SUBCATEGORY",  "Sub category" }, ;
      { "ONELINER",     "" },;
      { "SYNTAX",       "Syntax" },;
      { "ARGUMENTS",    "Argument(s)" },;
      { "RETURNS",      "Returns" },;
      { "DESCRIPTION",  "Description" },;
      { "DATALINK",     "Data link" },;
      { "DATANOLINK",   "Data no link" },;
      { "METHODSLINK",  "Methods link" },;
      { "METHODSNOLINK","Methods no link" },;
      { "EXAMPLES",     "Example(s)" },;
      { "TESTS",        "Test(s)" },;
      { "STATUS",       "Status" },;      /* p_aStatus is the constraint list */
      { "COMPLIANCE",   "Compliance" },;  /* p_aCompliance is the constraint list */
      { "PLATFORMS",    "Platform(s)" },; /* p_aPlatforms is the constraint list */
      { "FILES",        "File(s)" },;
      { "SEEALSO",      "See also" }, ;
      { "END",          "End" } ;
   }

#define S TPL_START
#define E TPL_END
#define T TPL_TEMPLATE
#define R TPL_REQUIRED
#define O TPL_OPTIONAL
#define P TPL_PREFORMATTED
#define U TPL_OUTPUT
#define x 0

   // this is best viewed with a fixed-width font
   // the columns of this array correspond to the elements of Fields
   CLASSDATA Templates AS ARRAY INIT { ;
      { "Template"      , { S, T, x+U, x, O  , x+U, x+U, x+U, x+U, x+U, x+U, x+U, x+U, x+U, x  +U, x  +U, x+U, x+U, x+U, x+U, x+U, E } }, ;
      { "Document"      , { S, T, R+U, R, O+U, O+U, x+U, x+U, x+U, R+U, x+U, x+U, x+U, x+U, x  +U, x  +U, x+U, x+U, O+U, O+U, O+U, E } }, ;
      { "Function"      , { S, T, R+U, R, R  , O+U, O+U, O+U, O+U, O+U, x+U, x+U, x+U, x+U, P+O+U, P+O+U, O+U, O+U, O+U, O+U, O+U, E } }, ;
      { "C Function"    , { S, T, R+U, R, R  , O+U, O+U, O+U, O+U, O+U, x+U, x+U, x+U, x+U, P+O+U, P+O+U, O+U, O+U, O+U, O+U, O+U, E } }, ;
      { "Procedure"     , { S, T, R+U, R, R  , O+U, O+U, O+U,   x, O+U, x+U, x+U, x+U, x+U, P+O+U, P+O+U, O+U, O+U, O+U, O+U, O+U, E } }, ;
      { "Command"       , { S, T, R+U, R, R  , O+U, R+U, R+U, x+U, R+U, x+U, x+U, x+U, x+U, P+O+U, P+O+U, O+U, O+U, O+U, O+U, O+U, E } }, ;
      { "Class"         , { S, T, R+U, R, R  , O+U, R+U, R+U, R+U, R+U, O+U, O+U, O+U, O+U, P+O+U, P+O+U, O+U, O+U, O+U, O+U, O+U, E } }, ;
      { "Class method"  , { S, T, R+U, R, R  , O+U, R+U, R+U, R+U, R+U, x+U, x+U, x+U, x+U, P+O+U, x  +U, x+U, x+U, x+U, x+U, O+U, E } }, ;
      { "Class data"    , { S, T, R+U, R, R  , O+U, R+U, x+U, x+U, R+U, x+U, x+U, x+U, x+U, P+O+U, x  +U, x+U, x+U, x+U, x+U, O+U, E } }, ;
      { "Run time error", { S, T, R+U, R, x  , O+U, x+U, x+U, x+U, R+U, x+U, x+U, x+U, x+U, P+O+U, x  +U, x+U, O+U, x+U, x+U, O+U, E } }, ;
   }

#undef S
#undef E
#undef T
#undef R
#undef O
#undef P
#undef U
#undef x

   METHOD New( cType ) CONSTRUCTOR
   METHOD IsField( c, nType )
   METHOD IsTemplate( cType )
   METHOD SetTemplate( cTemplate )
   METHOD IsConstraint( cSectionName, cSection )
   METHOD IsComplete( cIncompleteFielsList )
   METHOD IsPreformatted( cField )
   METHOD IsRequired( cField )
   METHOD IsOptional( cField )
   METHOD IsOutput( cField )
   METHOD FieldName( cField )
   METHOD CategoryIndex( cCategory )
   METHOD SubcategoryIndex( cCategory, cSubcategory )

   DATA Group AS ARRAY
   DATA filename AS STRING
   DATA type_ AS STRING
   DATA sourcefile_ AS STRING
   DATA sourcefileversion_ AS STRING
   DATA uid_ AS STRING
   CLASSDATA uid__ AS INTEGER INIT 0
ENDCLASS

METHOD New( cType ) CLASS Entry
   ::uid_ := hb_ntos( ++::uid__ )
   IF ! __objHasData( self, self:Fields[ 1 ][ 1 ] )
      AEval( self:Fields, {| a | __objAddData( self, a[ 1 ] ) } )
   ENDIF
   IF cType != NIL
      self:Group := self:Templates[ hb_AScan( self:Templates, {| a | Upper( a[ 1 ] ) == Upper( cType ) } ) ][ 2 ]
   ENDIF
   RETURN self

METHOD IsField( c, nType ) CLASS Entry
   LOCAL idx
   LOCAL lResult

   IF ( lResult := ( idx := hb_AScan( self:Fields, {| a | Upper( a[ 1 ] ) == Upper( c ) } ) ) > 0 )
      IF self:Group[ idx ] == 0
         lResult := .F.
      ELSEIF nType != NIL .AND. hb_bitAnd( self:Group[ idx ], nType ) != nType
         lResult := .F.
      ELSE
      ENDIF
   ENDIF

   RETURN lResult

METHOD IsTemplate( cType ) CLASS Entry
   RETURN hb_AScan( self:Templates, {| a | Upper( a[ 1 ] ) == Upper( cType ) } ) > 0

METHOD SetTemplate( cTemplate ) CLASS Entry
   LOCAL aData := Array( Len( self:Fields ) )
   LOCAL idx

   self:Group := self:Templates[ hb_AScan( self:Templates, {| a | Upper( a[ 1 ] ) == Upper( cTemplate ) } ) ][ 2 ]
   FOR idx := 1 TO Len( aData )
      IF self:Fields[ idx ][ 1 ] == "TEMPLATE"
         aData[ idx ] := { self:Fields[ idx ][ 1 ], cTemplate }
      ELSE
         aData[ idx ] := { self:Fields[ idx ][ 1 ], iif( self:Group[ idx ] == TPL_REQUIRED, NIL, "" ) }
      ENDIF
   NEXT
   __objSetValueList( self, aData )
   RETURN self

METHOD IsConstraint( cSectionName, cSection ) CLASS Entry
   LOCAL lResult
   LOCAL idx := hb_AScan( self:Fields, {| a | a[ 1 ] == cSectionName } )

   IF hb_bitAnd( self:Group[ idx ], hb_bitAnd( TPL_REQUIRED, TPL_OPTIONAL ) ) == 0
      lResult := .T.
   ELSEIF Type( "p_a" + cSectionName ) == "A"
      lResult := hb_AScan( &( "p_a" + cSectionName ), cSection ) .OR. ;
                 hb_AScan( &( "p_a" + cSectionName ), Parse( ( cSection ), "," ) )
   ELSE
      lResult := .T.
   ENDIF

   RETURN lResult

METHOD IsComplete( cIncompleteFielsList ) CLASS Entry
   LOCAL lResult := .T.
   LOCAL idx

   cIncompleteFielsList := ""

   FOR idx := 1 TO Len( self:Fields )
      IF hb_bitAnd( self:Group[ idx ], TPL_REQUIRED ) != 0 .AND. Empty( self:&( self:Fields[ idx ][ 1 ] ) )
         cIncompleteFielsList += "," + self:Fields[ idx ][ 1 ]
         lResult := .F.
      ENDIF
   NEXT

   cIncompleteFielsList := SUBSTR( cIncompleteFielsList, 2 )

   RETURN lResult

METHOD IsPreformatted( cField ) CLASS Entry
   RETURN hb_bitAnd( self:Group[ hb_AScan( self:Fields, {| a | a[ 1 ] == cField } ) ], TPL_PREFORMATTED ) != 0

METHOD IsRequired( cField ) CLASS Entry
   RETURN hb_bitAnd( self:Group[ hb_AScan( self:Fields, {| a | a[ 1 ] == cField } ) ], TPL_REQUIRED ) != 0

METHOD IsOptional( cField ) CLASS Entry
   RETURN hb_bitAnd( self:Group[ hb_AScan( self:Fields, {| a | a[ 1 ] == cField } ) ], TPL_OPTIONAL ) != 0

METHOD IsOutput( cField ) CLASS Entry
   RETURN hb_bitAnd( self:Group[ hb_AScan( self:Fields, {| a | a[ 1 ] == cField } ) ], TPL_OUTPUT ) != 0

METHOD FieldName( cField ) CLASS Entry
   RETURN self:Fields[ hb_AScan( self:Fields, {| a | a[ 1 ] == cField } ) ][ 2 ]

METHOD CategoryIndex( cCategory ) CLASS Entry
   RETURN hb_AScan( p_aCategories, {| a | a[ 1 ] == cCategory } )

METHOD SubcategoryIndex( cCategory, cSubcategory ) CLASS Entry
   RETURN hb_AScan( p_aCategories[ ::CategoryIndex( cCategory ) ][ 2 ], cSubcategory, , , .T. )
   //~ RETURN hb_AScan( p_aCategories[ ::CategoryIndex( cCategory ) ][ 2 ], {| c | c == cSubcategory } )

PROCEDURE init_Templates()
   LOCAL idx
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
      "INET", ;
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
      { "Document", { "License", "Compiler", "" } }, ;
      { "API", AClone( aSubCategories ) }, ;
      { "C level API", AClone( aSubCategories ) }, ;
      { "C level API compatability", AClone( aSubCategories ) }, ;
      { "Class", { ;
            "", ;
            "Access", ;
            "Assign", ;
            "Constructor", ;
            "Data", ;
            "Definition", ;
            "Destructor", ;
            "Method", ;
            "Var", ;
            } }, ;
      { "Command", AClone( aSubCategories ) }, ;
      /* { "Compile time errors", { {} } }, */ ;
      { "Run time errors", { "" } }, ;
   }

   FOR idx := 1 TO Len( p_aCategories )
      IF ! Empty( p_aCategories[ idx ] )
         AAdd( p_aCategories[ idx ], Array( Len( p_aCategories[ idx ][ 2 ] ) ) ) // holder array of sub-category entries
         AAdd( p_aCategories[ idx ], "" ) // holder for sub-category file name
      ENDIF
   NEXT

   PUBLIC p_aCompliance := { ;
      { "",         "" }, ;
      { "C",        "This is CA-Cl*pper v5.2 compliant" }, ;
      { "C(array)", "This is CA-Cl*pper v5.2 compliant except that arrays in Harbour can have an unlimited number of elements" }, ;
      { "C(menu)",  "This is CA-Cl*pper v5.2 compliant except that menus (internally arrays) in Harbour can have an unlimited number of elements" }, ;
      { "C(arrayblock)",  "Codeblock calling frequency and order differs from  CA-Cl*pper, since Harbour uses a different (faster) sorting algorithm (quicksort)" }, ;
      { "C52S",     "? verbage: This is an CA-Cl*pper v5.2 compliant and is only visible if source was compiled with the HB_C52_STRICT flag" }, ;
      { "C52U",     "This is an undocumented CA-Cl*pper v5.2 function and is only visible if source was compiled with the HB_C52_UNDOC flag" }, ;
      { "C53",      "This is CA-Cl*pper v5.3 compliant and is only visible if source was compiled with the HB_COMPAT_C53 flag" }, ;
      { "H",        "This is Harbour specific" }, ;
      { "NA",       "Not applicable" } ;
   }

   PUBLIC p_aPlatforms := { ;
      { "",          "" }, ;
      { "All",       "This is available on all platforms" }, ;
      { "All(64K)",  "This is available on all platforms though some platforms have a string length limit of 64KB" }, ;
      { "All(GT)",   "This part of the GT API and supported only by some platforms." }, ;
      { "All(LFN)",  "This is available on all platforms." + hb_eol() + ;
                     "If long file names are available Harbour will use/display the first 15 characters " +;
                     "else Harbour will use/display a 8.3 file name consistent with CA-Cl*pper" }, ;
      { "Linux(GT)", "Under Linux the number of columns avaliable depends of the current Terminal screen size." }, ;
      { "OS2(GT)",   "Under OS/2 the number of columns avaliable depends of the current Terminal screen size." }, ;
      { "Win(GT)",   "Under Windows, the return value of MAXROW() function is only affected if called after an SETMODE() function" }, ;
      { "BSD",       "This is available on the BSD platform" }, ;
      { "DARWIN",    "This is available on the DARWIN platform" }, ;
      { "DOS",       "This is available on the MS-DOS platform" }, ;
      { "HPUX",      "This is available on the HPUX platform" }, ;
      { "LINUX",     "This is available on the LINUX platform" }, ;
      { "OS2",       "This is available on the OS/2 platform" }, ;
      { "SUNOS",     "This is available on the SUNOS platform" }, ;
      { "Unix",      "This is available on the Unix platform(s)" }, ;
      { "Win",       "This is available on the MS-Windows platform(s)" }, ;
      { "WinCE",     "This is available on the MS-Windows-CE platform" } ;
   }

   PUBLIC p_aStatus := { ;
      { "",  "" }, ;
      { "R", "Ready" }, ;
      { "S", "Started" }, ;
      { "N", "Not started" } ;
   }

   PUBLIC p_aConversionList := { ;
      38, "amp", ;
      34, "quot", ;
      60, "lt", ;
      62, "gt" ;
   }

   RETURN


PROCEDURE ShowTemplatesHelp( cTemplate, cDelimiter )
   LOCAL o := Entry():New()
   LOCAL idxTemplates, nFrom := 1, nTo := Len( o:Templates )
   LOCAL idx

   IF ! Empty( cTemplate ) .AND. !( cTemplate == "Template" )
      IF o:IsTemplate( cTemplate )
         nFrom := nTo := hb_AScan( o:Templates, {| a | Upper( a[ 1 ] ) == Upper( cTemplate ) } )
      ELSE
         ShowHelp( "Unknown template '" + cTemplate + "'" )
         RETURN
      ENDIF
   ENDIF

   FOR idxTemplates := nFrom TO nTo
      IF ! Empty( o:Templates[ idxTemplates ] ) .AND. ;
         ! Empty( o:Templates[ idxTemplates ][ 1 ] ) .AND. ;
         !( o:Templates[ idxTemplates ][ 1 ] == "Template" )

         //~ IF nFrom != nTo
            //~ ShowSubHelp( o:Templates[ idxTemplates ][ 1 ], 1, 0 )
         //~ ENDIF

         o:SetTemplate( o:Templates[ idxTemplates ][ 1 ] )

         FOR idx := 1 TO Len( o:Fields )
            IF o:Group[ idx ] != 0
               ShowSubHelp( iif( idx == 1, "/", " " ) + "*  " + cDelimiter + o:Fields[ idx ][ 1 ] + cDelimiter, 1, 0 )
               IF o:Fields[ idx ][ 1 ] == "TEMPLATE"
                  ShowSubHelp( " *      " + o:Template, 1, 0 )
               ELSEIF o:Group[ idx ] != TPL_START .AND. o:Group[ idx ] != TPL_END .AND. .T.
                  ShowSubHelp( " *      " + iif( o:IsRequired( o:Fields[ idx ][ 1 ] ), "<required>", "<optional>" ), 1, 0 )
               ENDIF
            ENDIF
         NEXT
         ShowSubHelp( " */", 1, 0 )
         ShowSubHelp( "", 1, 0 )
      ENDIF
   NEXT

   RETURN

PROCEDURE ShowComplianceHelp()
   LOCAL idx

   FOR idx := 1 TO Len( p_aCompliance )
      ShowSubHelp( p_aCompliance[ idx ][ 1 ], 1, 0, idx )
      ShowSubHelp( Decode( "COMPLIANCE", NIL, p_aCompliance[ idx ][ 1 ] ), 1, 6, idx )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN

PROCEDURE ShowPlatformsHelp
   LOCAL idx

   FOR idx := 1 TO Len( p_aPlatforms )
      ShowSubHelp( p_aPlatforms[ idx ][ 1 ], 1, 0, idx )
      ShowSubHelp( Decode( "PLATFORMS", NIL, p_aPlatforms[ idx ][ 1 ] ), 1, 6, idx )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN
