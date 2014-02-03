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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

   CLASS VAR Fields AS ARRAY INIT { ;
      { "DOC",          "Doc" }, ;
      { "TEMPLATE",     "Template" }, ;
      { "NAME",         "" }, ;
      { "CATEGORY",     "Category" }, ;
      { "SUBCATEGORY",  "Sub category" }, ;
      { "ONELINER",     "" }, ;
      { "SYNTAX",       "Syntax" }, ;
      { "ARGUMENTS",    "Argument(s)" }, ;
      { "RETURNS",      "Returns" }, ;
      { "DESCRIPTION",  "Description" }, ;
      { "DATALINK",     "Data link" }, ;
      { "DATANOLINK",   "Data no link" }, ;
      { "METHODSLINK",  "Methods link" }, ;
      { "METHODSNOLINK","Methods no link" }, ;
      { "EXAMPLES",     "Example(s)" }, ;
      { "TESTS",        "Test(s)" }, ;
      { "STATUS",       "Status" }, ;      /* p_aStatus is the constraint list */
      { "COMPLIANCE",   "Compliance" }, ;  /* p_aCompliance is the constraint list */
      { "PLATFORMS",    "Platform(s)" }, ; /* p_aPlatforms is the constraint list */
      { "FILES",        "File(s)" }, ;
      { "SEEALSO",      "See also" }, ;
      { "END",          "End" } }

#define _S TPL_START
#define _E TPL_END
#define _T TPL_TEMPLATE
#define _R TPL_REQUIRED
#define _O TPL_OPTIONAL
#define _P TPL_PREFORMATTED
#define _U TPL_OUTPUT

   // this is best viewed with a fixed-width font
   // the columns of this array correspond to the elements of Fields
   CLASS VAR Templates AS ARRAY INIT { ;
      { "Template"      , { _S, _T,  0+_U,  0, _O   ,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U, _E } }, ;
      { "Document"      , { _S, _T, _R+_U, _R, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Function"      , { _S, _T, _R+_U, _R, _R   , _O+_U, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "C Function"    , { _S, _T, _R+_U, _R, _R   , _O+_U, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Procedure"     , { _S, _T, _R+_U, _R, _R   , _O+_U, _O+_U, _O+_U,     0, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Command"       , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U, _R+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Class"         , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U, _R+_U, _R+_U, _R+_U, _O+_U, _O+_U, _O+_U, _O+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Class method"  , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U, _R+_U, _R+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _E } }, ;
      { "Class data"    , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _E } }, ;
      { "Run time error", { _S, _T, _R+_U, _R,  0   , _O+_U,  0+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U, _O+_U,  0+_U,  0+_U, _O+_U, _E } } }

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

   VAR Group AS ARRAY
   VAR filename AS STRING
   VAR type_ AS STRING
   VAR sourcefile_ AS STRING
   VAR sourcefileversion_ AS STRING
   VAR uid_ AS STRING
   CLASS VAR uid__ AS INTEGER INIT 0

ENDCLASS

METHOD New( cType ) CLASS Entry

   ::uid_ := hb_ntos( ++::uid__ )
   IF ! __objHasData( self, ::Fields[ 1 ][ 1 ] )
      AEval( ::Fields, {| a | __objAddData( self, a[ 1 ] ) } )
   ENDIF
   IF cType != NIL
      ::Group := ::Templates[ AScan( ::Templates, {| a | Upper( a[ 1 ] ) == Upper( cType ) } ) ][ 2 ]
   ENDIF

   RETURN self

METHOD IsField( c, nType ) CLASS Entry

   LOCAL idx
   LOCAL lResult

   IF ( lResult := ( idx := AScan( ::Fields, {| a | Upper( a[ 1 ] ) == Upper( c ) } ) ) > 0 )
      IF ::Group[ idx ] == 0
         lResult := .F.
      ELSEIF nType != NIL .AND. hb_bitAnd( ::Group[ idx ], nType ) != nType
         lResult := .F.
      ENDIF
   ENDIF

   RETURN lResult

METHOD IsTemplate( cType ) CLASS Entry
   RETURN AScan( ::Templates, {| a | Upper( a[ 1 ] ) == Upper( cType ) } ) > 0

METHOD SetTemplate( cTemplate ) CLASS Entry

   LOCAL aData := Array( Len( ::Fields ) )
   LOCAL idx

   ::Group := ::Templates[ AScan( ::Templates, {| a | Upper( a[ 1 ] ) == Upper( cTemplate ) } ) ][ 2 ]
   FOR idx := 1 TO Len( aData )
      IF ::Fields[ idx ][ 1 ] == "TEMPLATE"
         aData[ idx ] := { ::Fields[ idx ][ 1 ], cTemplate }
      ELSE
         aData[ idx ] := { ::Fields[ idx ][ 1 ], iif( ::Group[ idx ] == TPL_REQUIRED, NIL, "" ) }
      ENDIF
   NEXT
   __objSetValueList( self, aData )

   RETURN self

METHOD IsConstraint( cSectionName, cSection ) CLASS Entry

   LOCAL lResult
   LOCAL idx := AScan( ::Fields, {| a | a[ 1 ] == cSectionName } )

   IF hb_bitAnd( ::Group[ idx ], hb_bitAnd( TPL_REQUIRED, TPL_OPTIONAL ) ) == 0
      lResult := .T.
   ELSEIF Type( "p_a" + cSectionName ) == "A"
      lResult := ;
         hb_AScan( &( "p_a" + cSectionName ), cSection, , , .T. ) .OR. ;
         hb_AScan( &( "p_a" + cSectionName ), Parse( cSection, "," ), , , .T. )
   ELSE
      lResult := .T.
   ENDIF

   RETURN lResult

METHOD IsComplete( cIncompleteFielsList ) CLASS Entry

   LOCAL lResult := .T.
   LOCAL idx

   cIncompleteFielsList := ""

   FOR idx := 1 TO Len( ::Fields )
      IF hb_bitAnd( ::Group[ idx ], TPL_REQUIRED ) != 0 .AND. Empty( ::&( ::Fields[ idx ][ 1 ] ) )
         cIncompleteFielsList += "," + ::Fields[ idx ][ 1 ]
         lResult := .F.
      ENDIF
   NEXT

   cIncompleteFielsList := SubStr( cIncompleteFielsList, 2 )

   RETURN lResult

METHOD IsPreformatted( cField ) CLASS Entry
   RETURN hb_bitAnd( ::Group[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ], TPL_PREFORMATTED ) != 0

METHOD IsRequired( cField ) CLASS Entry
   RETURN hb_bitAnd( ::Group[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ], TPL_REQUIRED ) != 0

METHOD IsOptional( cField ) CLASS Entry
   RETURN hb_bitAnd( ::Group[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ], TPL_OPTIONAL ) != 0

METHOD IsOutput( cField ) CLASS Entry
   RETURN hb_bitAnd( ::Group[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ], TPL_OUTPUT ) != 0

METHOD FieldName( cField ) CLASS Entry
   RETURN ::Fields[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ][ 2 ]

METHOD CategoryIndex( cCategory ) CLASS Entry
   RETURN AScan( p_aCategories, {| a | HB_ISARRAY( a ) .AND. Len( a ) >= 1 .AND. a[ 1 ] == cCategory } )

METHOD SubcategoryIndex( cCategory, cSubcategory ) CLASS Entry
   RETURN ::CategoryIndex( cCategory ) >= 1 .AND. ;
      hb_AScan( p_aCategories[ ::CategoryIndex( cCategory ) ][ 2 ], cSubcategory, , , .T. )

PROCEDURE init_Templates()

   LOCAL item
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
      "Virtual machine" }

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
            "Var" } }, ;
      { "Command", AClone( aSubCategories ) }, ;
      /* { "Compile time errors", { {} } }, */ ;
      { "Run time errors", { "" } } }

   FOR EACH item IN p_aCategories
      IF ! Empty( item )
         AAdd( item, Array( Len( item[ 2 ] ) ) ) // holder array of sub-category entries
         AAdd( item, "" ) // holder for sub-category file name
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
      { "NA",       "Not applicable" } }

   PUBLIC p_aPlatforms := { ;
      { "",          "" }, ;
      { "All",       "This is available on all platforms" }, ;
      { "All(GT)",   "This part of the GT API and supported only by some platforms." }, ;
      { "All(LFN)",  "This is available on all platforms." + hb_eol() + ;
                     "If long file names are available Harbour will use/display the first 15 characters " +;
                     "else Harbour will use/display a 8.3 file name consistent with CA-Cl*pper" }, ;
      { "Linux(GT)", "Under Linux the number of columns avaliable depends of the current Terminal screen size." }, ;
      { "OS2(GT)",   "Under OS/2 the number of columns avaliable depends of the current Terminal screen size." }, ;
      { "Win(GT)",   "Under Windows, the return value of MaxRow() function is only affected if called after an SetMode() function" }, ;
      { "BSD",       "This is available on the BSD platform" }, ;
      { "DARWIN",    "This is available on the Darwin platform" }, ;
      { "DOS",       "This is available on the MS-DOS platform" }, ;
      { "HPUX",      "This is available on the HPUX platform" }, ;
      { "LINUX",     "This is available on the Linux platform" }, ;
      { "OS2",       "This is available on the OS/2 platform" }, ;
      { "SUNOS",     "This is available on the SunOS platform" }, ;
      { "Unix",      "This is available on the Unix platform(s)" }, ;
      { "Win",       "This is available on the Windows platform(s)" }, ;
      { "WinCE",     "This is available on the Windows CE platform" } }

   PUBLIC p_aStatus := { ;
      { "",  "" }, ;
      { "R", "Ready" }, ;
      { "S", "Started" }, ;
      { "N", "Not started" } }

   RETURN

PROCEDURE ShowTemplatesHelp( cTemplate, cDelimiter )

   LOCAL o := Entry():New()
   LOCAL idxTemplates, nFrom := 1, nTo := Len( o:Templates )
   LOCAL idx

   IF ! Empty( cTemplate ) .AND. !( cTemplate == "Template" )
      IF o:IsTemplate( cTemplate )
         nFrom := nTo := AScan( o:Templates, {| a | Upper( a[ 1 ] ) == Upper( cTemplate ) } )
      ELSE
         ShowHelp( "Unknown template '" + cTemplate + "'" )
         RETURN
      ENDIF
   ENDIF

   FOR idxTemplates := nFrom TO nTo
      IF ! Empty( o:Templates[ idxTemplates ] ) .AND. ;
         ! Empty( o:Templates[ idxTemplates ][ 1 ] ) .AND. ;
         !( o:Templates[ idxTemplates ][ 1 ] == "Template" )

#if 0
         IF nFrom != nTo
            ShowSubHelp( o:Templates[ idxTemplates ][ 1 ], 1, 0 )
         ENDIF
#endif

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

   LOCAL item

   FOR EACH item IN p_aCompliance
      ShowSubHelp( item[ 1 ], 1, 0, item:__enumIndex() )
      ShowSubHelp( Decode( "COMPLIANCE", NIL, item[ 1 ] ), 1, 6, item:__enumIndex() )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN

PROCEDURE ShowPlatformsHelp

   LOCAL item

   FOR EACH item IN p_aPlatforms
      ShowSubHelp( item[ 1 ], 1, 0, item:__enumIndex() )
      ShowSubHelp( Decode( "PLATFORMS", NIL, item[ 1 ] ), 1, 6, item:__enumIndex() )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN
