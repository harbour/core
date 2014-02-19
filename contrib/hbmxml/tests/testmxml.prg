/*
 * Test program for Mini-XML, a small XML-like file parsing library.
 *
 * Copyright 2003-2011 by Michael R Sweet.
 *
 * These coded instructions, statements, and computer programs are the
 * property of Michael R Sweet and are protected by Federal copyright
 * law.  Distribution and use rights are outlined in the file "COPYING.txt"
 * which should have been included with this file.  If this file is
 * missing or damaged, see the license at:
 *
 *     http://www.minixml.org/
 *
 * Harbour port Copyright (c) 2011 Tamas TEVESZ <ice@extreme.hu>
 *
 */

#require "hbmxml"

#include "simpleio.ch"

REQUEST HB_GT_CGI_DEFAULT

REQUEST HB_CODEPAGE_UTF8EX

STATIC s_aTypes := { ;
   "MXML_ELEMENT", ;
   "MXML_INTEGER", ;
   "MXML_OPAQUE", ;
   "MXML_REAL", ;
   "MXML_TEXT" }

STATIC s_aSAXEventCounts := { 0, 0, 0, 0, 0, 0 }

/*
 * 'main()' - Main entry for test program.
 */

PROCEDURE Main( cFileArg )

   LOCAL hTree                            /* XML tree */
   LOCAL hNode                            /* Node which should be in test.xml */
   LOCAL hInd                             /* XML index */
   LOCAL nNum, cStr
   LOCAL i

   hb_cdpSelect( "UTF8EX" )
   hb_SetTermCP( hb_cdpTerm() )

   /*
    * Check arguments...
    */

   IF Empty( cFileArg )
      ? hb_StrFormat( "Usage: %1$s filename.xml", hb_ProgName() )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   /*
    * Test the basic functionality...
    */

   hTree := mxmlNewElement( MXML_NO_PARENT, "element" )
   IF Empty( hTree )
      ? "ERROR: No parent node in basic test!"
      RETURN
   ENDIF

   nNum := mxmlGetType( hTree )
   IF nNum != MXML_ELEMENT
      IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
         ? hb_StrFormat( "ERROR: Parent has type %1$s (%2$d), expected MXML_ELEMENT!", ;
            "UNKNOWN", nNum )
      ELSE
         ? hb_StrFormat( "ERROR: Parent has type %1$s (%2$d), expected MXML_ELEMENT!", ;
            s_aTypes[ nNum + 1 ], nNum )
      ENDIF

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   cStr := mxmlGetElement( hTree )
   IF !( cStr == "element" )
      ? hb_StrFormat( "ERROR: Parent value is '%1$s', expected 'element'", cStr )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   mxmlNewInteger( hTree, 123 )
   mxmlNewOpaque( hTree, "opaque" )
   mxmlNewReal( hTree, 123.4 )
   mxmlNewText( hTree, 1, "text" )

   mxmlLoadString( hTree, "<group type='string'>string string string</group>", MXML_NO_CALLBACK )
   mxmlLoadString( hTree, "<group type='integer'>1 2 3</group>", MXML_INTEGER_CALLBACK )
   mxmlLoadString( hTree, "<group type='real'>1.0 2.0 3.0</group>", MXML_REAL_CALLBACK )
   mxmlLoadString( hTree, "<group>opaque opaque opaque</group>", MXML_OPAQUE_CALLBACK )
   mxmlLoadString( hTree, "<foo><bar><one><two>value<two>value2</two></two></one></bar></foo>", MXML_OPAQUE_CALLBACK )

   IF Empty( hNode := mxmlGetFirstChild( hTree ) )
      ? "ERROR: No first child in basic test!"

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   nNum := mxmlGetType( hNode )
   IF nNum != MXML_INTEGER
      IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
         ? hb_StrFormat( "ERROR: First child has type %1$s (%2$d), expected MXML_TEXT!", ;
            "UNKNOWN", nNum )
      ELSE
         ? hb_StrFormat( "ERROR: First child has type %1$s (%2$d), expected MXML_TEXT!", ;
            s_aTypes[ nNum + 1 ], nNum )
      ENDIF

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   nNum := mxmlGetInteger( hNode )
   IF nNum != 123
      ? hb_StrFormat( "ERROR: First child value is %1$d, expected 123!", nNum )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   hNode := mxmlGetNextSibling( hNode )
   IF Empty( hNode )
      ? "ERROR: No second child node in basic test!"

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   nNum := mxmlGetType( hNode )
   IF nNum != MXML_OPAQUE
      IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
         ? hb_StrFormat( "ERROR: Second child has type %1$s (%2$d), expected MXML_OPAQUE!", ;
            "UNKNOWN", nNum )
      ELSE
         ? hb_StrFormat( "ERROR: Second child has type %1$s (%2$d), expected MXML_OPAQUE!", ;
            s_aTypes[ nNum + 1 ], nNum )
      ENDIF

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   hNode := mxmlGetNextSibling( hNode )
   IF Empty( hNode )
      ? "ERROR: No third child node in basic test!"

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   nNum := mxmlGetType( hNode )
   IF nNum != MXML_REAL
      IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
         ? hb_StrFormat( "ERROR: Third child has type %1$s (%2$d), expected MXML_REAL!", ;
            "UNKNOWN", nNum )
      ELSE
         ? hb_StrFormat( "ERROR: Third child has type %1$s (%2$d), expected MXML_REAL!", ;
            s_aTypes[ nNum + 1 ], nNum )
      ENDIF

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   nNum := mxmlGetReal( hNode )
   IF nNum != 123.4
      ? hb_StrFormat( "ERROR: Third child value is %1$f, expected 123.4!", nNum )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   hNode := mxmlGetNextSibling( hNode )
   IF Empty( hNode )
      ? "ERROR: No fourth child node in basic test!"

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   nNum := mxmlGetType( hNode )
   IF nNum != MXML_TEXT
      IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
         ? hb_StrFormat( "ERROR: Fourth child has type %1$s (%2$d), expected MXML_TEXT!", ;
            "UNKNOWN", nNum )
      ELSE
         ? hb_StrFormat( "ERROR: Fourth child has type %1$s (%2$d), expected MXML_TEXT!", ;
            s_aTypes[ nNum + 1 ], nNum )
      ENDIF

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   cStr := mxmlGetText( hNode, @nNum )
   IF nNum != 1 .OR. Empty( cStr ) .OR. !( cStr == "text" )
      ? hb_StrFormat( "ERROR: Fourth child value is %1$d, '%2$s', expected 1, 'text'!", ;
         nNum, cStr )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   FOR i := 0 TO 3
      IF Empty( hNode := mxmlGetNextSibling( hNode ) )
         ? hb_StrFormat( "ERROR: No group #%1$d child node in basic test!", i )

         mxmlDelete( hTree )
         ErrorLevel( 1 )
         RETURN
      ENDIF

      IF ( ( nNum := mxmlGetType( hNode ) ) != MXML_ELEMENT )
         IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
            ? hb_StrFormat( "ERROR: Group child #%1$d has type %2$s (%3$d), expected MXML_ELEMENT!", ;
               i + 1, "UNKNOWN", nNum )
         ELSE
            ? hb_StrFormat( "ERROR: Group child #%1$d has type %2$s (%3$d), expected MXML_ELEMENT!", ;
               i + 1, s_aTypes[ nNum + 1 ], nNum )
         ENDIF

         mxmlDelete( hTree )
         ErrorLevel( 1 )
         RETURN
      ENDIF
   NEXT

   /*
    * Test mxmlFindPath...
    */

   hNode := mxmlFindPath( hTree, "*/two" )
   IF Empty( hNode )
      ? "ERROR: Unable to find value for '*/two'."

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ELSEIF mxmlGetType( hNode ) != MXML_OPAQUE .OR. !( mxmlGetOpaque( hNode ) == "value" )
      ? "ERROR: Bad value for '*/two'."

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   hNode := mxmlFindPath( hTree, "foo/*/two" )
   IF Empty( hNode )
      ? "ERROR: Unable to find value for 'foo/*/two'."

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ELSEIF mxmlGetType( hNode ) != MXML_OPAQUE .OR. !( mxmlGetOpaque( hNode ) == "value" )
      ? "ERROR: Bad value for 'foo/*/two'."

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   hNode := mxmlFindPath( hTree, "foo/bar/one/two" )
   IF Empty( hNode )
      ? "ERROR: Unable to find value for 'foo/bar/one/two'."

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ELSEIF mxmlGetType( hNode ) != MXML_OPAQUE .OR. !( mxmlGetOpaque( hNode ) == "value" )
      ? "ERROR: Bad value for 'foo/bar/one/two'."

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   /*
    * Test indices...
    */

   hInd := mxmlIndexNew( hTree )
   IF Empty( hInd )
      ? "ERROR: Unable to create index of all nodes!"

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   IF ( nNum := mxmlIndexGetCount( hInd ) ) != 10
      ? hb_StrFormat( "ERROR: Index of all nodes contains %1$d nodes; expected 10!", nNum )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   mxmlIndexReset( hInd )
   IF Empty( mxmlIndexFind( hInd, "group" ) )
      ? "ERROR: mxmlIndexFind for 'group' failed!"

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   mxmlIndexDelete( hInd )

   hInd := mxmlIndexNew( hTree, "group" )
   IF Empty( hInd )
      ? "ERROR: Unable to create index of groups!"

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   IF ( nNum := mxmlIndexGetCount( hInd ) ) != 4
      ? hb_StrFormat( "ERROR: Index of groups contains %1$d nodes; expected 4!", nNum )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   mxmlIndexReset( hInd )
   IF Empty( mxmlIndexEnum( hInd ) )
      ? "ERROR: mxmlIndexEnum failed!"

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   mxmlIndexDelete( hInd )

   hInd := mxmlIndexNew( hTree,, "type" )
   IF Empty( hInd )
      ? "ERROR: Unable to create index of type attributes!"

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   IF ( nNum := mxmlIndexGetCount( hInd ) ) != 3
      ? hb_StrFormat( "ERROR: Index of type attributes contains %1$d nodes; expected 3!", nNum )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   mxmlIndexReset( hInd )
   IF Empty( mxmlIndexFind( hInd,, "string" ) )
      ? "ERROR: mxmlIndexFind for 'string' failed!"

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   mxmlIndexDelete( hInd )

   hInd := mxmlIndexNew( hTree, "group", "type" )
   IF Empty( hInd )
      ? "ERROR: Unable to create index of elements and attributes!"

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   IF ( nNum := mxmlIndexGetCount( hInd ) ) != 3
      ? hb_StrFormat( "ERROR: Index of type attributes contains %1$d nodes; expected 3!", nNum )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   mxmlIndexReset( hInd )
   IF Empty( mxmlIndexFind( hInd, "group", "string" ) )
      ? "ERROR: mxmlIndexFind for 'string' failed!"

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   mxmlIndexDelete( hInd )

   /*
    * Check the mxmlDelete() works properly...
    */

   FOR i := 0 TO 8
      IF ! Empty( mxmlGetFirstChild( hTree ) )
         mxmlDelete( mxmlGetFirstChild( hTree ) )
      ELSE
         ? hb_StrFormat( "ERROR: Child pointer prematurely NULL on child #%1$d", i )

         mxmlDelete( hTree )
         ErrorLevel( 1 )
         RETURN
      ENDIF
   NEXT

   IF ! Empty( mxmlGetFirstChild( hTree ) )
      ? "ERROR: Child pointer not NULL after deleting all children!"

      ErrorLevel( 1 )
      RETURN
   ENDIF

   IF ! Empty( mxmlGetLastChild( hTree ) )
      ? "ERROR: Last child pointer not NULL after deleting all children!"

      ErrorLevel( 1 )
      RETURN
   ENDIF

   /*
    * Open the file...
    */

   IF hb_LeftIs( cFileArg, "<" )
      hTree := mxmlLoadString( NIL, cFileArg, @type_cb() )
   ELSE

      /*
       * Read the file...
       */

      hTree := mxmlLoadFile( NIL, cFileArg, @type_cb() )
   ENDIF

   IF Empty( hTree )
      ? "Unable to read XML file!"
      ErrorLevel( 1 )
      RETURN
   ENDIF

   IF cFileArg == "test.xml"

      /*
       * Verify that mxmlFindElement() and indirectly mxmlWalkNext() work
       * properly... XXX: this doesn't test for the mxmlWalkNext() _binding_
       */

      IF Empty( hNode := mxmlFindElement( hTree, hTree, "choice",,, MXML_DESCEND ) )
         ? "Unable to find first <choice> element in XML tree!"

         mxmlDelete( hTree )
         ErrorLevel( 1 )
         RETURN
      ENDIF

      IF Empty( mxmlFindElement( hNode, hTree, "choice",,, MXML_NO_DESCEND ) )
         ? "Unable to find second <choice> element in XML tree!"

         mxmlDelete( hTree )
         ErrorLevel( 1 )
         RETURN
      ENDIF
   ENDIF

   /*
    * Print the XML tree...
    */

   FErase( "out.xml" )
   mxmlSaveFile( hTree, "out.xml", @whitespace_cb() )

   /* XXX: */
   /*
    * Save the XML tree to a string and print it...
    */

   cStr := Space( 16384 )
   IF ( nNum := mxmlSaveString( hTree, @cStr, @whitespace_cb() ) ) > 0
      ? cStr
   ENDIF

   /*
    * Delete the tree...
    */

   mxmlDelete( hTree )

   /*
    * Test SAX methods...
    */

   IF hb_LeftIs( cFileArg, "<" )
      mxmlSAXLoadString( NIL, cFileArg, @type_cb(), @sax_cb(), NIL )
   ELSE

      /*
       * Read the file...
       */

      mxmlSAXLoadFile( NIL, cFileArg, @type_cb(), @sax_cb(), NIL )
   ENDIF

   IF cFileArg == "test.xml"

      IF s_aSAXEventCounts[ MXML_SAX_CDATA ] != 1
         ? hb_StrFormat( "MXML_SAX_CDATA seen %1$d times, expected 1 times!", ;
            s_aSAXEventCounts[ MXML_SAX_CDATA ] )
         ErrorLevel( 1 )
         RETURN
      ENDIF

      IF s_aSAXEventCounts[ MXML_SAX_COMMENT ] != 1
         ? hb_StrFormat( "MXML_SAX_COMMENT seen %1$d times, expected 1 times!", ;
            s_aSAXEventCounts[ MXML_SAX_COMMENT ] )
         ErrorLevel( 1 )
         RETURN
      ENDIF

      IF s_aSAXEventCounts[ MXML_SAX_DATA ] != 60
         ? hb_StrFormat( "MXML_SAX_DATA seen %1$d times, expected 60 times!", ;
            s_aSAXEventCounts[ MXML_SAX_DATA ] )
         ErrorLevel( 1 )
         RETURN
      ENDIF

      IF s_aSAXEventCounts[ MXML_SAX_DIRECTIVE ] != 1
         ? hb_StrFormat( "MXML_SAX_DIRECTIVE seen %1$d times, expected 1 times!", ;
            s_aSAXEventCounts[ MXML_SAX_DIRECTIVE ] )
         ErrorLevel( 1 )
         RETURN
      ENDIF

      IF s_aSAXEventCounts[ MXML_SAX_ELEMENT_CLOSE ] != 20
         ? hb_StrFormat( "MXML_SAX_ELEMENT_CLOSE seen %1$d times, expected 20 times!", ;
            s_aSAXEventCounts[ MXML_SAX_ELEMENT_CLOSE ] )
         ErrorLevel( 1 )
         RETURN
      ENDIF

      IF s_aSAXEventCounts[ MXML_SAX_ELEMENT_OPEN ] != 20
         ? hb_StrFormat( "MXML_SAX_ELEMENT_OPEN seen %1$d times, expected 20 times!", ;
            s_aSAXEventCounts[ MXML_SAX_ELEMENT_OPEN ] )
         ErrorLevel( 1 )
         RETURN
      ENDIF

   ENDIF

   ErrorLevel( 0 )

   RETURN

/*
 * 'sax_cb()' - Process nodes via SAX.
 */

/* I - Current node */
/* I - SAX event */
/* I - SAX user data */

STATIC PROCEDURE sax_cb( hNode, hEvent, hData )

   /*
    * This SAX callback just counts the different events.
    */

   HB_SYMBOL_UNUSED( hNode )
   HB_SYMBOL_UNUSED( hData )

   s_aSAXEventCounts[ hEvent ]++

   RETURN

/*
 * 'type_cb()' - XML data type callback for mxmlLoadFile()...
 */

/* O - Data type */
/* I - Element node */

STATIC FUNCTION type_cb( hNode )

   LOCAL cType                            /* Type string */

   /*
    * You can lookup attributes and/or use the element name, hierarchy, etc...
    */

   IF Empty( cType := mxmlElementGetAttr( hNode, "type" ) )
      cType := mxmlGetElement( hNode )
   ENDIF

   SWITCH Lower( cType )
   CASE "integer" ;  RETURN MXML_INTEGER
   CASE "opaque"  ;  RETURN MXML_OPAQUE
   CASE "real"    ;  RETURN MXML_REAL
   ENDSWITCH

   RETURN MXML_TEXT

/*
 * 'whitespace_cb()' - Let the mxmlSaveFile() function know when to insert
 *                     newlines and tabs...
 */

/* O - Whitespace string or NIL */
/* I - Element node */
/* I - Open or close tag? */

STATIC FUNCTION whitespace_cb( hNode, nWhere )

   LOCAL hParent                          /* Parent node */
   LOCAL nLevel                           /* Indentation level */
   LOCAL cName                            /* Name of element */

   /*
    * We can conditionally break to a new line before or after any element.
    * These are just common HTML elements...
    */

   cName := Lower( mxmlGetElement( hNode ) )

   IF cName == "html" .OR. cName == "head" .OR. cName == "body" .OR. ;
      cName == "pre" .OR. cName == "p" .OR. ;
      cName == "h1" .OR. cName == "h2" .OR. cName == "h3" .OR. ;
      cName == "h4" .OR. cName == "h5" .OR. cName == "h6"

         /*
          * Newlines before open and after close...
          */

      IF nWhere == MXML_WS_BEFORE_OPEN .OR. nWhere == MXML_WS_AFTER_CLOSE
         RETURN hb_eol()
      ENDIF
   ELSEIF cName == "dl" .OR. cName == "ol" .OR. cName == "ul"

      /*
       * Put a newline before and after list elements...
       */

      RETURN hb_eol()
   ELSEIF cName == "dd" .OR. cName == "dd" .OR. cName == "li"

      /*
       * Put a tab before <li>s, <dd>s and <dt>s and a newline after them...
       */

      IF nWhere == MXML_WS_BEFORE_OPEN
         RETURN Space( 8 )
      ELSEIF nWhere == MXML_WS_AFTER_CLOSE
         RETURN hb_eol()
      ENDIF
   ELSEIF hb_LeftIs( cName, "?xml" )
      IF nWhere == MXML_WS_AFTER_OPEN
         RETURN hb_eol()
      ELSE
         RETURN NIL
      ENDIF
   ELSEIF nWhere == MXML_WS_BEFORE_OPEN .OR. ;
         ( ( cName == "choice" .OR. cName == "option" ) .AND. nWhere == MXML_WS_BEFORE_CLOSE )
      nLevel := -1
      hParent := mxmlGetParent( hNode )
      DO WHILE ! Empty( hParent )
         nLevel++
         hParent := mxmlGetParent( hParent )
      ENDDO

      IF nLevel > 8
         nLevel := 8
      ELSEIF nLevel < 0
         nLevel := 0
      ENDIF

      RETURN Replicate( Chr( 9 ), nLevel )
   ELSEIF nWhere == MXML_WS_AFTER_CLOSE .OR. ;
         ( ( cName == "group" .OR. cName == "option" .OR. cName == "choice" ) .AND. ;
         nWhere == MXML_WS_AFTER_OPEN )

      RETURN hb_eol()
   ELSEIF nWhere == MXML_WS_AFTER_OPEN .AND. Empty( mxmlGetFirstChild( hNode ) )
      RETURN hb_eol()
   ENDIF

   /*
    * Return NULL for no added whitespace...
    */

   RETURN NIL
