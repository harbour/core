/*
 * $Id$
 */

/*
 * Test program for Mini-XML, a small XML-like file parsing library.
 *
 * Copyright 2003-2010 by Michael R Sweet.
 *
 * These coded instructions, statements, and computer programs are the
 * property of Michael R Sweet and are protected by Federal copyright
 * law.  Distribution and use rights are outlined in the file "COPYING"
 * which should have been included with this file.  If this file is
 * missing or damaged, see the license at:
 *
 *     http://www.minixml.org/
 *
 * Harbour port Copyright (c) 2011 Tamas TEVESZ <ice@extreme.hu>
 *
 */

#include "common.ch"
#include "hbmxml.ch"
#include "simpleio.ch"

STATIC s_aTypes := { ;
   "MXML_ELEMENT",   ;
   "MXML_INTEGER",   ;
   "MXML_OPAQUE",    ;
   "MXML_REAL",      ;
   "MXML_TEXT"       ;
}

/*
 * Main entry for test program.
 */
PROCEDURE Main( cFileArg )

   LOCAL hTree, hNode, hInd
   LOCAL nNum, cStr
   LOCAL i

   /*
    * Check arguments...
    */
   IF Empty( cFileArg )
      OutErr( "Usage: textmxml filename.xml" + hb_eol() )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   /*
    * Test the basic functionality...
    */
   hTree := mxmlNewElement( MXML_NO_PARENT, "element" )
   IF Empty( hTree )
      OutErr( "ERROR: No parent node in basic test!" + hb_eol() )
      QUIT
   ENDIF

   IF ( ( nNum := mxmlGetType( hTree ) ) != MXML_ELEMENT )
      IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
         OutErr( hb_strFormat( "ERROR: Parent has type %s (%d), expected MXML_ELEMENT!",  ;
                               "UNKNOWN", nNum ) + hb_eol() )
      ELSE
         OutErr( hb_strFormat( "ERROR: Parent has type %s (%d), expected MXML_ELEMENT!",  ;
                               s_aTypes[ nNum + 1 ], nNum ) + hb_eol() )
      ENDIF

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ( ( cStr := mxmlGetElement( hTree ) ) != "element" )
      OutErr( hb_strFormat( "ERROR: Parent value is '%s', expected 'element'", cStr ) + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
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
      OutErr( "ERROR: No first child in basic test!" + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ( nNum := mxmlGetType( hNode ) ) != MXML_INTEGER
      IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
         OutErr( hb_strFormat( "ERROR: First child has type %s (%d), expected MXML_TEXT!",  ;
                               "UNKNOWN", nNum ) + hb_eol() )
      ELSE
         OutErr( hb_strFormat( "ERROR: First child has type %s (%d), expected MXML_TEXT!",  ;
                               s_aTypes[ nNum + 1 ], nNum ) + hb_eol() )
      ENDIF

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ( nNum := mxmlGetInteger( hNode ) ) != 123
      OutErr( hb_strFormat( "ERROR: First child value is %d, expected 123!", nNum ) + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF Empty( hNode := mxmlGetNextSibling( hNode ) )
      OutErr( "ERROR: No second child node in basic test!" + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ( nNum := mxmlGetType( hNode ) ) != MXML_OPAQUE
      IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
         OutErr( hb_strFormat( "ERROR: Second child has type %s (%d), expected MXML_OPAQUE!",  ;
                               "UNKNOWN", nNum ) + hb_eol() )
      ELSE
         OutErr( hb_strFormat( "ERROR: Second child has type %s (%d), expected MXML_OPAQUE!",  ;
                               s_aTypes[ nNum + 1 ], nNum ) + hb_eol() )
      ENDIF

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF Empty( hNode := mxmlGetNextSibling( hNode ) )
      OutErr( "ERROR: No third child node in basic test!" + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ( nNum := mxmlGetType( hNode ) ) != MXML_REAL
      IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
         OutErr( hb_strFormat( "ERROR: Third child has type %s (%d), expected MXML_REAL!",  ;
                               "UNKNOWN", nNum ) + hb_eol() )
      ELSE
         OutErr( hb_strFormat( "ERROR: Third child has type %s (%d), expected MXML_REAL!",  ;
                               s_aTypes[ nNum + 1 ], nNum ) + hb_eol() )
      ENDIF

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ( nNum := mxmlGetReal( hNode ) ) != 123.4
      OutErr( hb_strFormat( "ERROR: Third child value is %f, expected 123.4!", nNum ) + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF Empty( hNode := mxmlGetNextSibling( hNode ) )
      OutErr( "ERROR: No fourth child node in basic test!" + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ( nNum := mxmlGetType( hNode ) ) != MXML_TEXT
      IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
         OutErr( hb_strFormat( "ERROR: Fourth child has type %s (%d), expected MXML_TEXT!",  ;
                               "UNKNOWN", nNum ) + hb_eol() )
      ELSE
         OutErr( hb_strFormat( "ERROR: Fourth child has type %s (%d), expected MXML_TEXT!",  ;
                               s_aTypes[ nNum + 1 ], nNum ) + hb_eol() )
      ENDIF

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   cStr := mxmlGetText( hNode, @nNum )
   IF nNum != 1 .OR. Empty( cStr ) .OR. cStr != "text"
      OutErr( hb_strFormat( "ERROR: Fourth child value is %d, '%s', expected 1, 'text'!",    ;
                            nNum, cStr ) + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   FOR i := 0 TO 3
      IF Empty( hNode := mxmlGetNextSibling( hNode ) )
         OutErr( hb_strFormat( "ERROR: No group #%d child node in basic test!", i ) + hb_eol() )

         mxmlDelete( hTree )
         ErrorLevel( 1 )
         QUIT
      ENDIF

      IF ( ( nNum := mxmlGetType( hNode ) ) != MXML_ELEMENT )
         IF nNum < MXML_ELEMENT .OR. nNum > MXML_TEXT
            OutErr( hb_strFormat( "ERROR: Group child #%d has type %s (%d), expected MXML_ELEMENT!",  ;
                                  i + 1, "UNKNOWN", nNum ) + hb_eol() )
         ELSE
            OutErr( hb_strFormat( "ERROR: Group child #%d has type %s (%d), expected MXML_ELEMENT!",  ;
                                  s_aTypes[ nNum + 1 ], nNum ) + hb_eol() )
         ENDIF

         mxmlDelete( hTree )
         ErrorLevel( 1 )
         QUIT
      ENDIF
   NEXT

   /*
    * Test mxmlFindPath...
    */
   hNode := mxmlFindPath( hTree, "*/two" )
   IF Empty( hNode )
      OutErr( "ERROR: Unable to find value for '*/two'." + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ELSEIF mxmlGetType( hNode ) != MXML_OPAQUE .OR. mxmlGetOpaque( hNode ) != "value"
      OutErr( "ERROR: Bad value for '*/two'." + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   hNode := mxmlFindPath( hTree, "foo/*/two" )
   IF Empty( hNode )
      OutErr( "ERROR: Unable to find value for 'foo/*/two'." + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ELSEIF mxmlGetType( hNode ) != MXML_OPAQUE .OR. mxmlGetOpaque( hNode ) != "value"
      OutErr( "ERROR: Bad value for 'foo/*/two'." + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   hNode := mxmlFindPath( hTree, "foo/bar/one/two" )
   IF Empty( hNode )
      OutErr( "ERROR: Unable to find value for 'foo/bar/one/two'." + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ELSEIF mxmlGetType( hNode ) != MXML_OPAQUE .OR. mxmlGetOpaque( hNode ) != "value"
      OutErr( "ERROR: Bad value for 'foo/bar/one/two'." + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   /*
    * Test indices...
    */
   hInd := mxmlIndexNew( hTree )
   IF Empty( hInd )
      OutErr( "ERROR: Unable to create index of all nodes!" + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ( nNum := mxmlIndexGetCount( hInd ) ) != 10
      OutErr( hb_strFormat( "ERROR: Index of all nodes contains %d nodes; expected 10!", nNum ) + hb_eol() )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   mxmlIndexReset( hInd )
   IF Empty( mxmlIndexFind( hInd, "group" ) )
      OutErr( "ERROR: mxmlIndexFind for 'group' failed!" + hb_eol() )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   mxmlIndexDelete( hInd )

   hInd := mxmlIndexNew( hTree, "group" )
   IF Empty( hInd )
      OutErr( "ERROR: Unable to create index of groups!" + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ( nNum := mxmlIndexGetCount( hInd ) ) != 4
      OutErr( hb_strFormat( "ERROR: Index of groups contains %d nodes; expected 4!", nNum ) + hb_eol() )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   mxmlIndexReset( hInd )

   IF Empty( mxmlIndexEnum( hInd ) )
      OutErr( "ERROR: mxmlIndexEnum failed!" + hb_eol() )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   mxmlIndexDelete( hInd )

   hInd := mxmlIndexNew( hTree,, "type" )
   IF Empty( hInd )
      OutErr( "ERROR: Unable to create index of type attributes!" + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ( nNum := mxmlIndexGetCount( hInd ) ) != 3
      OutErr( hb_strFormat( "ERROR: Index of type attributes contains %d nodes; expected 3!", nNum ) + hb_eol() )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   mxmlIndexReset( hInd )
   IF Empty( mxmlIndexFind( hInd,, "string" ) )
      OutErr( "ERROR: mxmlIndexFind for 'string' failed!" + hb_eol() )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   mxmlIndexDelete( hInd )

   hInd := mxmlIndexNew( hTree, "group", "type" )
   IF Empty( hInd )
      OutErr( "ERROR: Unable to create index of elements and attributes!" + hb_eol() )

      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ( nNum := mxmlIndexGetCount( hInd ) ) != 3
      OutErr( hb_strFormat( "ERROR: Index of type attributes contains %d nodes; expected 3!", nNum ) + hb_eol() )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   mxmlIndexReset( hInd )
   IF Empty( mxmlIndexFind( hInd, "group", "string" ) )
      OutErr( "ERROR: mxmlIndexFind for 'string' failed!" + hb_eol() )

      mxmlIndexDelete( hInd )
      mxmlDelete( hTree )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   mxmlIndexDelete( hInd )

   /*
    * Check the mxmlDelete() works properly...
    */
   FOR i := 0 TO 8
      IF ! Empty( mxmlGetFirstChild( hTree ) )
         mxmlDelete( mxmlGetFirstChild( hTree ) )
      ELSE
         OutErr( hb_strFormat( "ERROR: Child pointer prematurely NULL on child #%d", i ) + hb_eol() )

         mxmlDelete( hTree )
         ErrorLevel( 1 )
         QUIT
      ENDIF
   NEXT

   IF ! Empty( mxmlGetFirstChild( hTree ) )
      OutErr( "ERROR: Child pointer not NULL after deleting all children!" + hb_eol() )

      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF ! Empty( mxmlGetLastChild( hTree ) )
      OutErr( "ERROR: Last child pointer not NULL after deleting all children!" + hb_eol() )

      ErrorLevel( 1 )
      QUIT
   ENDIF

   /*
    * Open the file...
    */
   IF Left( cFileArg, 1 ) == "<"
      hTree := mxmlLoadString( nil, cFileArg, @type_cb() )
   ELSE
      /*
       * Read the file...
       */
      hTree := mxmlLoadFile( nil, cFileArg, @type_cb() )
   ENDIF

   IF Empty( hTree )
      OutErr( "Unable to read XML file!" + hb_eol() )
      ErrorLevel( 1 )
      QUIT
   ENDIF

   IF cFileArg == "test.xml"
      /*
       * Verify that mxmlFindElement() and indirectly mxmlWalkNext() work
       * properly... XXX: this doesn't test for the mxmlWalkNext() _binding_
       */
      IF Empty( hNode := mxmlFindElement( hTree, hTree, "choice",,, MXML_DESCEND ) )
         OutErr( "Unable to find first <choice> element in XML tree!" + hb_eol() )

         mxmlDelete( hTree )
         ErrorLevel( 1 )
         QUIT
      ENDIF

      IF Empty( mxmlFindElement( hNode, hTree, "choice",,, MXML_NO_DESCEND ) )
         OutErr( "Unable to find second <choice> element in XML tree!" + hb_eol() )

         mxmlDelete( hTree )
         ErrorLevel( 1 )
         QUIT
      ENDIF
   ENDIF

   /*
    * Print the XML tree...
    */
   FErase( "out.xml" )
   mxmlSaveFile( hTree, "out.xml", @whitespace_cb() )

   /*
    * Save the XML tree to a string and print it...
    */
   cStr := Space( 112 )
   IF ( nNum := mxmlSaveString( hTree, @cStr, @whitespace_cb() ) ) > 0
      OutStd( cStr + hb_eol() )
   ELSE
      OutErr( cStr + hb_eol() )
      OutErr( hb_strFormat( "mxmlSaveString() = %d", nNum ) + hb_eol() )
   ENDIF

   mxmlDelete( hTree )

   RETURN

FUNCTION type_cb( hNode )

   LOCAL cType

   IF Empty( cType := mxmlElementGetAttr( hNode, "type" ) )
      cType := mxmlGetElement( hNode )
   ENDIF

   SWITCH Lower( cType )
      CASE "integer" ;  RETURN MXML_INTEGER
      CASE "opaque"  ;  RETURN MXML_OPAQUE
      CASE "real"    ;  RETURN MXML_REAL
   ENDSWITCH

   RETURN MXML_TEXT

FUNCTION whitespace_cb( hNode, nWhere )   /* O - Whitespace string or nil */
                                          /* I - Element node */
                                          /* I - Open or close tag? */

   LOCAL hParent                          /* Parent node */
   LOCAL nLevel                           /* Indentation level */
   LOCAL cName                            /* Name of element */

   /*
    * We can conditionally break to a new line before or after any element.
    * These are just common HTML elements...
    */

   cName := Lower( mxmlGetElement( hNode ) )

   IF cName == "html" .OR. cName == "head" .OR. cName == "body" .OR.                                  ;
      cName == "pre" .OR. cName == "p" .OR.                                                           ;
      cName == "h1" .OR. cName == "h2" .OR. cName == "h3" .OR.                                        ;
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
   ELSEIF Left( cName, 4 ) == "?xml"
      IF nWhere == MXML_WS_AFTER_OPEN
         RETURN hb_eol()
      ELSE
         RETURN nil
      ENDIF
   ELSEIF nWhere == MXML_WS_BEFORE_OPEN .OR.                                                          ;
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

      RETURN Space( nLevel * 8 )
   ELSEIF nWhere == MXML_WS_AFTER_CLOSE .OR.                                                          ;
          ( ( cName == "group" .OR. cName == "option" .OR. cName == "choice" ) .AND.                  ;
              nWhere == MXML_WS_AFTER_OPEN )

      RETURN hb_eol()
   ELSEIF nWhere == MXML_WS_AFTER_OPEN .AND. Empty( mxmlGetFirstChild( hNode ) )
      RETURN hb_eol()
   ENDIF

   /*
    * Return NULL for no added whitespace...
    */

   RETURN nil
