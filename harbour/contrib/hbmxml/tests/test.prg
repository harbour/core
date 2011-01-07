/*
 * $Id$
 */

#include "hbmxml.ch"
#include "fileio.ch"
#include "hbinkey.ch"

FUNCTION main( cFile )

   LOCAL tree
   LOCAL node
   LOCAL index
   LOCAL nType, i, nLen
   LOCAL cBuffer
   LOCAL wt := 1
   LOCAL nFile
   LOCAL tmp

   OutStd( hb_mxmlVersion(), hb_eol() )

   mxmlSetErrorCallback( @my_mxmlError() )

  /*
   * Test the basic functionality...
   */

   IF ! hb_FileExists( cFile )
      cFile := hb_dirBase() + "test.xml"
   ENDIF

   IF ! hb_FileExists( cFile )
      OutStd( "Usage: test filename.xml" )

      RETURN  1
   ENDIF

   /* parent node */
   tree := mxmlNewElement( MXML_NO_PARENT, "element" )
   IF Empty( tree )
      OutErr( "ERROR: No parent node in basic test!" )

      RETURN -1
   ENDIF

   nType := mxmlGetType( tree )
   IF nType != MXML_ELEMENT
      OutErr( "ERROR: Parent has type ", Type2Text( nType ), ", expected MXML_ELEMENT!" )
      mxmlDelete( tree )

      RETURN -1

   ELSEIF mxmlGetElement( tree ) != "element"
      OutErr( "ERROR: Parent value is ", mxmlGetElement( tree ), ', expected "element"' )
      mxmlDelete( tree )

      RETURN -1
   ENDIF

   mxmlNewInteger( tree, 123 )
   mxmlNewOpaque( tree, "opaque" )
   mxmlNewReal( tree, 123.49 )
   mxmlNewText( tree, 1 , "text" )

   mxmlLoadString( tree, "<group type='string'>string string string</group>", ;
      MXML_TEXT_CALLBACK )
   mxmlLoadString( tree, "<group type='integer'>1 2 3</group>", MXML_INTEGER_CALLBACK )
   mxmlLoadString( tree, "<group type='real'>1.0 2.0 3.0</group>", MXML_REAL_CALLBACK )
   mxmlLoadString( tree, "<group>opaque opaque opaque</group>", MXML_OPAQUE_CALLBACK )
   mxmlLoadString( tree, "<foo><bar><one><two>value<two>value2</two></two></one></bar></foo>", ;
      MXML_OPAQUE_CALLBACK )

   /* 1st node */
   node := mxmlGetFirstChild( tree )
   IF Empty( node )
      OutErr( "ERROR: No first child node in basic test!" )
      mxmlDelete( tree )

      RETURN -1
   ENDIF

   nType := mxmlGetType( node )
   IF nType != MXML_INTEGER
      OutErr( "ERROR: First child has type ", Type2Text( nType ), ", expected MXML_INTEGER!" )
      mxmlDelete( tree )

      RETURN -1

   ELSEIF mxmlGetInteger( node ) != 123
      OutErr( "ERROR: First child value is ", mxmlGetInteger( node ), ", expected 123!" )
      mxmlDelete( tree )

      RETURN -1
   ENDIF

   /* 2nd node */
   node := mxmlGetNextSibling( node )
   IF Empty( node )
      OutErr( "ERROR: No second child node in basic test!" )
      mxmlDelete( tree )

      RETURN -1
   ENDIF

   nType := mxmlGetType( node )
   IF nType != MXML_OPAQUE
      OutErr( "ERROR: Second child has type ", Type2Text( nType ), ", expected MXML_OPAQUE!" )
      mxmlDelete( tree )

      RETURN -1

   ELSEIF mxmlGetOpaque( node ) != "opaque"
      OutErr( "ERROR: Second child value is ", mxmlGetOpaque( node ), ", expected 'opaque'!" )
      mxmlDelete( tree )

      RETURN -1
   ENDIF

   /* 3rd node */
   node := mxmlGetNextSibling( node )
   IF Empty( node )
      OutErr( "ERROR: No third child node in basic test!" )
      mxmlDelete( tree )

      RETURN -1
   ENDIF

   nType := mxmlGetType( node )
   IF nType != MXML_REAL
      OutErr( "ERROR: Third child has type ", Type2Text( nType ), ", expected MXML_REAL!" )
      mxmlDelete( tree )

      RETURN -1

   ELSEIF mxmlGetReal( node ) != 123.49
      OutErr( "ERROR: Third child value is ", mxmlGetReal( node ), ", expected 123.49!" )
      mxmlDelete( tree )

      RETURN -1
   ENDIF

   /* 4th node */
   node := mxmlGetNextSibling( node )
   IF Empty( node )
      OutErr( "ERROR: No fourth child node in basic test!" )
      mxmlDelete( tree )

      RETURN -1
   ENDIF

   nType := mxmlGetType( node )
   IF nType != MXML_TEXT
      OutErr( "ERROR: Fourth child has type ", Type2Text( nType ), ", expected MXML_TEXT!" )
      mxmlDelete( tree )

      RETURN -1

   ELSEIF mxmlGetText( node, @wt ) != "text" .or. wt != 1
      OutErr( "ERROR: Fourth child value is ", mxmlGetText( node ), ", expected 'text'!" )
      mxmlDelete( tree )

      RETURN -1
   ENDIF

   FOR i := 1 TO 4
      node := mxmlGetNextSibling( node )

      IF Empty( node )
         OutErr( "ERROR: No group", hb_ntos(i), "child node in basic test!" )
         mxmlDelete( tree )

         RETURN -1

      ELSEIF mxmlGetType( node ) != MXML_ELEMENT
         OutErr( "ERROR: Group child", i, "has type ", Type2Text( nType ), ", expected MXML_ELEMENT!" )
         mxmlDelete( tree )

         RETURN -1
      ENDIF
   NEXT

  /*
   * Test mxmlFindPath...
   */

   node := mxmlFindPath( tree, "*/two" )
   IF Empty( node )
      OutErr( "ERROR: Unable to find value for '*/two'" )
      mxmlDelete( tree )

      RETURN -2
   ELSEIF mxmlGetType( node ) != MXML_OPAQUE .OR. mxmlGetOpaque( node ) != "value"
      OutErr( "ERROR: Bad value for '*/two'" )
      mxmlDelete( tree )

      RETURN -2
   ENDIF

   node := mxmlFindPath( tree, "foo/*/two" )
   IF Empty( node )
      OutErr( "ERROR: Unable to find value for 'foo/*/two'" )
      mxmlDelete( tree )

      RETURN -2
   ELSEIF mxmlGetType( node ) != MXML_OPAQUE .OR. mxmlGetOpaque( node ) != "value"
      OutErr( "ERROR: Bad value for 'foo/*/two'" )
      mxmlDelete( tree )

      RETURN -2
   ENDIF

   node := mxmlFindPath( tree, "foo/bar/one/two" )
   IF Empty( node )
      OutErr( "ERROR: Unable to find value for 'foo/bar/one/two'" )
      mxmlDelete( tree )

      RETURN -2
   ELSEIF mxmlGetType( node ) != MXML_OPAQUE .OR. mxmlGetOpaque( node ) != "value"
      OutErr( "ERROR: Bad value for 'foo/bar/one/two'" )
      mxmlDelete( tree )

      RETURN -2
   ENDIF

  /*
   * Test indices...
   */

   OutStd( "Try to create index of all nodes", hb_eol() )
   index := mxmlIndexNew( tree )

   IF Empty( index )
      OutErr( "ERROR: Unable to create index of all nodes!" )
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   i := mxmlIndexGetCount( index )
   IF i != 10
      OutErr( "ERROR: Index of all nodes contains ", i, "nodes; expected 10!" )
      index := NIL
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   mxmlIndexReset( index )
   IF Empty( mxmlIndexFind( index, "group" ) )
      OutErr( 'ERROR: mxmlIndexFind for "group" failed!' )
      index := NIL
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   index := NIL

   OutStd( "Try to create index of groups", hb_eol() )
   index := mxmlIndexNew( tree, "group" )
   IF Empty( index )
      OutErr( "ERROR: Unable to create index of groups!" )
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   i := mxmlIndexGetCount( index )
   IF i != 4
      OutErr( "ERROR: Index of groups contains ", i, "nodes; expected 4!" )
      index := NIL
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   mxmlIndexReset( index )
   IF Empty( mxmlIndexEnum( index ) )
      OutErr( "ERROR: mxmlIndexEnum failed!" )
      index := NIL
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   index := NIL

   OutStd( "Try to create index of type attributes", hb_eol() )
   index := mxmlIndexNew( tree, , "type" )
   IF Empty( index )
      OutErr( "ERROR: Unable to create index of type attributes!" )
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   i := mxmlIndexGetCount( index )
   IF i != 3
      OutErr( "ERROR: Index of type attributes contains ", i, "nodes; expected 3!" )
      index := NIL
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   mxmlIndexReset( index )
   IF Empty( mxmlIndexFind( index, "group" ) )
      OutErr( 'ERROR: mxmlIndexFind for "string" failed!' )
      index := NIL
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   index := NIL

   OutStd( "Try to create index of elements and attributes", hb_eol() )
   index := mxmlIndexNew( tree, "group", "type" )
   IF Empty( index )
      OutErr( "ERROR: Unable to create index of elements and attributes!" )
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   i := mxmlIndexGetCount( index )
   IF i != 3
      OutErr( "ERROR: Index of elements and attributes contains contains ", i, "nodes; expected 3!" )
      index := NIL
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   mxmlIndexReset( index )
   IF Empty( mxmlIndexFind( index, "group", "string" ) )
      OutErr( 'ERROR: mxmlIndexFind for "string" failed!' )
      index := NIL
      mxmlDelete( tree )

      RETURN -3
   ENDIF

   index := NIL

  /*
   * Check the mxmlDelete() works properly...
   */

   FOR i := 1 TO 9
      node := mxmlGetFirstChild( tree )
      IF ! Empty( node )
         mxmlDelete( node )
      ELSE
         OutErr( "ERROR: Child pointer prematurely NULL on child ", hb_ntos( i ) )
         mxmlDelete( tree )

         RETURN -4
      ENDIF
   NEXT

   IF ! Empty( mxmlGetFirstChild( tree ) )
      OutErr( "ERROR: Child pointer not NULL after deleting all children!" )
      mxmlDelete( tree )

      RETURN -4
   ENDIF

   IF ! Empty( mxmlGetLastChild( tree ) )
      OutErr( "ERROR: Last child pointer not NULL after deleting all children!" )
      mxmlDelete( tree )

      RETURN -4
   ENDIF

   mxmlDelete( tree )

  /*
   * Verify that mxmlFindElement() and indirectly mxmlWalkNext() work
   * properly...
   */

   tree := mxmlLoadString( NIL, hb_memoRead( cFile ), @type_cb() )

   node := mxmlFindElement( tree, tree, "choice", NIL, NIL, MXML_DESCEND )
   IF Empty( node )
      OutErr( "Unable to find first <choice> element in XML tree!" )
      mxmlDelete( tree )

      RETURN -5
   ENDIF

   IF Empty( node := mxmlFindElement( node, tree, "choice", NIL, NIL, MXML_NO_DESCEND ) )
      OutErr( "Unable to find second <choice> element in XML tree!" )
      mxmlDelete( tree )

      RETURN -5
   ENDIF

   mxmlRemove( node )

  /*
   * Save & Print the XML tree...
   */

   cBuffer := ""
   nLen := mxmlSaveString( tree, @cBuffer, @whitespace_cb() )
   cBuffer := Space( abs( nLen ) )

   IF ( mxmlSaveString( tree, @cBuffer, @whitespace_cb() ) > 0 )
      hb_memoWrit( "test2.xml", cBuffer + hb_eol() )
   ENDIF

   OutStd( cBuffer + hb_eol() )
   mxmlDelete( tree )

   cBuffer := mxmlSaveAllocString( node, 0 ) 
   IF Len( cBuffer ) > 0
      hb_memoWrit( "test3.xml", cBuffer + hb_eol() )
   ENDIF
   
   IF mxmlSaveFile( node, "test4.xml" ) < 0
      OutErr( "ERROR: Can't execute mxmlSaveFile()!", hb_eol() )
   ENDIF

   tmp := mxmlNewXML()
   mxmlSaveFile( mxmlLoadFile( tmp, "test4.xml", @type_cb() ), ;
                 "test5.xml", @whitespace_cb() )
   tmp := NIL

   IF mxmlSetUserData( node, wt ) > -1
      OutStd( mxmlGetUserData( node ), hb_eol() )
   ENDIF
   
   mxmlDelete( node )

   OutStd( "--- The End! ---", hb_eol() )

   RETURN 0

PROCEDURE my_mxmlError( cErrorMsg )

   OutErr( cErrorMsg, hb_eol() )

   RETURN

STATIC FUNCTION Type2Text( nType )

   LOCAL aTypes := { "MXML_ELEMENT", "MXML_INTEGER", "MXML_OPAQUE", "MXML_REAL", "MXML_TEXT" }

   RETURN iif( nType < MXML_ELEMENT .OR. nType > MXML_TEXT, "UNKNOWN", aTypes[ nType + 1 ] )

/* mxmlLoadString */

FUNCTION type_cb( node )

   LOCAL nResult
   LOCAL cType

   /* You can lookup attributes and/or use the element name, hierarchy, etc... */
 
   IF Empty( cType := mxmlElementGetAttr( node, "type" ) )
      cType := mxmlGetElement( node )
   ENDIF

   SWITCH Lower( cType )
   CASE "integer"
      nResult := MXML_INTEGER
      EXIT

   CASE "opaque"
   CASE "pre"
      nResult := MXML_OPAQUE
      EXIT

   CASE "real"
      nResult := MXML_REAL
      EXIT

   OTHERWISE
      nResult := MXML_TEXT
      EXIT
   ENDSWITCH

   RETURN nResult

/* mxmlSaveString */

FUNCTION whitespace_cb( node, where )  

   LOCAL parent        /* Parent node */
   LOCAL nLevel := -1  /* Indentation level */
   LOCAL name          /* Name of element */

   name := mxmlGetElement( node )
   /* OutStd( name, hb_eol() ) */

  /*
   * We can conditionally break to a new line before or after any element.
   * These are just common HTML elements...
   */

   SWITCH Lower( name )
   CASE "html"
   CASE "head"
   CASE "body" 
   CASE "pre" 
   CASE "p"
   CASE "h1"
   CASE "h2"
   CASE "h3"
   CASE "h4"
   CASE "h5"
   CASE "h6"
      /* Newlines before open and after close... */
      IF where == MXML_WS_BEFORE_OPEN .OR. where == MXML_WS_AFTER_CLOSE
         RETURN hb_eol()
      ENDIF
      EXIT

   CASE "dl"
   CASE "ol"
   CASE "ul"
      /* Put a newline before and after list elements... */
      IF .t.
         RETURN hb_eol() /* Warning W0028 Unreachable code */
      ENDIF
      EXIT

   CASE "dd" 
   CASE "dt"
   CASE "li"
      /* Put a tab before <li>'s, <dd>'s, and <dt>'s, and a newline after them... */
      IF where == MXML_WS_BEFORE_OPEN
         RETURN Chr( HB_K_TAB )
      ELSEIF where == MXML_WS_AFTER_CLOSE
         RETURN hb_eol()
      ENDIF
      EXIT
   ENDSWITCH

   IF Left( name, 4 ) == "?xml"
      IF where == MXML_WS_AFTER_OPEN 
         RETURN hb_eol()
      ELSE
         RETURN NIL
      ENDIF

   ELSEIF where == MXML_WS_BEFORE_OPEN .OR. ;
          ( ( name == "choice" .OR. name == "option" ) .AND. ;
          where == MXML_WS_BEFORE_CLOSE )

      parent := mxmlGetParent( node )
      DO WHILE ! Empty( parent )
         nLevel++
         parent := mxmlGetParent( parent )
      ENDDO

      IF nLevel > 8 
         nLevel := 8
      ELSEIF nLevel < 0 
         nLevel := 0
      ENDIF

      RETURN Replicate( Chr( HB_K_TAB ), nLevel )

   ELSEIF where == MXML_WS_AFTER_CLOSE .OR. ;
          ( ( name == "group".OR. name == "option" .OR. name == "choice" ) .AND. ;
          where == MXML_WS_AFTER_OPEN )
      RETURN hb_eol()

   ELSEIF where == MXML_WS_AFTER_OPEN .AND. Empty( mxmlGetFirstChild( node ) )
      RETURN hb_eol()
   ENDIF

   /* Return NIL for no added whitespace... */

   RETURN NIL
 