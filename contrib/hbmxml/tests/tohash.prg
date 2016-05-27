/* Based on original code posted on this URL:
   https://github.com/vszakats/harbour-core/issues/181#issue-121482480 */

#require "hbmxml"

#include "simpleio.ch"

PROCEDURE Main( cFile )

   LOCAL pRoot := mxmlLoadString( , hb_MemoRead( hb_defaultValue( cFile, "test.xml" ) ), @s_type_cb() )

   ? hb_ValToExp( XMLToHash( pRoot, "group" ) )

   mxmlDelete( pRoot )

   RETURN

STATIC FUNCTION XMLToHash( pRoot, cElement )

   IF HB_ISSTRING( cElement )
      pRoot := mxmlFindElement( pRoot, pRoot, cElement,,, MXML_DESCEND )
   ENDIF

   RETURN iif( Empty( pRoot ), { => }, s_NodeToHash( mxmlWalkNext( pRoot, pRoot, MXML_DESCEND ) ) )

STATIC FUNCTION s_NodeToHash( pNode )

   LOCAL hNext
   LOCAL hHashChild
   LOCAL hHash := { => }

   DO WHILE ! Empty( pNode )

      IF mxmlGetType( pNode ) == MXML_ELEMENT
         IF hb_HHasKey( hHash, mxmlGetElement( pNode ) )
            IF ! HB_ISARRAY( hHash[ mxmlGetElement( pNode ) ] )
               hHash[ mxmlGetElement( pNode ) ] := mxmlGetOpaque( pNode )
            ENDIF
         ELSE
            hHash[ mxmlGetElement( pNode ) ] := mxmlGetOpaque( pNode )
         ENDIF

         IF Empty( mxmlGetOpaque( pNode ) )
            IF ! Empty( hNext := mxmlWalkNext( pNode, pNode, MXML_DESCEND ) )
               hHashChild := s_NodeToHash( hNext )
               IF hHashChild != NIL .AND. ! Empty( hHashChild )
                  IF Empty( hHash[ mxmlGetElement( pNode ) ] )
                     hHash[ mxmlGetElement( pNode ) ] := {}
                  ENDIF
                  IF hb_mxmlGetAttrsCount( pNode ) > 0
                     hHashChild[ mxmlGetElement( pNode ) + "@attr" ] := hb_mxmlGetAttrs( pNode )
                  ENDIF
                  AAdd( hHash[ mxmlGetElement( pNode ) ], hHashChild )
               ENDIF
            ELSEIF hb_mxmlGetAttrsCount( pNode ) > 0
               IF Empty( hHash[ mxmlGetElement( pNode ) ] )
                  hHash[ mxmlGetElement( pNode ) ] := {}
               ENDIF
               AAdd( hHash[ mxmlGetElement( pNode ) ], hb_mxmlGetAttrs( pNode ) )
            ENDIF
         ELSEIF hb_mxmlGetAttrsCount( pNode ) > 0
            hHash[ mxmlGetElement( pNode ) + "@attr" ] := hb_mxmlGetAttrs( pNode )
         ENDIF
      ENDIF

      pNode := mxmlGetNextSibling( pNode )
   ENDDO

   RETURN hHash

STATIC FUNCTION s_type_cb()
   RETURN MXML_OPAQUE
