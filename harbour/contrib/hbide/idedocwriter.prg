/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                               14Mar2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"

/*----------------------------------------------------------------------*/

#define buttonArgs_clicked                        101
#define buttonDesc_clicked                        102
#define buttonExample_clicked                     103
#define buttonTests_clicked                       104

#define buttonCloseArgs_clicked                   111
#define buttonCloseDesc_clicked                   112
#define buttonCloseExample_clicked                113
#define buttonCloseTests_clicked                  114

#define buttonLoadFromCurFunc_clicked             115

#define buttonClear_clicked                       116
#define buttonSaveInFunc_clicked                  117
#define buttonSave_clicked                        118


#define qqTemplate                                1
#define qqVersion                                 2
#define qqStatus                                  3
#define qqCompliance                              4
#define qqCategory                                5
#define qqSubCategory                             6
#define qqName                                    7
#define qqExtLink                                 8
#define qqOneLiner                                9
#define qqSyntax                                  10
#define qqReturns                                 11
#define qqSeeAlso                                 12
#define qqFiles                                   13
#define qqArgs                                    14
#define qqDesc                                    15
#define qqExamples                                16
#define qqTests                                   17

#define qqNumVrbls                                17

/*----------------------------------------------------------------------*/

FUNCTION hbide_getSVNHeader()

   RETURN "/* " + hb_eol() + " * $Id:" + hb_eol() + " */" + hb_eol() + hb_eol()

/*----------------------------------------------------------------------*/

FUNCTION hbide_populateParam( txt_, cToken, cParam )
   LOCAL a_
   IF !empty( cParam )
      aadd( txt_, cToken )
      a_:= hbide_memoToArray( cParam )
      aeval( a_, {|e| aadd( txt_, " *      " + strtran( strtran( e, chr( 13 ), "" ), chr( 10 ), "" ) ) } )
   ENDIF
   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION hbide_ar2paramList( aArg )
   LOCAL s, cList := ""
   FOR EACH s IN aArg
      s := alltrim( s )
      cList += s + iif( s:__enumIndex() < Len( aArg ), ", ", "" )
   NEXT
   RETURN cList

/*----------------------------------------------------------------------*/

FUNCTION hbide_arg2memo( aArg )
   LOCAL s, cMemo := ""

   FOR EACH s IN aArg
      cMemo += "<" + s + ">" + iif( s:__enumIndex() < Len( aArg ), hb_eol(), "" )
   NEXT

   RETURN cMemo

/*----------------------------------------------------------------------*/

CLASS IdeDocWriter INHERIT IdeObject

   DATA   qHiliter
   DATA   qHiliter1

   DATA   oEdit
   DATA   cFuncPtoto                              INIT ""
   DATA   nFuncLine                               INIT 0
   DATA   nTagsIndex                              INIT 0
   DATA   cSourceFile                             INIT ""

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD execEvent( nMode, p )
   METHOD setImages()
   METHOD installSignals()
   METHOD setParameters()
   METHOD loadCurrentFuncDoc()
   METHOD parsePrototype( cProto )
   METHOD clear()
   METHOD fillForm( aFacts )
   METHOD fillFormByObject( oFunc )
   METHOD buildDocument()
   METHOD saveInFunction()
   METHOD saveInFile()
   METHOD pullDocFromSource( nLineFrom, oEdit )
   METHOD removeDocHelp( nLineFrom, oEdit )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:destroy()

   ::oEdit := NIL

   IF !empty( ::oUI )
      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:show()

   IF empty( ::oUI )
      ::oUI := hbide_getUI( "docwriter" )

      ::oDocWriteDock:oWidget:setWidget( ::oUI:oWidget )

      ::setImages()
      ::installSignals()
      ::setParameters()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:setImages()

   ::oUI:buttonLoadFromDocFile :setIcon( QIcon( hbide_image( "load_3"      ) ) )
   ::oUI:buttonLoadFromSource  :setIcon( QIcon( hbide_image( "load_2"      ) ) )
   ::oUI:buttonLoadFromCurFunc :setIcon( QIcon( hbide_image( "load_1"      ) ) )

   ::oUI:buttonArgs            :setIcon( QIcon( hbide_image( "arguments"   ) ) )
   ::oUI:buttonDesc            :setIcon( QIcon( hbide_image( "description" ) ) )
   ::oUI:buttonExamples        :setIcon( QIcon( hbide_image( "example"     ) ) )
   ::oUI:buttonTests           :setIcon( QIcon( hbide_image( "tests"       ) ) )

   ::oUI:buttonClear           :setIcon( QIcon( hbide_image( "clean"       ) ) )
   ::oUI:buttonSaveInFunc      :setIcon( QIcon( hbide_image( "unload_1"    ) ) )
   ::oUI:buttonSave            :setIcon( QIcon( hbide_image( "helpdoc"     ) ) )

   ::oUI:buttonCloseArgs       :setIcon( QIcon( hbide_image( "closetab"    ) ) )
   ::oUI:buttonCloseDesc       :setIcon( QIcon( hbide_image( "closetab"    ) ) )
   ::oUI:buttonCloseExamples   :setIcon( QIcon( hbide_image( "closetab"    ) ) )
   ::oUI:buttonCloseTests      :setIcon( QIcon( hbide_image( "closetab"    ) ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:installSignals()

   ::oUI:buttonArgs           :connect( "toggled(bool)", {|p| ::execEvent( buttonArgs_clicked        , p ) } )
   ::oUI:buttonDesc           :connect( "toggled(bool)", {|p| ::execEvent( buttonDesc_clicked        , p ) } )
   ::oUI:buttonExamples       :connect( "toggled(bool)", {|p| ::execEvent( buttonExample_clicked     , p ) } )
   ::oUI:buttonTests          :connect( "toggled(bool)", {|p| ::execEvent( buttonTests_clicked       , p ) } )
   ::oUI:buttonCloseArgs      :connect( "clicked()"    , {| | ::execEvent( buttonCloseArgs_clicked       ) } )
   ::oUI:buttonCloseDesc      :connect( "clicked()"    , {| | ::execEvent( buttonCloseDesc_clicked       ) } )
   ::oUI:buttonCloseExamples  :connect( "clicked()"    , {| | ::execEvent( buttonCloseExample_clicked    ) } )
   ::oUI:buttonCloseTests     :connect( "clicked()"    , {| | ::execEvent( buttonCloseTests_clicked      ) } )
   ::oUI:buttonClear          :connect( "clicked()"    , {| | ::execEvent( buttonClear_clicked           ) } )
   ::oUI:buttonSaveInFunc     :connect( "clicked()"    , {| | ::execEvent( buttonSaveInFunc_clicked      ) } )
   ::oUI:buttonSave           :connect( "clicked()"    , {| | ::execEvent( buttonSave_clicked            ) } )
   ::oUI:buttonLoadFromCurFunc:connect( "clicked()"    , {|| ::execEvent( buttonLoadFromCurFunc_clicked  ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:setParameters()

   ::oUI:buttonArgs    :setCheckable( .t. )
   ::oUI:buttonDesc    :setCheckable( .t. )
   ::oUI:buttonExamples:setCheckable( .t. )
   ::oUI:buttonTests   :setCheckable( .t. )

   ::oUI:buttonArgs    :setChecked( .t. )
   ::oUI:buttonDesc    :setChecked( .t. )
   ::oUI:buttonExamples:setChecked( .f. )
   ::oUI:buttonTests   :setChecked( .f. )

   ::oUI:frameTests:hide()
   ::oUI:frameExamples:hide()

   ::oUI:comboTemplate:addItem( "Function"  )
   ::oUI:comboTemplate:addItem( "Procedure" )
   ::oUI:comboTemplate:addItem( "Class"     )

   ::qHiliter  := ::oTH:SetSyntaxHilighting( ::oUI:plainExamples, "Pritpal's Favourite" )
   ::qHiliter1 := ::oTH:SetSyntaxHilighting( ::oUI:plainTests   , "Evening Glamour"     )

   ::oUI:plainExamples:setFont( ::oFont:oWidget )
   ::oUI:plainTests:setFont( ::oFont:oWidget )

   ::oUI:frameGeneral:setSizePolicy( QSizePolicy_Preferred, QSizePolicy_Fixed )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:execEvent( nMode, p )

   IF ::lQuitting
      RETURN Self
   ENDIF

   SWITCH nMode
   CASE buttonArgs_clicked
      IF p
         ::oUI:frameArgs:show()
      ELSE
         ::oUI:frameArgs:hide()
      ENDIF
      EXIT
   CASE buttonDesc_clicked
      IF p
         ::oUI:frameDesc:show()
      ELSE
         ::oUI:frameDesc:hide()
      ENDIF
      EXIT
   CASE buttonExample_clicked
      IF p
         ::oUI:frameExamples:show()
      ELSE
         ::oUI:frameExamples:hide()
      ENDIF
      EXIT
   CASE buttonTests_clicked
      IF p
         ::oUI:frameTests:show()
      ELSE
         ::oUI:frameTests:hide()
      ENDIF
      EXIT

   CASE buttonCloseArgs_clicked
      ::oUI:buttonArgs:setChecked( .f. )
      EXIT
   CASE buttonCloseDesc_clicked
      ::oUI:buttonDesc:setChecked( .f. )
      EXIT
   CASE buttonCloseExample_clicked
      ::oUI:buttonExamples:setChecked( .f. )
      EXIT
   CASE buttonCloseTests_clicked
      ::oUI:buttonTests:setChecked( .f. )
      EXIT

   CASE buttonLoadFromCurFunc_clicked
      ::loadCurrentFuncDoc()
      EXIT
   CASE buttonClear_clicked
      ::clear()
      EXIT
   CASE buttonSaveInFunc_clicked
      ::saveInFunction()
      EXIT
   CASE buttonSave_clicked
      ::saveInFile()
      EXIT

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:clear()

   ::oEdit       := NIL
   ::cFuncPtoto  := ""
   ::nFuncLine   := 0
   ::nTagsIndex  := 0
   ::cSourceFile := ""

   ::fillForm( afill( array( qqNumVrbls ), "" ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:fillForm( aFacts )

   ::oUI:editVersion     :setText      ( aFacts[ qqVersion     ] )
   ::oUI:editStatus      :setText      ( aFacts[ qqStatus      ] )
   ::oUI:editCompliance  :setText      ( aFacts[ qqCompliance  ] )
   ::oUI:editCategory    :setText      ( aFacts[ qqCategory    ] )
   ::oUI:editSubCategory :setText      ( aFacts[ qqSubCategory ] )
   ::oUI:editName        :setText      ( aFacts[ qqName        ] )
   ::oUI:editExtLink     :setText      ( aFacts[ qqExtLink     ] )
   ::oUI:editOneLiner    :setText      ( aFacts[ qqOneLiner    ] )
   ::oUI:editSyntax      :setText      ( aFacts[ qqSyntax      ] )
   ::oUI:editReturns     :setText      ( aFacts[ qqReturns     ] )
   ::oUI:editSeeAlso     :setText      ( aFacts[ qqSeeAlso     ] )
   ::oUI:editFiles       :setText      ( aFacts[ qqFiles       ] )
   ::oUI:plainArgs       :setPlainText ( aFacts[ qqArgs        ] )
   ::oUI:plainDesc       :setPlainText ( aFacts[ qqDesc        ] )
   ::oUI:plainExamples   :setPlainText ( aFacts[ qqExamples    ] )
   ::oUI:plainTests      :setPlainText ( aFacts[ qqTests       ] )

   ::oUI:comboTemplate:setCurrentIndex( iif( aFacts[ qqVersion ] == "Procedure", 1, ;
                                          iif( aFacts[ qqVersion ] == "Class", 2, 0 ) ) )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:fillFormByObject( oFunc )

   ::oUI:editVersion     :setText      ( oFunc:cVersion      )
   ::oUI:editStatus      :setText      ( oFunc:cStatus       )
   ::oUI:editCompliance  :setText      ( oFunc:cPlatForms    )
   ::oUI:editCategory    :setText      ( oFunc:cCategory     )
   ::oUI:editSubCategory :setText      ( oFunc:cSubCategory  )
   ::oUI:editName        :setText      ( oFunc:cName         )
   ::oUI:editExtLink     :setText      ( oFunc:cExternalLink )
   ::oUI:editOneLiner    :setText      ( oFunc:cOneLiner     )
   ::oUI:editSyntax      :setText      ( hbide_ar2delString( oFunc:aSyntax , "; " ) )
   ::oUI:editReturns     :setText      ( hbide_ar2delString( oFunc:aReturns, "; " ) )
   ::oUI:editSeeAlso     :setText      ( oFunc:cSeeAlso      )
   ::oUI:editFiles       :setText      ( hbide_ar2delString( oFunc:aFiles  , "; " ) )
   ::oUI:plainArgs       :setPlainText ( hbide_arrayTOmemo( oFunc:aArguments    ) )
   ::oUI:plainDesc       :setPlainText ( hbide_arrayTOmemo( oFunc:aDescription  ) )
   ::oUI:plainExamples   :setPlainText ( hbide_arrayTOmemo( oFunc:aExamples     ) )
   ::oUI:plainTests      :setPlainText ( hbide_arrayTOmemo( oFunc:aTests        ) )

   ::oUI:comboTemplate:setCurrentIndex( iif( oFunc:cTemplate == "Procedure", 1, ;
                                          iif( oFunc:cTemplate == "Class", 2, 0 ) ) )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:removeDocHelp( nLineFrom, oEdit )
   LOCAL i, cLine, n, lExists, lDone, nFrom, nTo

   n       := 0
   lExists := .f.
   lDone   := .f.

   FOR i := ( nLineFrom - 1 ) TO 1 STEP -1
      cLine := oEdit:getLine( i )

      IF "$END$" $ cLine
         nTo := i + 1
         lExists := .t.
      ENDIF
      IF "$DOC$" $ cLine
         lDone := .t.
         nFrom := i
      ENDIF
      IF ++n > 4 .AND. ! lExists
         EXIT
      ENDIF
      IF lDone
         EXIT
      ENDIF
   NEXT

   IF !empty( nFrom ) .AND. !empty( nTo )
      oEdit:goto( nFrom )
      FOR i := 1 TO ( nTo - nFrom + 1 )
         oEdit:deleteLine()
      NEXT
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:pullDocFromSource( nLineFrom, oEdit )
   LOCAL aDoc, i, cLine, n, lExists, lDone, a_

   aDoc    := {}
   n       := 0
   lExists := .f.
   lDone   := .f.

   FOR i := ( nLineFrom - 1 ) TO 1 STEP -1
      cLine := oEdit:getLine( i )

      IF "$END$" $ cLine
         lExists := .t.
      ENDIF
      IF "$DOC$" $ cLine
         lDone := .t.
      ENDIF

      IF lExists
         aadd( aDoc, cLine )
      ENDIF

      IF ++n > 4 .AND. ! lExists /* Search must terminate after 4 lines if document is not in sight */
         EXIT
      ENDIF
      IF lDone
         EXIT
      ENDIF
   NEXT

   a_:={}
   IF lDone
      FOR i := Len( aDoc ) TO 1 STEP -1
         aadd( a_, aDoc[ i ] )
      NEXT
   ENDIF

   RETURN a_

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:loadCurrentFuncDoc()
   LOCAL oEdit, nCurLine, n, cProto, nProtoLine, aFacts, aDoc, oFunc

   IF !empty( oEdit := ::oEM:getEditObjectCurrent() )
      IF oEdit:isModified()
         MsgBox( oEdit:oEditor:sourceFile + " is modified.", "Please save the source first" )
         RETURN Self
      ENDIF

      IF !empty( ::aTags )
         nCurLine := oEdit:getLineNo()
         IF Len( ::aTags ) == 1
            n := 1
         ELSEIF ( n := ascan( ::aTags, {|e_| e_[ 3 ] >= nCurLine } ) ) == 0
            n := Len( ::aTags )
         ELSEIF n > 0
            n--
         ENDIF
         IF n > 0
            nProtoLine := ::aTags[ n, 3 ]
            cProto := oEdit:getLine( nProtoLine )

            IF !empty( aFacts := ::parsePrototype( cProto ) )
               ::clear()
               ::oEdit       := oEdit
               ::cFuncPtoto  := cProto
               ::nFuncLine   := nProtoLine
               ::nTagsIndex  := n
               ::cSourceFile := oEdit:oEditor:sourceFile
               IF empty( aDoc := ::pullDocFromSource( nProtoLine, oEdit ) )
                  ::fillForm( aFacts )
               ELSE
                  IF !empty( oFunc := ::oHL:getDocFunction( aDoc ) )
                     ::fillFormByObject( oFunc )
                  ELSE
                     ::fillForm( aFacts )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:parsePrototype( cProto )
   LOCAL aFacts, n, n1, cPre, cArg, aArg, cSyn, cTpl, cFun, s

   IF ( n := at( "(", cProto ) ) > 0
      IF ( n1 := at( ")", cProto ) ) > 0
         cPre := alltrim( substr( cProto, 1, n - 1 ) )
         cArg := alltrim( substr( cProto, n + 1, n1 - n - 1 ) )
         aArg := hb_aTokens( cArg, "," )
         FOR EACH s IN aArg
            s := alltrim( s )
         NEXT
         n := rat( " ", cPre )                       /* and it must be */
         cTpl := alltrim( substr( cPre, 1, n - 1 ) )
         cFun := alltrim( substr( cPre, n + 1 ) )

         cSyn := cFun + "( " + hbide_ar2paramList( aArg ) + " )"
         cSyn := strtran( cSyn, "(  )", "()" )

         aFacts := afill( array( qqNumVrbls ), "" )
         cTpl   := lower( cTpl )
         aFacts[ qqTemplate    ] := iif( "func"  $ cTpl, "Function" , ;
                                    iif( "proc"  $ cTpl, "Procedure", ;
                                    iif( "class" $ cTpl, "Class"    , "Function" ) ) )

         aFacts[ qqVersion     ] := ""
         aFacts[ qqStatus      ] := ""
         aFacts[ qqCompliance  ] := ""
         aFacts[ qqCategory    ] := ""
         aFacts[ qqSubCategory ] := ""
         aFacts[ qqName        ] := upper( cFun ) + "()"
         aFacts[ qqExtLink     ] := ""
         aFacts[ qqOneLiner    ] := ""
         aFacts[ qqSyntax      ] := cSyn
         aFacts[ qqReturns     ] := ""
         aFacts[ qqSeeAlso     ] := ""
         aFacts[ qqFiles       ] := ""
         aFacts[ qqArgs        ] := hbide_arg2memo( aArg )
         aFacts[ qqDesc        ] := ""
         aFacts[ qqExamples    ] := ""
         aFacts[ qqTests       ] := ""

      ENDIF
   ENDIF

   RETURN aFacts

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:saveInFile()
   LOCAL cFile, cBuffer
   LOCAL txt_    := ::buildDocument()
   LOCAL n       := ::oUI:comboTemplate:currentIndex()
   LOCAL cPrefix := iif( n == 0, "fun_", iif( n == 1, "proc_", "class_" ) )
   LOCAL cName   := lower( ::oUI:editName:text() )

   cName := strtran( cName, "(", "" )
   cName := strtran( cName, ")", "" )
   cFile := cPrefix + alltrim( cName ) + ".txt"

   cFile := hbide_saveAFile( ::oDlg, "Provide filename to save documentation", ;
                                 { { "Harbour Documentation File", "*.txt" } }, cFile, "txt" )
   IF !empty( cFile )
      cBuffer := hb_memoread( cFile )
      cBuffer := iif( "$Id:" $ cBuffer, cBuffer, hbide_getSVNHeader() + cBuffer )
      cBuffer += hb_eol()
      cBuffer += hbide_arrayToMemo( txt_ )

      hb_memowrit( cFile, cBuffer )
      MsgBox( cFile + " : is saved", "Save File Alert" )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:saveInFunction()
   LOCAL nCurLine, oEdit, qCursor, a_

   /* Bring it on top and make it current */
   ::oSM:editSource( ::cSourceFile, , , , , , .f. )

   IF !empty( oEdit := ::oEM:getEditObjectCurrent() )
      IF oEdit:lReadOnly
         RETURN Self
      ENDIF

      IF oEdit:isModified()
         MsgBox( oEdit:oEditor:sourceFile + " is modified.", "Please save the source first!" )
         RETURN Self
      ENDIF
      IF oEdit:find( ::cFuncPtoto, 0 )
         nCurLine := oEdit:getLineNo()
         IF nCurLine != ::nFuncLine
            // This is possible user might have edited the source; just issue warning
            MsgBox( "Source is modified, anyway proceeding.", "Documentation Save Alert" )
         ENDIF

         qCursor := oEdit:qEdit:textCursor()
         qCursor:beginEditBlock()

         ::removeDocHelp( nCurLine, oEdit )

         a_:= ::buildDocument()

         oEdit:home()
         oEdit:insertText( a_ )

         qCursor:endEditBlock()
         oEdit:qEdit:setTextCursor( qCursor )
         oEdit:qEdit:centerCursor()
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDocWriter:buildDocument()
   LOCAL s
   LOCAL nIndex := ::oUI:comboTemplate:currentIndex()

   LOCAL hEntry := { => }

   hb_HKeepOrder( hEntry, .T. )

   hEntry[ "TEMPLATE"     ] := iif( nIndex == 2, "Class", iif( nIndex == 1, "Procedure", "Function" ) )
   IF !empty( s := ::oUI:editName:text() )
      hEntry[ "NAME"         ] := s
   ENDIF
   IF !empty( s := ::oUI:editCategory:text() )
      hEntry[ "CATEGORY"     ] := s
   ENDIF
   IF !empty( s := ::oUI:editSubCategory:text() )
      hEntry[ "SUBCATEGORY"  ] := s
   ENDIF
   IF !empty( s := ::oUI:editExtLink:text() )
      hEntry[ "EXTERNALLINK" ] := s
   ENDIF
   IF !empty( s := ::oUI:editOneLiner:text() )
      hEntry[ "ONELINER"     ] := s
   ENDIF
   IF !empty( s := ::oUI:editSyntax:text() )
      hEntry[ "SYNTAX"       ] := s
   ENDIF
   hEntry[ "ARGUMENTS"    ] := ::oUI:plainArgs:toPlainText()
   IF !empty( s := ::oUI:editReturns:text() )
      hEntry[ "RETURNS"      ] := s
   ENDIF
   hEntry[ "DESCRIPTION"  ] := ::oUI:plainDesc:toPlainText()
   hEntry[ "EXAMPLES"     ] := ::oUI:plainExamples:toPlainText()
   hEntry[ "TESTS"        ] := ::oUI:plainTests:toPlainText()
   IF !empty( s := ::oUI:editStatus:text() )
      hEntry[ "STATUS"       ] := s
   ENDIF
   IF !empty( s := ::oUI:editCompliance:text() )
      hEntry[ "PLATFORMS"    ] := s
   ENDIF
   IF !empty( s := ::oUI:editVersion:text() )
      hEntry[ "VERSION"      ] := s
   ENDIF
   IF !empty( s := ::oUI:editFiles:text() )
      hEntry[ "FILES"        ] := s
   ENDIF
   IF !empty( s := ::oUI:editSeeAlso:text() )
      hEntry[ "SEEALSO"      ] := s
   ENDIF

   RETURN __hbdoc_ToSource( { hEntry } )

/*----------------------------------------------------------------------*/
