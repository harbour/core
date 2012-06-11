/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2012 Carlos Bacco <carlosbacco at gmail.com>
 * www - http://harbour-project.org
 *
 */

#ifndef HB_QDBFBROWSER_PRG_
#define HB_QDBFBROWSER_PRG_

#include "hbclass.ch"


FUNCTION QDBFBrowser( ... )
RETURN B2_QDBFBrowser():new( ... ):init()

CREATE CLASS B2QDBFBrowser INHERIT HB_QTableView FUNCTION B2_QDBFBrowser
   METHOD init()
   METHOD attach()
   METHOD detach()
   METHOD destroy()

   VAR oModel
   VAR nArea  INIT 0
   VAR aStru

   VAR oItemDgt
   ENDCLASS

METHOD init()
   ::oModel := HBQAbstractItemModel( {| t, r, x, y| B2_QDBFBrowse( Self, t, r, x, y ) } )
   ::setModel( ::oModel )
   ::oItemDgt := ::itemDelegate()
   ::oItemDgt:connect( "commitData(QWidget*)", {|oWidget| B2_QDBFCommit( Self, oWidget ) } )
   RETURN Self

METHOD B2QDBFBrowser:destroy()
   ::oItemDgt:disconnect( "commitData(QWidget*)" )
   ::oModel:reset()
   ::nArea := 0
   IF Select() > 0
      DbCloseArea()
   ENDIF    
   RETURN NIL
   
METHOD B2QDBFBrowser:attach()
   IF ( ::nArea := Select() ) > 0
      ::aStru := DBStruct()
      ::oModel:reset()
   ENDIF
   RETURN NIL

METHOD B2QDBFBrowser:detach()
   ::nArea := 0
   ::oModel:reset()
   RETURN NIL

PROCEDURE B2_QDBFCommit( o, oWidget )
   LOCAL oIndex := o:currentIndex()
   LOCAL nCX := oIndex:column()
   LOCAL nCY := oIndex:row()
   LOCAL nOldArea := Select()
   LOCAL cData := oWidget:property( "text" ):toString()
   
   DBSelectArea( o:nArea )
   DBGoto( nCY + 1 )

   SWITCH o:aStru[ nCX + 1, 2 ]
   CASE "C"
      FieldPut( nCX + 1, AllTrim( cData ) )
      EXIT
   CASE "N"
      FieldPut( nCX + 1, Val( cData ) )
      EXIT
   CASE "L"
      FieldPut( nCX + 1, Left( cData, 1 ) $ "YyTt" )
      EXIT
   CASE "D"
      FieldPut( nCX + 1, CToD( cData ) )
      EXIT
   ENDSWITCH

   DBSelectArea( nOldArea )
   RETURN


FUNCTION B2_QDBFBrowse( o, nT, nRole, nX, nY )
   LOCAL xRet
   LOCAL nOldArea := Select()

   IF o:nArea == 0
      RETURN NIL
   ENDIF

   SWITCH nT
   CASE HBQT_QAIM_flags
      RETURN Qt_ItemIsEnabled + Qt_ItemIsSelectable + Qt_ItemIsEditable;

   CASE HBQT_QAIM_data

      SWITCH nRole
      CASE Qt_DisplayRole
         DBSelectArea( o:nArea )
         DBGoto( nY + 1 )
         
         xRet := "?"
         SWITCH o:aStru[ nX + 1, 2 ]
         CASE "C"
            xRet := AllTrim( FieldGet( nX + 1 ) )
            EXIT
         CASE "N"
            xRet := hb_NToS( FieldGet( nX + 1 ) )
            EXIT
         CASE "L"
            xRet := IIf( FieldGet( nX + 1 ), "Yes", "No" )
            EXIT
         CASE "D"
            xRet := DToC( FieldGet( nX + 1 ) )
            EXIT
         ENDSWITCH

         DBSelectArea( nOldArea )
         RETURN xRet

      CASE Qt_EditRole /* Here we can specify different formats for editing*/
         DBSelectArea( o:nArea )
         DBGoto( nY + 1 )
         
         xRet := ""
         SWITCH o:aStru[ nX + 1, 2 ]
         CASE "C"
            xRet := AllTrim( FieldGet( nX + 1 ) )
            EXIT
         CASE "N"
            xRet := hb_NToS( FieldGet( nX + 1 ) )
            EXIT
         CASE "L"
            xRet := IIf( FieldGet( nX + 1 ), "Y", "N" )
            EXIT
         CASE "D"
            xRet := DToC( FieldGet( nX + 1 ) )
            EXIT
         ENDSWITCH

         DBSelectArea( nOldArea )
         RETURN xRet

      CASE Qt_TextAlignmentRole
         SWITCH o:aStru[ nX + 1, 2 ]
         CASE "C"
            RETURN Qt_AlignVCenter + Qt_AlignLeft
         CASE "N"
            RETURN Qt_AlignVCenter + Qt_AlignRight
            
         ENDSWITCH
         RETURN Qt_AlignCenter
         
      ENDSWITCH
      RETURN NIL

   CASE HBQT_QAIM_headerData
      SWITCH nRole
      CASE Qt_DisplayRole
         IF nX == Qt_Horizontal
            RETURN o:aStru[ nY + 1, 1 ]
         ELSE
            RETURN hb_NToS( nY + 1 )
         ENDIF

      CASE Qt_TextAlignmentRole
         IF nX == Qt_Horizontal
            RETURN Qt_AlignCenter
         ELSE
            RETURN Qt_AlignVCenter + Qt_AlignRight
         ENDIF

      ENDSWITCH
      RETURN NIL

   CASE HBQT_QAIM_rowCount
      RETURN LastRec()

   CASE HBQT_QAIM_columnCount
      RETURN Len( o:aStru )
      
   ENDSWITCH

   RETURN NIL

#endif /* HB_QDBFBROWSER_CH_ */
