/*
 * Copyright 2006-2015 Pritpal Bedi <bedipritpal@hotmail.com>
 * Copyright 2006-2015 CURACAO - http://www.icuracao.com
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

#include "cacherdd.ch"
#include "cachemgr.ch"

#include "hbgtinfo.ch"
#include "common.ch"
#include "wvtwin.ch"
#include "wvttowvg.ch"
#include "inkey.ch"


#xtranslate uiGetSome(    =>                      VouGetSome(
#xtranslate uiGetSomeA(   =>                      VouGetSomeA(
#xtranslate uiSaveScrn(   =>                      VouSaveScrn(
#xtranslate uiRestScrn(   =>                      VouRestScrn(
#xtranslate uiDrawShadow( =>                      VouShadow(


#define ID_BTN_OK                                 1
#define ID_BTN_CANCEL                             2

#define ID_BTN_EXECUTE                            6
#define ID_BTN_SAVE                               7
#define ID_BTN_LOAD                               8
#define ID_BTN_SAVECSV                            9

#define ID_MLE                                    10
#define ID_CHK_SATIS                              11
#define ID_EDT_TIME                               51
#define ID_LST_LIST                               13
#define ID_CMB_COMBO                              31
#define ID_RDO_XH                                 21
#define ID_RDO_CLIP                               22
#define ID_RDO_XBASE                              23
#define ID_EDT_TEXT                               14
#define ID_EDT_NUMB                               15
#define ID_STA_TEXT                               71
#define ID_STA_IMAGE                              72
#define ID_ICO_VOUCH                              81
#define ID_EDT_CSV                                91
#define ID_EXE_EXCEL                              92

#define ID_GRP_COMP                               113

#define ID_MNU_FILE                               201
#define ID_MNU_CONTROL                            202

#define RC_LEFT                                   1
#define RC_TOP                                    2
#define RC_RIGHT                                  3
#define RC_BOTTOM                                 4


#define SQP_SQL                                   1
#define SQP_SAVE                                  2
#define SQP_SHOW                                  3
#define SQP_ASK                                   4
#define SQP_TABLE                                 5
#define SQP_ONSERVER                              6
#define SQP_PARAM                                 7

#define SQP_VRBLS                                 7


THREAD STATIC cQryText  := ""
THREAD STATIC lExeQuery := .F.
THREAD STATIC cCSVFile  := ""
THREAD STATIC lExeExcel := .F.

THREAD STATIC tlbr_:= {}


FUNCTION Sql_LoadCacheQueryOld()
   LOCAL cXls, lOpenExcel, i, nStart, nEnd, cSql, aResult, aStruct, bError
   LOCAL aRecords := {}
   LOCAL aAttr    := {}

   CLS

   cSql := alltrim( VouGetQuery( "Query" ) )
   IF Empty( cSql ) .OR. !( lExeQuery )
      RETURN NIL
   ENDIF
   IF lower( left( cSql, 6 ) ) <> "select"
      Alert( "Only select statement can be executed!" )
      RETURN NIL
   ENDIF

   CLS

   cXLS       := cCSVFile
   lOpenExcel := lExeExcel

   bError   := ErrorBlock( {|| break() } )

   BEGIN SEQUENCE
   nStart  := seconds()
   aResult := ExeCacheQueryEx( { cSql, .F., .f., .f., cXLS, .f., NIL }, @aRecords )
   nEnd    := seconds()

   IF aResult[ 8,8 ]
      IF aResult[ 8,06 ] > 0
         DispOutAt( 0,0,padc( "Number of Records Returned : " + ltrim( str(  aResult[ 8,06 ] ) ),MaxCol()+1 ), "N/GR*" )

         aStruct :=  aResult[ 8,04 ]
         FOR i := 1 to Len( aStruct )
            AAdd( aAttr, { i, aStruct[ i,1 ], aStruct[ i,2 ], aStruct[ i,3 ], aStruct[ i,4 ] } )
         NEXT

         IF ! Empty( aRecords )
            MGR_ArrayBrowse( aRecords, aAttr, left( NTRIM( nEnd-nStart ) + " : " + cSql, MaxCol() ) )
         ELSE
            IF lOpenExcel
               IF ( ".csv" == lower( Right( cXLS, 4 ) ) )
                  OpenCSVFileViaExcel( cXLS, aAttr )
               ELSEIF ( ".dbf" == lower( Right( cXLS, 4 ) ) )
                  MyBrowseFile( cXLS, .F., 1, "DBFCDX" )
               ENDIF
            ELSE
               Alert( "Done, check file " + cXLS )
            ENDIF
         ENDIF
      ELSE
         Alert( "No Records Returned!" )
      ENDIF
   ELSE
      Alert( "Query could not been prepared, some syntactical mistake!" )
   ENDIF

   RECOVER
   Alert( "Some error in query syntax!" )
   END SEQUENCE

   ErrorBlock( bError )
   CLS
   RETURN aResult


FUNCTION Sql_LoadCacheQuery()
   LOCAL cSql, bError, cXls, aResult
   LOCAL aRecords := {}

   CLS

   cSql := alltrim( VouGetQuery( "Query" ) )
   IF Empty( cSql ) .OR. !( lExeQuery )
      RETURN NIL
   ENDIF
   IF lower( left( cSql, 6 ) ) <> "select"
      Alert( "Only SELECT statement can be executed!" )
      RETURN NIL
   ENDIF

   cXLS   := cCSVFile
   bError := ErrorBlock( {|| break() } )
   BEGIN SEQUENCE
      aResult := ExeCacheQueryEx( { cSql, .F., .f., .f., cXLS, .f., NIL }, @aRecords )
   RECOVER
      Alert( "Some error in query syntax!" )
   END SEQUENCE
   ErrorBlock( bError )

   CLS
   RETURN aResult


FUNCTION ExeCacheQueryEx( a_, aRecords )
   LOCAL cSql, lSave, lShow, lAskParam, cTable, aParam, lExeOnSrv, lPrepared, aStruct
   LOCAL oSql, aScr, cColor, aHeads, aStatus, cExt
   LOCAL nStart    := 0, nEnd := 0
   LOCAL aValues   := {}
   LOCAL aNames    := {}
   LOCAL aTypes    := {}
   LOCAL lInTable  := .F.
   LOCAL lExecuted := .F.

   DEFAULT a_  TO {}

   asize( a_, SQP_VRBLS )

   cSql      := a_[ SQP_SQL      ]
   lSave     := a_[ SQP_SAVE     ]
   lShow     := a_[ SQP_SHOW     ]
   lAskParam := a_[ SQP_ASK      ]
   cTable    := a_[ SQP_TABLE    ]
   lExeOnSrv := a_[ SQP_ONSERVER ]
   aParam    := a_[ SQP_PARAM    ]

   DEFAULT cSql  TO VouGetQuery( "Query" )
   IF Empty( cSql )
      RETURN a_
   ENDIF

   DEFAULT lSave     TO .T.
   DEFAULT lShow     TO .T.
   DEFAULT lAskParam TO .T.
   DEFAULT lExeOnSrv TO .T.
   DEFAULT aParam    TO {}
   DEFAULT aRecords  TO {}

   IF lAskParam
      aHeads := {}
      AAdd( aHeads, "Save to Table"     )
      AAdd( aHeads, "Display Results"   )
      AAdd( aHeads, "Table Name"        )
      AAdd( aHeads, "Execute on Server" )

      aValues := { lSave, lShow, iif( Empty( cTable ), Pad( "MYQTABLE", 30 ), Pad( cTable, 30 ) ), lExeOnSrv }

      aValues := uiGetSomeA( aValues, aHeads, "Redefine Params" )

      lSave     := aValues[ 1 ]
      lShow     := aValues[ 2 ]
      cTable    := Trim( aValues[ 3 ] )
      lExeOnSrv := aValues[ 4 ]
      aValues   := {}
   ELSE
      IF lSave
         DEFAULT cTable TO iif( lSave, Trim( uiGetSome( "Table Name to Save Query Results", Space( 40 ) ) ), NIL )
      ENDIF
   ENDIF

   IF lShow
      aScr   := uiSaveScrn( 0, 0, MaxRow(), MaxCol() )
      cColor := SetColor( "N/W" )
      CLS
      uiDispOut( 0, 0, PadC( "Cache Query Generation", MaxCol() + 1 ), "W+/B" )
   ENDIF

   IF Empty( cSql )
      uiRestScrn( aScr )
      SetColor( cColor )
      RETURN {}
   ENDIF

   oSql := CacheSqlQuery():New()
   oSql:aRecords := aRecords

   /* New Protocl to Export To */
   IF ! Empty( cTable )
      hb_fNameSplit( cTable, , , @cExt )
      cExt := Upper( StrTran( cExt, ".", "" ) )
      oSql:fileName := cTable
      oSql:fileFormat := cExt
   ENDIF

   oSql:cSql := cSql
   oSql:create()

   IF ( lPrepared := oSql:lPrepared )
      aValues := GetQueryParam( oSql, aParam, lAskParam )

      nStart := Seconds()
      IF oSql:Execute( aValues )
         lExecuted := .T.
         IF ! lExeOnSrv
            aNames := oSql:GetColNames()
            aTypes := oSql:GetColTypes()
         ENDIF
      ENDIF
      nEnd := Seconds()
      aRecords := oSql:aRecords
      Wvt_SetTitle( LTrim( Str( nEnd - nStart, 10, 3 ) ) + ".." + cSql )

      CacheDebug( "Time Taken:", nEnd - nStart )
   ELSE
      CacheDebug( "Query could not been prepared!" )
   ENDIF
   aStruct := AClone( oSql:aStruct )
   aStatus := { cSql, lInTable, cTable, aStruct, aValues, oSql:nNumOfRecords, nEnd - nStart, lPrepared }

   IF lExecuted
      AAdd( aStatus, aRecords )
      AAdd( aStatus, aNames   )
      AAdd( aStatus, aTypes   )
   ENDIF
   hb_ThreadStart( {|| ShowQueryResults( .F., aStatus, oSql ) } )

   uiRestScrn( aScr )
   SetColor( cColor )

   RETURN { cSql, lSave, lShow, lAskParam, cTable, lExeOnSrv, aParam, aStatus }


STATIC FUNCTION ShowQueryResults( lInTable, aStatus, oSql )
   LOCAL i, aStruct, cAlias, cTitle
   LOCAL aAttr := {}

   hb_threadDetach( hb_ThreadStart( {|| oSql:fetchRecords() } ) )
   hb_idleSleep( 2 )

   hb_gtReload( "WVG" )

   Hb_GtInfo( HB_GTI_FONTNAME  , "Courier New" )
   Hb_GtInfo( HB_GTI_FONTSIZE  , 16 )
   Hb_GtInfo( HB_GTI_FONTWIDTH , 10 )
   Hb_GtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
   SetCursor( 0 )
   SetColor( "N/W" )
   CLS

   IF ! lInTable
      aStruct := aStatus[ 4 ]
      cAlias  := "Query Results"

      cTitle := LTrim( Str( aStatus[ 6 ], 10, 0 ) ) + ":" + ;
                           LTrim( Str( aStatus[ 7 ], 10, 2 ) ) + "::" + Trim( cAlias )

      FOR i := 1 TO Len( aStruct )
         AAdd( aAttr, { i, aStruct[ i,1 ], aStruct[ i,2 ], aStruct[ i,3 ], aStruct[ i,4 ] } )
      NEXT

      MGR_ArrayBrowse( @oSql:aRecords, aAttr, cTitle, .F. )
   ELSE
#if 0
      nArea := Select()
      USE ( aStatus[ 3 ] ) NEW SHARED VIA "CACHERDD" //CONNECTION MySetConxn( "TEMP" )
      IF ! NetErr()
         MyTableBrowse()
         USE
      ELSE
         Alert( "Table : " + aStatus[ 3 ] + " could not been opened!" )
      ENDIF
      Select( nArea )
#else
      MyBrowseFile( aStatus[ 3 ] )
#endif
   ENDIF

   oSql:Close()
   oSql:Destroy()
   RETURN NIL


STATIC FUNCTION GetQueryParam( oSql, aParam, lAsk )
   LOCAL nParams, i, cMsg, xValue, aTypes
   LOCAL aValues := {}
   LOCAL aHeads  := {}
   LOCAL nSupParam := Len( aParam )

   DEFAULT lAsk TO .T.

   nParams:= oSql:GetNumParam()

   IF nParams > 0
      aSize( aValues, nParams )
      aSize( aHeads, nParams )

      aTypes := oSql:GetParTypes()

      FOR i := 1 to nParams
         IF i > nSupParam
            cMsg := "Parameter " + NTRIM( i ) + ":" + aTypes[ i ]

            SWITCH aTypes[ i ]
            CASE "M" ; xValue := space( 10 ) ; EXIT
            CASE "C" ; xValue := space( 30 ) ; EXIT
            CASE "N" ; xValue := 0           ; EXIT
            CASE "D" ; xValue := ctod( "" )  ; EXIT
            CASE "L" ; xValue := .F.         ; EXIT
            ENDSWITCH

            AAdd( aParam, { cMsg, xValue } )
         ENDIF
         aHeads[ i ]  := aParam[ i,1 ]
         aValues[ i ] := aParam[ i,2 ]
      NEXT

      IF lAsk
         aValues := uiGetSomeA( aValues, aHeads, 'Parameters Needed by Query' )
      ENDIF
      AEval( aValues, {|e,i| iif( ValType( e )=='C', aValues[ i ] := Trim( e ), NIL )  } )
   ENDIF

   RETURN aValues


FUNCTION VouGetQuery( cTitle )
   LOCAL aDlg, nStyle, bDlgProc
   LOCAL nRows := 13, nCols := 70

   HB_SYMBOL_UNUSED( cTitle )

   nStyle := DS_SETFONT + WS_VISIBLE + WS_POPUP + WS_CAPTION + WS_SYSMENU + WS_THICKFRAME + WS_MINIMIZEBOX
   aDlg   := Wvt_MakeDlgTemplate( 1, 1, nRows, nCols, {0,0,0,0},'Please Enter SQL Query', nStyle )

   //  OK button
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_PUSHBUTTON
   aDlg   := Wvt_AddDlgItem( aDlg, nRows - 2, nCols-10, 1, 8, {-3,0,7,0}, ID_BTN_EXECUTE, 'BUTTON' , nStyle, 'Execute' )
   aDlg   := Wvt_AddDlgItem( aDlg, nRows - 2, nCols-19, 1, 8, {-3,0,7,0}, ID_BTN_SAVE   , 'BUTTON' , nStyle, 'Save'    )
   aDlg   := Wvt_AddDlgItem( aDlg, nRows - 2, nCols-28, 1, 8, {-3,0,7,0}, ID_BTN_LOAD   , 'BUTTON' , nStyle, 'Load'    )
   aDlg   := Wvt_AddDlgItem( aDlg, nRows - 2, nCols-50, 1, 3, { 5,0,7,0}, ID_BTN_SAVECSV, 'BUTTON' , nStyle, 'File'    )

   // Multi line edit control
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + ES_AUTOVSCROLL + ES_MULTILINE + ;
             ES_WANTRETURN + WS_BORDER  + WS_VSCROLL
   aDlg   := Wvt_AddDlgItem( aDlg,  1, 2, nRows-4, nCols-4, {}, ID_MLE , 'EDIT'   , nStyle, /* cText, nHelpId, nExStyle */ )

   // Open Excel after Creation
   nStyle :=  WS_CHILD + WS_VISIBLE + WS_TABSTOP + BS_AUTOCHECKBOX
   aDlg   := Wvt_AddDlgItem( aDlg, nRows - 2, 26, 1, 15, {3,0,7,0}, ID_EXE_EXCEL, 'BUTTON', nStyle, 'Execute Export?' )

   nStyle := WS_CHILD + WS_VISIBLE + SS_LEFT
   aDlg   := Wvt_AddDlgItem( aDlg, nRows - 2, 2, 0.3, 17, {-12}, 401, 'STATIC' , nStyle, 'File Name to Export:' )

   // CSV File Name
   nStyle := WS_CHILD + WS_VISIBLE + WS_TABSTOP + ES_LEFT + WS_BORDER + ES_AUTOHSCROLL
   aDlg   := Wvt_AddDlgItem( aDlg, nRows - 2, 2, 1, 17, {5,0,7,0}, ID_EDT_CSV, 'EDIT' , nStyle, '' )

   bDlgProc := {|a,b,c,d| DYNDLGPROC_2(a,b,c,d) }

   Wvt_DialogBox( aDlg, bDlgProc, Wvt_GetWindowHandle() )

   RETURN trim( cQryText )


STATIC FUNCTION DynDlgProc_2( hDlg, nMsg, wParam, lParam )
   LOCAL nChars, nIndex, cFile, rc_, rItm_, i, pt_, pt1_

   THREAD STATIC hBrush
   THREAD STATIC hFont

   HB_SYMBOL_UNUSED( lParam )

   SWITCH ( nMsg )

   CASE WM_CTLCOLOREDIT
      IF ( Win_p2n( Win_GetDlgItem( hDlg, ID_MLE ) ) == lParam )
         Win_SetTextColor( Win_n2p( wParam ), RGB( 0,0,255 ) )
         Win_SetBkColor( Win_n2p( wParam ), RGB( 255,255,200 ) )
         IF hBrush == NIL
            hBrush := Win_CreateBrush( 0, RGB( 255,255,200 ) )
         ENDIF
         RETURN hBrush
      ENDIF
      EXIT

   CASE WM_INITDIALOG
      lExeQuery := .F.
      lExeExcel := .F.
      cCSVFile  := ""

      hFont := Wvt_CreateFont( "Times New Roman", 18 )

      Win_SendMessage( Win_GetDlgItem( hDlg, ID_MLE ), WM_SETFONT, hFont, 0 )
      Win_SetDlgItemText( hDlg, ID_MLE, cQryText + " " )
      nChars := Win_SendMessage( Win_GetDlgItem( hDlg, ID_MLE ), WM_GETTEXTLENGTH, 0 )
      Win_SendMessage( Win_GetDlgItem( hDlg, ID_MLE ), EM_SETSEL, 0, nChars )
      CenterWindow( hDlg, Wvt_GetWindowHandle() )
      Win_SetFocus( Win_GetDlgItem( hDlg, ID_MLE ) )

      rc_:= Win_GetClientRect( hDlg )
      //
      rItm_:= Win_GetWindowRect( Win_GetDlgItem( hDlg, ID_MLE ) )
      pt_:= { rItm_[ 1 ],rItm_[ 2 ] }  ;  Win_ScreenToClient( hDlg, pt_ )
      pt1_:= { rItm_[ 3 ],rItm_[ 4 ] } ;  Win_ScreenToClient( hDlg, pt1_ )
      AAdd( tlbr_, { ID_MLE, Win_GetDlgItem( hDlg, ID_MLE ), ;
                     pt_[ 1 ], ;               // left offset
                     pt_[ 2 ], ;               // top offset
                     rc_[ 3 ] - pt1_[ 1 ], ;   // right offset
                     rc_[ 4 ] - pt1_[ 2 ]  ;   // bottom offset
                   } )

      rItm_:= Win_GetWindowRect( Win_GetDlgItem( hDlg, 401 ) )
      pt_:= { rItm_[ 1 ],rItm_[ 2 ] }  ; Win_ScreenToClient( hDlg, pt_ )
      AAdd( tlbr_, { 401                           , ;
                     Win_GetDlgItem( hDlg, 401 )   , ;
                     pt_[ 1 ]                             , ;   // x
                     rc_  [ RC_BOTTOM ] - pt_[ 2 ]        , ;   // y offset from bottom
                     rItm_[ RC_RIGHT  ] - rItm_[ RC_LEFT ], ;   // width
                     rItm_[ RC_BOTTOM ] - rItm_[ RC_TOP  ]  ;   // height
                   } )

      rItm_:= Win_GetWindowRect( Win_GetDlgItem( hDlg, ID_BTN_SAVECSV ) )
      pt_:= { rItm_[ 1 ],rItm_[ 2 ] }  ; Win_ScreenToClient( hDlg, pt_ )
      AAdd( tlbr_, { ID_BTN_SAVECSV                           , ;
                     Win_GetDlgItem( hDlg, ID_BTN_SAVECSV )   , ;
                     pt_[ 1 ]                             , ;   // x
                     rc_  [ RC_BOTTOM ] - pt_[ 2 ]        , ;   // y offset from bottom
                     rItm_[ RC_RIGHT  ] - rItm_[ RC_LEFT ], ;   // width
                     rItm_[ RC_BOTTOM ] - rItm_[ RC_TOP  ]  ;   // height
                   } )

      rItm_:= Win_GetWindowRect( Win_GetDlgItem( hDlg, ID_EXE_EXCEL ) )
      pt_:= { rItm_[ 1 ],rItm_[ 2 ] }  ; Win_ScreenToClient( hDlg, pt_ )
      AAdd( tlbr_, { ID_EXE_EXCEL                           , ;
                     Win_GetDlgItem( hDlg, ID_EXE_EXCEL )   , ;
                     pt_[ 1 ]                             , ;   // x
                     rc_  [ RC_BOTTOM ] - pt_[ 2 ]        , ;   // y offset from bottom
                     rItm_[ RC_RIGHT  ] - rItm_[ RC_LEFT ], ;   // width
                     rItm_[ RC_BOTTOM ] - rItm_[ RC_TOP  ]  ;   // height
                   } )

      rItm_:= Win_GetWindowRect( Win_GetDlgItem( hDlg, ID_EDT_CSV ) )
      pt_:= { rItm_[ 1 ],rItm_[ 2 ] }  ; Win_ScreenToClient( hDlg, pt_ )
      AAdd( tlbr_, { ID_EDT_CSV                           , ;
                     Win_GetDlgItem( hDlg, ID_EDT_CSV )   , ;
                     pt_[ 1 ]                             , ;   // x
                     rc_  [ RC_BOTTOM ] - pt_[ 2 ]        , ;   // y offset from bottom
                     rItm_[ RC_RIGHT  ] - rItm_[ RC_LEFT ], ;   // width
                     rItm_[ RC_BOTTOM ] - rItm_[ RC_TOP  ]  ;   // height
                   } )

      rItm_:= Win_GetWindowRect( Win_GetDlgItem( hDlg, ID_BTN_LOAD ) )
      pt_:= { rItm_[ 1 ],rItm_[ 2 ] }  ; Win_ScreenToClient( hDlg, pt_ )
      AAdd( tlbr_, { ID_BTN_LOAD                           , ;
                     Win_GetDlgItem( hDlg, ID_BTN_LOAD )   , ;
                     rc_[ 3 ] - pt_[ 1 ]                  , ;   // x
                     rc_  [ RC_BOTTOM ] - pt_[ 2 ]        , ;   // y offset from bottom
                     rItm_[ RC_RIGHT  ] - rItm_[ RC_LEFT ], ;   // width
                     rItm_[ RC_BOTTOM ] - rItm_[ RC_TOP  ]  ;   // height
                   } )

      rItm_:= Win_GetWindowRect( Win_GetDlgItem( hDlg, ID_BTN_SAVE ) )
      pt_:= { rItm_[ 1 ],rItm_[ 2 ] }  ; Win_ScreenToClient( hDlg, pt_ )
      AAdd( tlbr_, { ID_BTN_SAVE                           , ;
                     Win_GetDlgItem( hDlg, ID_BTN_SAVE )   , ;
                     rc_[ 3 ] - pt_[ 1 ]                  , ;   // x
                     rc_  [ RC_BOTTOM ] - pt_[ 2 ]        , ;   // y offset from bottom
                     rItm_[ RC_RIGHT  ] - rItm_[ RC_LEFT ], ;   // width
                     rItm_[ RC_BOTTOM ] - rItm_[ RC_TOP  ]  ;   // height
                   } )

      rItm_:= Win_GetWindowRect( Win_GetDlgItem( hDlg, ID_BTN_EXECUTE ) )
      pt_:= { rItm_[ 1 ],rItm_[ 2 ] }  ; Win_ScreenToClient( hDlg, pt_ )
      AAdd( tlbr_, { ID_BTN_EXECUTE                           , ;
                     Win_GetDlgItem( hDlg, ID_BTN_EXECUTE )   , ;
                     rc_[ 3 ] - pt_[ 1 ]                  , ;   // x
                     rc_  [ RC_BOTTOM ] - pt_[ 2 ]        , ;   // y offset from bottom
                     rItm_[ RC_RIGHT  ] - rItm_[ RC_LEFT ], ;   // width
                     rItm_[ RC_BOTTOM ] - rItm_[ RC_TOP  ]  ;   // height
                   } )
      EXIT

   CASE WM_COMMAND
      DO CASE
      CASE wParam == ID_BTN_EXECUTE
         cQryText  := Win_GetDlgItemText( hDlg, ID_MLE )
         IF ! Empty( cQryText )
            cCSVFile := Win_GetDlgItemText( hDlg, ID_EDT_CSV )
            lExeQuery := .T.
            lExeExcel := Win_IsDlgButtonChecked( hDlg,ID_EXE_EXCEL ) == 1
            Win_SendMessage( hDlg, WM_CLOSE, 0, 0 )

            #if 0
            IF ! Empty( cCSVFile )
               lExeQuery := .T.
               lExeExcel := Win_IsDlgButtonChecked( hDlg,ID_EXE_EXCEL ) == 1
               Win_SendMessage( hDlg, WM_CLOSE, 0, 0 )
            ELSE
               Win_SendMessage( hDlg, WM_CLOSE, 0, 0 )
            ENDIF
            #endif
         ELSE
            Win_SendMessage( hDlg, WM_CLOSE, 0, 0 )
         ENDIF

      CASE wParam == ID_BTN_SAVE
         cFile := space( 254 )
         cFile := Wvt_GetSaveFileName( hDlg, cFile, 'Save as .SQL file', {{ 'SQL File','*.sql;'}}, , , 'SQL', @nIndex  )

         IF ! Empty( cFile ) .and. Upper( Right( cFile,4 ) ) == '.SQL'
            MemoWrit( cFile, Win_GetDlgItemText( hDlg, ID_MLE ) )
         ENDIF

      CASE wParam == ID_BTN_SAVECSV
         cFile := space( 254 )
         cFile := Wvt_GetSaveFileName( hDlg, cFile, 'Select a file to export', ;
                {{'DBF File','*.dbf'},{ 'CSV File','*.csv'}}, , , 'CSV', @nIndex  )
         IF ! Empty( cFile ) .and. Upper( Right( cFile,4 ) ) $ '.CSV,.DBF'
            Win_SetDlgItemText( hDlg, ID_EDT_CSV, cFile )
         ENDIF

      CASE wParam == ID_BTN_LOAD
         cFile := space( 254 )
         cFile := Wvt_GetOpenFileName( hDlg, @cFile, 'Select an .SQL file', {{ 'SQL File','*.sql'}}, , , 'SQL', @nIndex  )

         IF ! Empty( cFile ) .and. ! Empty( MemoRead( cFile ) )
            Win_SetDlgItemText( hDlg, ID_MLE, MemoRead( cFile )+' ' )
            nChars := Win_SendMessage( Win_GetDlgItem( hDlg, ID_MLE ), WM_GETTEXTLENGTH, 0 )
            Win_SendMessage( Win_GetDlgItem( hDlg, ID_MLE ), EM_SETSEL, 0, nChars )
            Win_SetFocus(  Win_GetDlgItem( hDlg, ID_MLE ) )
         ENDIF

      ENDCASE
      EXIT

   CASE WM_DESTROY
      IF hBrush <> NIL
         Win_DeleteObject( hBrush )
      ENDIF
      IF hFont <> NIL
         Win_DeleteObject( hFont )
      ENDIF
      hBrush := NIL
      hFont  := NIL
      EXIT

   CASE WM_SIZE
      rc_:= Win_GetClientRect( hDlg )
      FOR i := 1 to Len( tlbr_ )
         switch tlbr_[ i,1 ]
         CASE ID_MLE
            Win_MoveWindow( tlbr_[ i,2 ], tlbr_[ i,3 ], tlbr_[ i,4 ], rc_[ 3 ] - tlbr_[ i,5 ] - tlbr_[ i,3 ], ;
                                                                      rc_[ 4 ] - tlbr_[ i,6 ] - tlbr_[ i,4 ], .T. )
            EXIT
         CASE ID_EDT_CSV
         CASE 401
         CASE ID_BTN_SAVECSV
         CASE ID_EXE_EXCEL
            Win_MoveWindow( tlbr_[ i,2 ], tlbr_[ i,3 ], rc_[ RC_BOTTOM ] - tlbr_[ i,4 ], tlbr_[ i,5 ], tlbr_[ i,6 ], .T. )
            EXIT

         CASE ID_BTN_LOAD
         CASE ID_BTN_EXECUTE
         CASE ID_BTN_SAVE
            Win_MoveWindow( tlbr_[ i,2 ], rc_[ RC_RIGHT ] - tlbr_[ i,3 ], rc_[ RC_BOTTOM ] - tlbr_[ i,4 ], tlbr_[ i,5 ], tlbr_[ i,6 ], .T. )
            EXIT
         end
      NEXT
      EXIT

   OTHERWISE
      WVT_GETXYFROMROWCOL( 10,10 )
      EXIT

   ENDSWITCH

   RETURN ( 0 )


STATIC FUNCTION CenterWindow( hWnd, hWndParent )
   LOCAL iCWidth, iCHeight
   LOCAL aChild_
   LOCAL aParent_
   LOCAL aPoint_

   aChild_  := Win_GetWindowRect( hWnd )
   iCWidth  := aChild_[ 3 ] - aChild_[ 1 ]
   iCHeight := aChild_[ 4 ] - aChild_[ 2 ]

   aParent_ := Win_GetClientRect( hWndParent )
   aPoint_  := { ( aParent_[ 3 ] / 2 ) , ( aParent_[ 4 ] / 2 ) }

   Win_ClientToScreen( hWndParent, @aPoint_ )

   aPoint_[ 1 ] -= ( iCWidth / 2 )
   aPoint_[ 2 ] -= ( iCHeight / 2 )

   Win_ScreenToClient( hWndParent, @aPoint_ )

   aPoint_[ 1 ] := Max( 0, aPoint_[ 1 ] )
   aPoint_[ 2 ] := Max( 0, aPoint_[ 2 ] )

   Win_ClientToScreen( hWndParent, @aPoint_ )

   Win_MoveWindow( hWnd, aPoint_[ 1 ] , aPoint_[ 2 ] , iCWidth, iCHeight, .F. )

   Return( NIL )


FUNCTION Array2Excel( aData, cFileName, aAttr, lHeader, lOpenExcel )
   LOCAL j, oApp, oWB, oAS, cType, nColumns
   LOCAL cServer := 'Excel.Application'

   DEFAULT aAttr      TO {}
   DEFAULT lHeader    TO .F.
   DEFAULT lOpenExcel TO .T.

   IF Empty( aAttr )
      lHeader := .F.
   ENDIF

   IF ! Array2Csv( aData, cFileName, aAttr )
      RETURN NIL
   ENDIF
   nColumns := Len( aData[ 1 ] )

   TRY
      oApp := CreateObject( cServer )
   CATCH
      RETURN NIL
   END

   oApp:Visible       := .F.
   oApp:DisplayAlerts := .F.
   oApp:Workbooks:Close()

   oWB := oApp:Workbooks:Open( cFileName )
   oAs := oWB:ActiveSheet()

   FOR j := 1 to nColumns
      cType := ValType( aData[ 1,j ] )
      oAs:columns( j ):NumberFormat( iif( cType == 'C', '@', iif( cType == 'N', '???.??', ;
                                     iif( cType == 'D', 'mm/dd/yyyy', '@' ) ) ) )
   NEXT

   // Columns be made visible properly
   FOR j := 1 to nColumns
      oAs:columns( j ):Autofit()
      oAs:Cells( 1,j ):font:bold := .T.
      oAs:Cells( 1,j ):font:color := RGB( 27,79,216 )
   NEXT

   IF ! lOpenExcel
      oWB:Close( 'SaveChanges',.F. )
      oAS := NIL
      oWB := NIL
      oApp:Quit()
      oApp := NIL
   ELSE
      oApp:Visible := .T.
   ENDIF

   RETURN NIL


FUNCTION Array2CSV( aArray, cCsvFile, aAttr )
   LOCAL i, s
   LOCAL nRows := Len( aArray )
   LOCAL nCols := Len( aArray[ 1 ] )
   LOCAL aTyp  := array( nCols )
   LOCAL aCsv  := {}

   AEval( aArray[ 1 ], {|e,i| aTyp[ i ] := ValType( e ) } )

   FOR i := 1 TO nRows
      s := ''
      AEval( aArray[ i ], {|e, j| s += X2Csv( e, aTyp[ j ] ) + if( j == nCols, '', ',' ) } )
      AAdd( aCsv, s )
   NEXT

   s := ''
   AEval( aAttr, {|e_| s += e_[ 2 ]+',' } )
   s += CRLF
   AEval( aCsv, {|e| s += e + CRLF } )
   MemoWrit( cCsvFile, s )

   RETURN File( cCsvFile )


STATIC FUNCTION X2Csv( x, cTyp )
   LOCAL xVar := ''

   DO CASE
   CASE cTyp == 'C'
      IF at( '"', x ) > 0 .OR. at( ',', x ) > 0
         xVar := '"' + StrTran( x, '"', '""' ) + '"'
      ELSEIF IsDigit( left( x, 1 ) )
         RETURN '="' + x + '"'
      ELSE
         RETURN x
      ENDIF
   CASE cTyp == 'N'
      xVar := ltrim( str( x ) )
   CASE cTyp == 'D'
      xVar := dtoc( x )
   CASE cTyp == 'L'
      xVar := if( x, 'Yes','No' )
   ENDCASE

   RETURN xVar


FUNCTION OpenCSVFileViaExcel( cFileName, aAttr )
   LOCAL j, oApp, oWB, oAS
   LOCAL cServer := 'Excel.Application'
   LOCAL nColumns

   nColumns := Len( aAttr )

   TRY
      oApp := CreateObject( cServer )
   CATCH
      RETURN NIL
   END

   oApp:Visible       := .F.
   oApp:DisplayAlerts := .F.
   oApp:Workbooks:Close()

   CacheDebug( 'OpenCSVFileViaExcel', cFileName )

   oWB := oApp:Workbooks:Open( cFileName )
   oAs := oWB:ActiveSheet()
#if 0
   FOR j := 1 to nColumns
      cType := aAttr[ j,3 ]
      CacheDebug( j, cType )
      oAs:columns( j ):NumberFormat( if( cType == 'C', '@', iif( cType == 'N', '???.??', ;
                                     if( cType == 'D', 'mm/dd/yyyy', '@' ) ) ) )
   NEXT
#endif
   // Columns be made visible properly
   FOR j := 1 to nColumns
      oAs:columns( j ):Autofit()
      oAs:Cells( 1,j ):font:bold := .T.
      oAs:Cells( 1,j ):font:color := RGB( 27,79,216 )
   NEXT

   oApp:Visible := .T.

   RETURN NIL

