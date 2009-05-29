/*
 * $Id$
 */

//----------------------------------------------------------------------//

#define WT_DIALOG     0      // used internally (user custom dialog class - advanced option)

#include "commdlg.ch"
#include "winuser.ch"

/*
pragma(4)
#include "ctruct.ch"
#include "winstruc.ch"
*/

// Under development !!!

//----------------------------------------------------------------------//
//
// FindText()
// don't forget to call RegisterWindowMessage(FINDMESSAGESTRING) before calling
// SYNTAX: FinText(<hWnd>,[<hInst>],<nFlags>,<cFindWhat>,[<bAction>]) -> hDlg
// isDialogMessage() will detect this dialog automatically, if called in auto mode
//
Function WHT_FindText( hWnd, hInst, nFlags, cFindWhat, bAction)
   LOCAL nIndex
   LOCAL n
   LOCAL aDialog := _Get_aDialog()
   LOCAL aWindow := _Get_aWindow()
   LOCAL hDlg

   // register the dialog

   If  ( nIndex := aScan( aDialog, { | x | x[ 1 ] == NIL } ) ) == 0
      aAdd( aDialog, { 0, bAction, 1 } )
      nIndex := Len( aDialog )
   Else
      aDialog[ nIndex ] := { 0, bAction, 1 }  // 0 means waiting...
   EndIf                                      // 1 means modal

   // we need to add it here too, to QUIT on the last window !!!
   // note type 0

   If ( n := aScan( aWindow, { | x | x[ 1 ] == NIL } ) ) == 0
       aAdd( aWindow, { 0, WT_DIALOG, { } } )
       n := Len( aWindow )
   Else
      aWindow[ n ] := { 0, WT_DIALOG, { } }  // window 0 means waiting ...
   EndIf

   // create the dialog
   hDlg := _FindText( hWnd, hInst, nFlags, cFindWhat) //, _GetDlgProc( ) )

   // if failed to create
   If hDlg == 0
      aDialog[ nIndex ] := { NIL , NIL, NIL }
      aWindow[ n ] := { NIL , NIL , { } }
      __KillWindow( )
   EndIf

   Return( hDlg )

//----------------------------------------------------------------------//
// FindText()
// don't forget to call RegisterWindowMessage(FINDMESSAGESTRING) before calling
// SYNTAX: ReplaceText(<hWnd>,[<hInst>],<nFlags>,<cFindWhat>,<cReplaceWith>,[<bAction>]) -> hDlg
// isDialogMessage() will detect this dialog automatically, if called in auto mode
//
Function WHT_ReplaceText( hWnd, hInst, nFlags, cFindWhat, cReplaceWith, bAction)
   LOCAL n
   LOCAL nIndex
   LOCAL aDialog := _Get_aDialog()
   LOCAL aWindow := _Get_aWindow()
   LOCAL hDlg

   // register the dialog

   If  ( nIndex := aScan( aDialog, { | x | x[ 1 ] == NIL } ) ) == 0
      aAdd( aDialog, { 0, bAction, 1 } )
      nIndex := Len( aDialog )
   Else
      aDialog[ nIndex ] := { 0, bAction, 1 }  // 0 means waiting...
   EndIf                                      // 1 means modal

   // we need to add it here too, to QUIT on the last window !!!
   // note type 0

   If ( n := aScan( aWindow, { | x | x[ 1 ] == NIL } ) ) == 0
       aAdd( aWindow, { 0, WT_DIALOG, { } } )
       n := Len( aWindow )
   Else
      aWindow[ n ] := { 0, WT_DIALOG, { } }  // window 0 means waiting ...
   EndIf

   // create the dialog
   hDlg := _ReplaceText( hWnd, hInst, nFlags, cFindWhat,cReplaceWith ) //, _GetDlgProc( ) )

   // if failed to create
   If hDlg == 0
      aDialog[ nIndex ] := { NIL , NIL, NIL }
      aWindow[ n ] := { NIL , NIL , { } }
      __KillWindow( )
   EndIf

   Return( hDlg )
//----------------------------------------------------------------------//
/*
GetOpenFileName( hWnd, @cPath, cTitle, aFilter, nFlags, cInitDir, cDefExt, nIndex)

hWnd:     Handle to parent window
cPath:    (optional) if OFN_ALLOWMULTISELECT the path is stored
cTitle:   Window Title
aFilter:  Array of Files Types i.e. { {'Data Bases','*.dbf'},{'Clipper','*.prg'} }
nFlags:   OFN_* values default to OFN_EXPLORER
cInitDir: Initial directory
cDefExt:  Default Extension i.e. 'DBF'
nIndex:   Index position of types

Returns:  If OFN_ALLOWMULTISELECT
              Array of files selected
          else
              FileName.
          endif
*/
FUNCTION WHT_GetOpenFileName( hWnd, cPath, cTitle, aaFilters, nFlags, cIniDir, cDefExt, nIndex )

   LOCAL aFiles, cRet, cFile, x, aFilter, cFilter := "", cItem, nAt, cChar

   IF cPath == NIL
      cPath := ""
   ENDIF

   IF ValType( aaFilters ) == "A"
      FOR EACH aFilter IN aaFilters
          cFilter += aFilter[1] + Chr(0) + aFilter[2] + Chr(0)
      NEXT
   ENDIF

   IF AND(nFlags,OFN_ALLOWMULTISELECT ) > 0
      cFile := Space( 32000 )
   ELSE
      cFile := Padr( Trim( cPath ), 256, Chr(0) )
   ENDIF

   cRet := _GetOpenFileName( hWnd, @cFile, cTitle, cFilter, nFlags, cIniDir, cDefExt, @nIndex )

   IF AND( nFlags, OFN_ALLOWMULTISELECT ) > 0
      nAt := At( Chr(0) + Chr(0), cFile )

      cFile := Left( cFile, nAt )
      aFiles := {}

      IF nAt == 0 // no double chr(0) user must have pressed cancel
         RETURN( aFiles )
      ENDIF

      x := At( Chr(0), cFile ) // fist null
      cPath := Left( cFile, x )

      cFile := StrTran( cFile, cPath, "" )

      IF ! Empty(cFile) // user selected more than 1 file
         cItem := ""

         FOR EACH cChar IN cFile
             IF cChar == 0
                aAdd( aFiles, StrTran( cPath, Chr(0), "" ) + '\' + cItem )
                cItem := ""
                LOOP
             ENDIF

             cItem += cChar
         NEXT
      ELSE
         /*
         cFile:=cPath
         x:=RAT('\',cFile)
         cPath:=LEFT(cFile,x-1)
         */
         aFiles := { StrTran( cPath, CHR(0), "" ) } //STRTRAN(STRTRAN(cFile,cPath),'\')}
      ENDIF

      Return( aFiles )
   ELSE
     //cRet := Left( cRet, At( chr(0), cRet ) -1 )
   ENDIF

   RETURN cRet
//----------------------------------------------------------------------//
/*
GetSaveFileName( hWnd, cFile, cTitle, aFilter, nFlags, cInitDir, cDefExt, nIndex)

hWnd:     Handle to parent window
cFile:    (optional) Default FileName
cTitle:   Window Title
aFilter:  Array of Files Types i.e. { {'Data Bases','*.dbf'},{'Clipper','*.prg'} }
nFlags:   OFN_* values default to OFN_EXPLORER
cInitDir: Initial directory
cDefExt:  Default Extension i.e. 'DBF'
nIndex:   Index position of types

Returns:  FileName.
*/
FUNCTION WHT_GetSaveFileName(hWnd, cFile, cTitle, aFilter, nFlags, cIniDir, cDefExt, nIndex )
   local n,c:=''

   IF aFilter==nil
      aFilter:={}
   END
   FOR n:=1 TO LEN(aFilter)
       c+=aFilter[n][1]+chr(0)+aFilter[n][2]+chr(0)
   NEXT
   cFile:=_GetSaveFileName(hWnd, cFile, cTitle, c, nFlags, cIniDir, cDefExt, @nIndex )

   Return(cFile)
//----------------------------------------------------------------------//
