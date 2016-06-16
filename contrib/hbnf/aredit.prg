/* This is an original work by James J. Orlowski, M.D. and is placed in the
   public domain.

      Rev 1.2   15 Aug 1991 23:05:56   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   12 Jun 1991 00:42:38   GLENN
   A referee suggested changing the documentation such that the return value
   is shown as "xElement" rather than "cElement" because the function
   can return different types.

      Rev 1.0   07 Jun 1991 23:03:24   GLENN
   Initial revision.
 */

/* The tbmethods section is a short cut from Spence's book instead
   of using the longer DO CASE method.

   Jim Gale showed me the basic array browser and Robert DiFalco
   showed me the improved skipblock in public messages on Nanforum.

   I added the functionality of the "Edit Get" code block
   (ie bGetFunc), TestGet() demo, and the add/delete rows. */

#include "box.ch"
#include "inkey.ch"

// Default info for tb_methods section
#define KEY_ELEM 1
#define BLK_ELEM 2

// ANYTYPE[]   ar        - Array to browse
// NUMERIC     nElem     - Element In Array
// CHARACTER[] aHeadings - Array of Headings for each column
// BLOCK[]     aBlocks   - Array containing code block for each column
// CODE BLOCK  bGetFunc  - Code Block For Special Get Processing
//  NOTE: When evaluated a code block is passed the array element to
//        be edited

FUNCTION ft_ArEdit( nTop, nLeft, nBot, nRight, ar, nElem, aHeadings, aBlocks, bGetFunc )

   LOCAL exit_requested, nKey, meth_no
   LOCAL cSaveWin, i, b, column
   LOCAL dim, cType, cVal
   LOCAL tb_methods := { ;
      { K_DOWN,       {| b | b:down() } }, ;
      { K_UP,         {| b | b:up() } }, ;
      { K_PGDN,       {| b | b:pagedown() } }, ;
      { K_PGUP,       {| b | b:pageup() } }, ;
      { K_CTRL_PGUP,  {| b | b:gotop() } }, ;
      { K_CTRL_PGDN,  {| b | b:gobottom() } }, ;
      { K_RIGHT,      {| b | b:Right() } }, ;
      { K_LEFT,       {| b | b:Left() } }, ;
      { K_HOME,       {| b | b:home() } }, ;
      { K_END,        {| b | b:end() } }, ;
      { K_CTRL_LEFT,  {| b | b:panleft() } }, ;
      { K_CTRL_RIGHT, {| b | b:panright() } }, ;
      { K_CTRL_HOME,  {| b | b:panhome() } }, ;
      { K_CTRL_END,   {| b | b:panend() } } }

   cSaveWin := SaveScreen( nTop, nLeft, nBot, nRight )
   hb_DispBox( nTop, nLeft, nBot, nRight, HB_B_SINGLE_UNI )

   b := TBrowseNew( nTop + 1, nLeft + 1, nBot - 1, nRight - 1 )
   b:headsep := hb_UTF8ToStrBox( "═╤═" )
   b:colsep  := hb_UTF8ToStrBox( " │ " )
   b:footsep := hb_UTF8ToStrBox( "═╧═" )

   b:gotopblock    := {|| nElem := 1 }
   b:gobottomblock := {|| nElem := Len( ar[ 1 ] ) }

   // skipblock originally coded by Robert DiFalco
   b:SkipBlock := {| nSkip, nStart | nStart := nElem, ;
      nElem := Max( 1, Min( Len( ar[ 1 ] ), nElem + nSkip ) ), nElem - nStart }

   FOR i := 1 TO Len( aBlocks )
      column := TBColumnNew( aHeadings[ i ], aBlocks[ i ] )
      b:addcolumn( column )
   NEXT

   exit_requested := .F.
   DO WHILE ! exit_requested

      DO WHILE ( nKey := Inkey() ) == 0 .AND. ! b:stabilize()
      ENDDO

      IF nKey == 0
         nKey := Inkey( 0 )
      ENDIF

      nKey := hb_keyStd( nKey )

      IF ( meth_no := AScan( tb_methods, {| elem | nKey == elem[ KEY_ELEM ] } ) ) > 0
         Eval( tb_methods[ meth_no ][ BLK_ELEM ], b )
      ELSE
         DO CASE
         CASE nKey == K_F7
            FOR EACH dim IN ar
               hb_ADel( dim, nElem, .T. )
            NEXT
            b:refreshAll()

         CASE nKey == K_F8
            FOR EACH dim IN ar
               // check type of current element before hb_AIns()
               cType := ValType( dim[ nElem ] )
               cVal  := dim[ nElem ]
               hb_AIns( dim, nElem,, .T. )
               DO CASE
               CASE cType == "C" ; dim[ nElem ] := Space( Len( cVal ) )
               CASE cType == "N" ; dim[ nElem ] := 0
               CASE cType == "L" ; dim[ nElem ] := .F.
               CASE cType == "D" ; dim[ nElem ] := hb_SToD()
               ENDCASE
            NEXT
            b:refreshAll()

         CASE nKey == K_ESC
            exit_requested := .T.

         CASE HB_ISEVALITEM( bGetFunc )  // Other exception handling
            IF nKey != K_ENTER
               hb_keyPut( LastKey() )  // want last key to be part of GET edit so KEYBOARD it
            ENDIF
            Eval( bGetFunc, b, ar, b:colPos, nElem )
            // after get move to next field
            hb_keyPut( iif( b:colPos < b:colCount, K_RIGHT, { K_HOME, K_DOWN } ) )

         CASE nKey == K_ENTER
            // Placing K_ENTER here below Edit Block (i.e. bGetFunc)
            // defaults K_ENTER to Edit when bGetFunc Is Present
            // BUT if no bGetFunc, then K_ENTER selects element to return
            exit_requested := .T.

         ENDCASE
      ENDIF
   ENDDO
   RestScreen( nTop, nLeft, nBot, nRight, cSaveWin )

   RETURN ar[ b:colPos ][ nElem ]
