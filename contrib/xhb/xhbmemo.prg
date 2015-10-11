/*
 * xhb_MemoEdit() function
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

#include "hbclass.ch"
#include "memoedit.ch"
#include "inkey.ch"

// A specialized HBEditor which can simulate MemoEdit() behaviour

CREATE CLASS xhb_TMemoEditor INHERIT XHBEditor

   VAR xUserFunction   // User Function called to change default MemoEdit() behaviour

   VAR aEditKeys
   VAR aConfigurableKeys
   VAR aMouseKeys
   VAR aExtKeys

   METHOD MemoInit( xUDF )                         // This method is called after ::New() returns to perform ME_INIT actions
   METHOD Edit()                                   // Calls ::Super:Edit( nKey ) but is needed to handle configurable keys
   METHOD KeyboardHook( nKey )                     // Gets called every time there is a key not handled directly by HBEditor

   METHOD ExistUdf() INLINE HB_ISSTRING( ::xUserFunction ) .OR. HB_ISEVALITEM( ::xUserFunction )
   METHOD HandleUdf( nKey, nUdfReturn, lEdited )   // Handles requests returned to MemoEdit() by UDF
   METHOD CallUdf( nMode )                         // Call user function. (old xDo)

ENDCLASS

METHOD MemoInit( xUDF ) CLASS xhb_TMemoEditor

   LOCAL nUdfReturn

   ::aEditKeys := { ;
      K_DOWN, ;
      K_UP, ;
      K_LEFT, ;
      K_RIGHT, ;
      K_CTRL_LEFT, ;
      K_CTRL_RIGHT, ;
      K_HOME, ;
      K_END, ;
      K_CTRL_HOME, ;
      K_CTRL_END, ;
      K_PGUP, ;
      K_PGDN, ;
      K_CTRL_PGUP, ;
      K_CTRL_PGDN, ;
      K_RETURN, ;
      K_ENTER, ;
      K_DEL, ;
      K_BS, ;
      K_CTRL_BS, ;
      K_TAB, ;
      K_SH_TAB }

   // Save/Init object internal representation of user function

   ::xUserFunction := xUDF

   // NOTE: K_ALT_W is not compatible with Cl*pper exit memo and save key,
   //       it's used as an alternative for K_CTRL_W with GT's without
   //       extended keycode support

   /* CTRL_V in not same as K_INS when extended keycodes are available, this works as paste selected text to clipboard. */
   /* CTRL_V is same as K_INS when extended keycodes are not available, so it has special treatment in memoedit. */
   ::aConfigurableKeys := { ;
      K_CTRL_N, ;
      K_CTRL_Y, ;
      K_CTRL_T, ;
      K_CTRL_B, ;
      K_CTRL_W, ;
      K_CTRL_RETURN }
   ::aExtKeys := { ;
      hb_keyNew( "W", HB_KF_ALT  ), ;
      hb_keyNew( "A", HB_KF_CTRL ), ;
      hb_keyNew( "B", HB_KF_CTRL ), ;
      hb_keyNew( "C", HB_KF_CTRL ), ;
      hb_keyNew( "V", HB_KF_CTRL ), ;
      hb_keyNew( "X", HB_KF_CTRL ), ;
      hb_keyNew( HB_KX_INS   , HB_KF_SHIFT ), ;
      hb_keyNew( HB_KX_DOWN  , HB_KF_SHIFT ), ;
      hb_keyNew( HB_KX_UP    , HB_KF_SHIFT ), ;
      hb_keyNew( HB_KX_DEL   , HB_KF_SHIFT ), ;
      hb_keyNew( HB_KX_RIGHT , HB_KF_SHIFT ), ;
      hb_keyNew( HB_KX_LEFT  , HB_KF_SHIFT ), ;
      hb_keyNew( HB_KX_END   , HB_KF_SHIFT ), ;
      hb_keyNew( HB_KX_HOME  , HB_KF_SHIFT ) }

   ::aMouseKeys := { K_LBUTTONUP, K_MWFORWARD, K_MWBACKWARD }

   IF ::ExistUdf()
      /* Keep calling user function until it returns 0
         2004-08-05 - <maurilio.longo@libero.it>
                      Cl*pper 5.2 MemoEdit() treats a NIL as ME_DEFAULT */
      DO WHILE AScan( { ME_DEFAULT, NIL }, nUdfReturn := ::CallUdf( ME_INIT ) ) == 0

         // At this time there is no input from user of MemoEdit() only handling
         // of values returned by ::xUserFunction, so I pass this value on both
         // parameters of ::HandleUdf()

         ::HandleUdf( nUdfReturn, nUdfReturn, .F. )
      ENDDO
   ENDIF

   RETURN Self

METHOD Edit() CLASS xhb_TMemoEditor

   LOCAL nKey, nKeyStd, nUdfReturn, nNextKey

   // If I have an user function I need to trap configurable keys and ask to
   // user function if handle them the standard way or not

   IF NextKey() == 0 .AND. ::ExistUdf()
      ::CallUdf( ME_IDLE )
   ENDIF

   nNextKey := 0

   DO WHILE ! ::lExitEdit

      IF nNextKey == 0
         nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) )
      ELSE
         nKey := nNextKey
         nNextKey := 0
      ENDIF
      nKeyStd := hb_keyStd( nKey )

      IF nNextKey == 0 .AND. ;
         ( ( ::bKeyBlock := SetKey( nKey ) ) != NIL .OR. ;
           ( ::bKeyBlock := SetKey( nKeyStd ) ) != NIL )

         Eval( ::bKeyBlock, ::ProcName, ::ProcLine, ReadVar() )

         /* 2006-09-15 - E.F. - After SetKey() is executed, if exist nextkey,
                                I need trap this nextkey to memoedit process
                                <nKey> first and the <nNextKey> on the next loop. */
         nNextKey := hb_keyNext( hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) )

         IF nNextKey != 0
            Inkey()
         ENDIF
      ENDIF

      /* 2005-10-24 - <maurilio.longo@libero.it>
         Taken from Cl*pper Norton Guide:

         The user function: <cUserFunction>, a user-defined function
         specified as an argument, handles key exceptions and reconfigures
         special keys.  The user function is called at various times by
         MemoEdit(), most often in response to keys it does not recognize.
         Keys that instigate a key exception are all available control keys,
         function keys, and Alt keys.  Since these keys are not processed by
         MemoEdit(), they can be reconfigured.  Some of these keys have a
         default action assigned to them.  In the user function, you perform
         various actions, depending on the current MemoEdit() mode, then
         RETURN a value telling MemoEdit() what to do next.

         When the user function argument is specified, MemoEdit() defines two
         classes of keys: nonconfigurable and key exceptions.  When a
         nonconfigurable key is pressed, MemoEdit() executes it, otherwise a
         key exception is generated and the user function is called.  When
         there are no keys left in the keyboard buffer for MemoEdit() to
         process, the user function is called once again. */

      IF ::bKeyBlock == NIL

         IF ( ! HB_ISNULL( hb_keyChar( nKey ) ) .OR. ;
              AScan( ::aEditKeys, nKeyStd ) > 0 .OR. ;
              AScan( ::aConfigurableKeys, nKeyStd ) > 0 .OR. ;
              AScan( ::aExtKeys, nKey ) > 0 .OR. ;
              ( nKeyStd == K_INS .AND. ! ::ExistUdf() ) .OR. ;
              ( nKeyStd == K_ESC .AND. ! ::ExistUdf() ) )

            ::Super:Edit( nKey )

         ELSEIF AScan( ::aConfigurableKeys, nKeyStd ) == 0 .AND. ;
                AScan( ::aExtKeys, nKey ) == 0 .AND. ;
                ( nKeyStd > 255 .OR. nKeyStd < 0 ) .OR. ;
                ( nKeyStd == K_INS .AND. ::lEditAllow .AND. ::ExistUdf() ) .OR. ;
                ( nKeyStd == K_ESC .AND. ::ExistUdf() )

            ::KeyboardHook( nKey )
         ENDIF
      ENDIF

      IF ::ExistUdf()

         IF ! HB_ISNULL( hb_keyChar( nKey ) ) .OR. ;
            AScan( ::aEditKeys, nKeyStd ) > 0 .OR. ;
            AScan( ::aConfigurableKeys, nKeyStd ) > 0 .OR. ;
            AScan( ::aExtKeys, nKey ) > 0 .OR. ;
            nKeyStd == K_F1

            IF NextKey() == 0 .AND. ;
               AScan( ::aConfigurableKeys, nKeyStd ) == 0 .AND. nKeyStd != K_F1

               nUdfReturn := ::CallUdf( ME_IDLE )
            ELSE
               IF AScan( ::aConfigurableKeys, nKeyStd ) == 0
                  nUdfReturn := ::CallUdf( iif( ::lChanged, ME_UNKEYX, ME_UNKEY ) )
               ELSE
                  nUdfReturn := ::CallUdf( ME_UNKEY )
               ENDIF
            ENDIF

            ::HandleUdf( nKey, nUdfReturn, ::bKeyBlock == NIL )
         ENDIF
      ENDIF
   ENDDO

   RETURN Self

// I come here if I have an unknown key and it is not a configurable key
// if there is an user function I leave to it its handling
METHOD KeyboardHook( nKey ) CLASS xhb_TMemoEditor

   LOCAL nUdfReturn

   IF ::ExistUdf()
      nUdfReturn := ::CallUdf( iif( ::lChanged, ME_UNKEYX, ME_UNKEY ) )
      ::HandleUdf( nKey, nUdfReturn, .F. )
   ENDIF

   RETURN Self

METHOD HandleUdf( nKey, nUdfReturn, lEdited ) CLASS xhb_TMemoEditor

   LOCAL nKeyStd

   /* 2004-08-05 - <maurilio.longo@libero.it>
                   A little trick to be able to handle a nUdfReturn with value of NIL
                   like it had a value of ME_DEFAULT */

   hb_default( @nKey, 0 )
   hb_default( @nUdfReturn, ME_DEFAULT )
   hb_default( @lEdited, .F. )

   // I won't reach this point during ME_INIT since ME_DEFAULT ends
   // initialization phase of MemoEdit()

   SWITCH nUdfReturn
   CASE ME_DEFAULT

      // HBEditor is not able to handle keys with a value higher than 256 or lower than 1

      IF ! lEdited
         nKeyStd := hb_keyStd( nKey )
         IF ! HB_ISNULL( hb_keyChar( nKey ) ) .OR. ;
            AScan( { K_ALT_W, K_CTRL_W }, nKeyStd ) > 0 .OR. ;
            AScan( ::aExtKeys, nKey ) > 0 .OR. ;
            nKeyStd == K_ESC .OR. ;
            nKeyStd == K_INS .OR. ;
            AScan( ::aMouseKeys, nKeyStd ) > 0

            ::Super:Edit( nKey )
         ENDIF
      ENDIF
      EXIT

   CASE ME_IGNORE

      // Ignore unknown key, only check insert state.
      ::DisplayInsert( ::lInsert() )
      EXIT

   CASE ME_DATA

      IF ! lEdited
         nKeyStd := hb_keyStd( nKey )
         IF ! HB_ISNULL( hb_keyChar( nKey ) ) .OR. ;
            AScan( ::aExtKeys, nKey ) > 0 .OR. ;
            nKeyStd == K_ESC .OR. ;
            nKeyStd == K_INS

            ::Super:Edit( nKey )
         ENDIF
      ENDIF
      EXIT

   CASE ME_TOGGLEWRAP
      ::lWordWrap := ! ::lWordWrap
      EXIT

   CASE ME_TOGGLESCROLL
      ::lVerticalScroll := ! ::lVerticalScroll
      EXIT

   CASE ME_WORDRIGHT
      ::WordRight()
      EXIT

   CASE ME_BOTTOMRIGHT
      ::Bottom()
      ::End()
      EXIT

   CASE ME_PASTE
      // see inkey.ch
      EXIT

   OTHERWISE            // ME_UNKEY

      IF ! lEdited
         /* 2006-08-02 - E.F. - (NG) Process requested action corresponding to key value. */
         nKeyStd := hb_keyStd( nUdfReturn )
         IF ( nKeyStd >= 1 .AND. nKeyStd <= 31 ) .OR. ;
            AScan( ::aExtKeys, nKey ) > 0

            ::Super:Edit( nKey )
         ENDIF
      ENDIF
      EXIT

   ENDSWITCH

   RETURN Self

METHOD CallUdf( nMode ) CLASS xhb_TMemoEditor

   LOCAL nCurRow := ::Row()
   LOCAL nCurCol := ::Col()
   LOCAL xResult

   IF ::ExistUdf()
      // Latest parameter, <Self>, is an xHarbour extension
      xResult := Do( ::xUserFunction, nMode, ::nRow, ::nCol - 1, Self )

      ::SetPos( nCurRow, nCurCol )
   ENDIF

   RETURN xResult

// Prg Level Call of MemoEdit()
FUNCTION xhb_MemoEdit( ;
      cString, ;
      nTop, nLeft, ;
      nBottom, nRight, ;
      lEditMode, ;
      xUDF, ;
      nLineLength, ;
      nTabSize, ;
      nTextBuffRow, ;
      nTextBuffColumn, ;
      nWindowRow, ;
      nWindowColumn )

   LOCAL oEd

   __defaultNIL( @cString, "" )
   __defaultNIL( @nTop, 0 )
   __defaultNIL( @nLeft, 0 )
   __defaultNIL( @nBottom, MaxRow() )
   __defaultNIL( @nRight, MaxCol() )
   __defaultNIL( @lEditMode, .T. )
   __defaultNIL( @nTabSize, 4 )
   __defaultNIL( @nTextBuffRow, 1 )
   __defaultNIL( @nTextBuffColumn, 0 )
   __defaultNIL( @nWindowRow, 0 )
   __defaultNIL( @nWindowColumn, nTextBuffColumn )

   // 2006-07-22 - E.F. Check argument types.

   IF ! HB_ISSTRING( cString )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<cString> Argument type error", ProcName() ) )
   ENDIF
   IF ! HB_ISNUMERIC( nTop )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<nTop> Argument type error", ProcName() ) )
   ENDIF
   IF ! HB_ISNUMERIC( nLeft )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<nLeft> Argument type error", ProcName() ) )
   ENDIF
   IF ! HB_ISNUMERIC( nRight )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<nRight> Argument type error", ProcName() ) )
   ENDIF
   IF ! HB_ISNUMERIC( nBottom )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<nBottom> Argument type error", ProcName() ) )
   ENDIF
   IF ! HB_ISLOGICAL( lEditMode )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<lEditMode> Argument type error", ProcName() ) )
   ENDIF
   IF xUDF != NIL .AND. ! HB_ISSTRING( xUDF ) .AND. ! HB_ISLOGICAL( xUDF )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<cUserFunction> Argument type error", ProcName() ) )
   ENDIF
   IF nLineLength != NIL .AND. ! HB_ISNUMERIC( nLineLength )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<nLineLength> Argument type error", ProcName() ) )
   ENDIF
   IF ! HB_ISNUMERIC( nTabSize )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<nTabSize> Argument type error", ProcName() ) )
   ENDIF
   IF ! HB_ISNUMERIC( nTextBuffRow )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<nTextBuffRow> Argument type error", ProcName() ) )
   ENDIF
   IF ! HB_ISNUMERIC( nTextBuffColumn )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<nTextBuffColumn> Argument type error", ProcName() ) )
   ENDIF
   IF ! HB_ISNUMERIC( nWindowRow )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<nWindowRow> Argument type error", ProcName() ) )
   ENDIF
   IF ! HB_ISNUMERIC( nWindowColumn )
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<nWindowColumn> Argument type error", ProcName() ) )
   ENDIF

   // 2006-07-22 - E.F. To avoid run time error.
   IF nTop > nBottom .OR. nLeft > nRight
      Throw( xhb_ErrorNew( "BASE", 0, 1127, "<nTop,nLeft,nRight,nBottom> Argument error", ProcName() ) )
   ENDIF

   IF HB_ISSTRING( xUDF ) .AND. Empty( xUDF )
      xUDF := NIL
   ENDIF

   /* 2005-10-24 - <maurilio.longo@libero.it>
                   Cl*pper MemoEdit() converts tabs into spaces */
   oEd := xhb_TMemoEditor():New( StrTran( cString, Chr( 9 ), Space( nTabSize ) ), ;
      nTop, nLeft, nBottom, nRight, ;
      lEditMode, ;
      nLineLength, ;
      nTabSize, ;
      nTextBuffRow, ;
      nTextBuffColumn, ;
      nWindowRow, ;
      nWindowColumn )

   oEd:ProcName := ProcName( 1 )
   oEd:ProcLine := ProcLine( 1 )

   oEd:MemoInit( xUDF )
   oEd:RefreshWindow()

   // 2006-08-06 - E.F. Cl*pper's <cUserFunction> in .T. or. F. means the same.
   IF HB_ISLOGICAL( xUDF )
      /* 2006-07-24 - E.F. - If xUDF is in .F. or .T. cause diplay memo content and exit,
                             so we have to repos the cursor at bottom of memoedit
                             screen after that. */
      SetPos( Min( nBottom, MaxRow() ), 0 )
   ELSE
      oEd:Edit()

      IF oEd:lSaved
         cString := oEd:GetText( .T. )  // Cl*pper inserts Soft CR
      ENDIF
   ENDIF

   RETURN cString
