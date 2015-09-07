#require "hbnf"

#include "fileio.ch"
#include "inkey.ch"
#include "tbrowse.ch"

PROCEDURE Main( cFile )

   IF ft_FUse( hb_defaultValue( cFile, __FILE__ ) ) != F_ERROR
      RunTxtBrowse( TxtBrowse() )
      ft_FUse()
   ELSE
      ? "File not found", cFile
   ENDIF

   RETURN

STATIC PROCEDURE RunTxtBrowse( oTBrowse )

   LOCAL nKey, nLen, lRun := .T.

   DO WHILE lRun
      oTBrowse:forceStable()

      SWITCH hb_keyStd( nKey := Inkey( 0 ) )
      CASE K_LEFT
         IF oTBrowse:cargo > 1
            oTBrowse:cargo--
            oTBrowse:refreshAll()
         ENDIF
         EXIT

      CASE K_RIGHT
         oTBrowse:cargo++
         oTBrowse:refreshAll()
         EXIT

      CASE K_HOME
         IF oTBrowse:cargo > 1
            oTBrowse:cargo := 1
            oTBrowse:refreshAll()
         ENDIF
         EXIT

      CASE K_END
         nLen := Len( ft_FReadLn() )
         IF nLen - oTBrowse:cargo + 1 > 72
            oTBrowse:cargo := nLen - 72 + 1
            oTBrowse:refreshAll()
         ENDIF
         EXIT

      CASE K_CTRL_HOME
         EXIT

      CASE K_CTRL_END
         EXIT

      OTHERWISE
         IF oTBrowse:applyKey( nKey ) == TBR_EXIT
            lRun := .F.
         ENDIF
      ENDSWITCH
   ENDDO

   RETURN

STATIC FUNCTION TxTBrowse( nT, nL, nB, nR )

   LOCAL oTBrowse := TBrowseNew( nT, nL, nB, nR )

   oTBrowse:addColumn( TBColumnNew( " ", {|| Str( ft_FRecNo(), 5 ) + ":" } ) )
   oTBrowse:addColumn( TBColumnNew( " ", {|| PadR( SubStr( ft_FReadLn(), oTBrowse:cargo ), 72 ) } ) )

   oTBrowse:goTopBlock    := {|| ft_FGoTop() }
   oTBrowse:goBottomBlock := {|| ft_FGoBot() }
   oTBrowse:skipBlock     := {| n | TxtSkipper( n ) }

   oTBrowse:cargo  := 1
   oTBrowse:colPos := 2

   RETURN oTBrowse

STATIC FUNCTION TxtSkipper( nRequest )

   LOCAL nSkip := 0

   DO CASE
   CASE nRequest < 0

      DO WHILE nSkip > nRequest .AND. ft_FRecNo() > 1
         ft_FSkip( -1 )
         nSkip--
      ENDDO

   CASE nRequest > 0
      DO WHILE nSkip < nRequest
         ft_FSkip()
         IF ft_FEof()
            EXIT
         ENDIF
         nSkip++
      ENDDO
   ENDCASE

   RETURN nSkip
