#!/usr/bin/env hbmk2

/*
 * Converts core lang modules to standard .po files
 *
 * Copyright 2013 Viktor Szakats (vszakats.net/harbour)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their website at https://www.gnu.org/).
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "hblang.ch"

PROCEDURE Main_lang2po()

   LOCAL cLang

   FOR EACH cLang IN CoreLangList()
      hb_MemoWrit( Lower( hb_FNameName( cLang ) ) + ".po", LangToPO( cLang ) )
   NEXT

   RETURN

STATIC FUNCTION LangToPO( cLang )

   LOCAL nPos := 0
   LOCAL cPO := Item( "", Meta(), nPos++ )
   LOCAL tmp

   cPO += Item( "English (in English)", hb_langMessage( 1, cLang ), nPos++ )
   cPO += Item( "English", hb_langMessage( 2, cLang ), nPos++ )

   FOR tmp := HB_LANG_ITEM_BASE_MONTH TO HB_LANG_ITEM_MAX_ - 1
      IF ! HB_ISNULL( hb_langMessage( tmp, "en" ) )
         cPO += Item( ;
            hb_langMessage( tmp, "en" ), ;
            iif( hb_langMessage( tmp, "en" ) == hb_langMessage( tmp, cLang ) .AND. ;
               Translatable( hb_langMessage( tmp, "en" ) ) .AND. ;
               nPos != 28 .AND. ;  /* Copy "Ins" translation even if the same as original */
               Len( hb_langMessage( tmp, "en" ) ) > 1, "", hb_langMessage( tmp, cLang ) ), ;
            nPos++ )
      ENDIF
   NEXT

   RETURN hb_StrShrink( cPO, Len( hb_eol() ) )

STATIC FUNCTION Translatable( cString )

   LOCAL tmp

   FOR tmp := 1 TO Len( cString )
      IF IsAlpha( SubStr( cString, tmp, 1 ) ) .OR. ;
         IsDigit( SubStr( cString, tmp, 1 ) )
         RETURN .T.
      ENDIF
   NEXT

   RETURN .F.

STATIC FUNCTION CoreLangList()

   LOCAL aList := {}

   LOCAL nCount := __dynsCount()
   LOCAL cName
   LOCAL tmp

   FOR tmp := 1 TO nCount
      cName := __dynsGetName( tmp )
      IF hb_LeftEq( cName, "HB_LANG_" )
         cName := SubStr( cName, Len( "HB_LANG_" ) + 1 )
         IF ( Len( cName ) != 5 .OR. "_" $ cName ) .AND. ;
            ! "|" + cName + "|" $ "|RUKOI8|UAKOI8|ZHB5|ZHGB|"  /* HB_LEGACY_LEVEL4 */
            AAdd( aList, Lower( Left( cName, 2 ) ) + SubStr( cName, 3 ) )
         ENDIF
      ENDIF
   NEXT

   RETURN aList

STATIC FUNCTION Meta()

   LOCAL cISO_TimeStamp := ISO_TimeStamp()

   LOCAL hMeta
   LOCAL cMeta

   LOCAL meta

   hMeta := { => }
   hMeta[ "Project-Id-Version:"        ] := "core-lang"
   hMeta[ "Report-Msgid-Bugs-To:"      ] := "https://github.com/vszakats/harbour-core/issues"
   hMeta[ "POT-Creation-Date:"         ] := cISO_TimeStamp
   hMeta[ "PO-Revision-Date:"          ] := cISO_TimeStamp
   hMeta[ "Last-Translator:"           ] := "foo bar <foo.bar@example.org>"
   hMeta[ "Language-Team:"             ] := "https://www.transifex.com/projects/p/harbour/"
   hMeta[ "MIME-Version:"              ] := "1.0"
   hMeta[ "Content-Type:"              ] := "text/plain; charset=UTF-8"
   hMeta[ "Content-Transfer-Encoding:" ] := "8bit"

   cMeta := ""
   FOR EACH meta IN hMeta
      cMeta += meta:__enumKey() + " " + meta
      IF ! meta:__enumIsLast()
         cMeta += e"\n"
      ENDIF
   NEXT

   RETURN cMeta

STATIC FUNCTION ISO_TimeStamp()

   LOCAL nOffset := hb_UTCOffset()

   RETURN hb_StrFormat( "%1$s%2$s%3$02d%4$02d", ;
      hb_TToC( hb_DateTime(), "yyyy-mm-dd", "hh:mm" ), ;
      iif( nOffset < 0, "-", "+" ), ;
      Int( Abs( nOffset ) / 3600 ), ;
      Int( Abs( nOffset ) % 3600 / 60 ) )

STATIC FUNCTION Item( cOri, cTrs, nPos )

   LOCAL cComment := Comment( nPos )

   RETURN ;
      iif( Empty( cComment ), "", "#  " + cComment + hb_eol() ) + ;
      "#: lang_id:" + hb_ntos( nPos ) + hb_eol() + ;
      "#, c-format" + hb_eol() + ;
      "msgid " + ItemString( iif( HB_ISNULL( cOri ) .AND. nPos != 0, "{" + StrZero( nPos, 3 ) + "}", cOri ) ) + ;
      "msgstr " + ItemString( cTrs ) + hb_eol()

STATIC FUNCTION ItemString( cString )

   LOCAL cResult := ""
   LOCAL line

   LOCAL aLine := hb_ATokens( cString, e"\n" )

   IF Len( aLine ) > 1
      cResult += '""' + hb_eol()
   ENDIF

   FOR EACH line IN aLine
      cResult += '"' + ConvToC_2( line )
      IF ! line:__enumIsLast()
         cResult += "\n"
      ENDIF
      cResult += '"' + hb_eol()
   NEXT

   RETURN cResult

STATIC FUNCTION ConvToC_2( cStr )
   RETURN hb_StrReplace( cStr, { '"' => '\"' } )

STATIC FUNCTION Comment( nPos )

   SWITCH nPos
   CASE 22  ; RETURN "Columns must be aligned to character positions: 1, 19, 32, 48"
   CASE 25
   CASE 26
   CASE 27  ; RETURN "Keep the '*' decorations and internal space padding intact."
   CASE 28  ; RETURN "Abbrev of 'Insert' (as 'insert mode' in editing)."
   CASE 29  ; RETURN "Abbrev of 'Overwrite' using same length as 'Ins', can be spaces only (fill with 3 spaces if in doubt, or match the length of translation of 'Ins')."
   CASE 31  ; RETURN "Must have one space padding on the right (after ':' character)."
   CASE 32  ; RETURN "One space padding on each side."
   CASE 102 ; RETURN "Local date format, where YYYY=year, MM=month, DD=day. DO NOT TRANSLATE 'YYYY', 'MM' or 'DD', only reorder and set delimiter per country standards."
   ENDSWITCH

   RETURN ""
