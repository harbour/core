/*
 * Harbour Project source code:
 * [S|G]ETKX[LAT|TAB] CA-T*ols functions (USE IT AT YOUR OWN RISK)
 *
 * Copyright 2012 Viktor Szakats (vszakats.net/harbour)
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "inkey.ch"

#include "hbgtinfo.ch"
#include "hbinkey.ch"

#include "ctscan.ch"

/* Trick to make it work in the STATIC initializer.
   It's safe because it's only used with ASCII chars. */
#define hb_keyCode( x ) Asc( x )

STATIC s_hTrs := { => }
STATIC s_hMutex := hb_mutexCreate()

STATIC sc_hCnv := { ;
   KS_DISABLE        => 0                 , ;
   KS_A              => hb_keyCode( "A" ) , ;
   KS_B              => hb_keyCode( "B" ) , ;
   KS_C              => hb_keyCode( "C" ) , ;
   KS_D              => hb_keyCode( "D" ) , ;
   KS_E              => hb_keyCode( "E" ) , ;
   KS_F              => hb_keyCode( "F" ) , ;
   KS_G              => hb_keyCode( "G" ) , ;
   KS_H              => hb_keyCode( "H" ) , ;
   KS_I              => hb_keyCode( "I" ) , ;
   KS_J              => hb_keyCode( "J" ) , ;
   KS_K              => hb_keyCode( "K" ) , ;
   KS_L              => hb_keyCode( "L" ) , ;
   KS_M              => hb_keyCode( "M" ) , ;
   KS_N              => hb_keyCode( "N" ) , ;
   KS_O              => hb_keyCode( "O" ) , ;
   KS_P              => hb_keyCode( "P" ) , ;
   KS_Q              => hb_keyCode( "Q" ) , ;
   KS_R              => hb_keyCode( "R" ) , ;
   KS_S              => hb_keyCode( "S" ) , ;
   KS_T              => hb_keyCode( "T" ) , ;
   KS_U              => hb_keyCode( "U" ) , ;
   KS_V              => hb_keyCode( "V" ) , ;
   KS_W              => hb_keyCode( "W" ) , ;
   KS_X              => hb_keyCode( "X" ) , ;
   KS_Y              => hb_keyCode( "Y" ) , ;
   KS_Z              => hb_keyCode( "Z" ) , ;
   KS_a              => hb_keyCode( "a" ) , ;
   KS_b              => hb_keyCode( "b" ) , ;
   KS_c              => hb_keyCode( "c" ) , ;
   KS_d              => hb_keyCode( "d" ) , ;
   KS_e              => hb_keyCode( "e" ) , ;
   KS_f              => hb_keyCode( "f" ) , ;
   KS_g              => hb_keyCode( "g" ) , ;
   KS_h              => hb_keyCode( "h" ) , ;
   KS_i              => hb_keyCode( "i" ) , ;
   KS_j              => hb_keyCode( "j" ) , ;
   KS_k              => hb_keyCode( "k" ) , ;
   KS_l              => hb_keyCode( "l" ) , ;
   KS_m              => hb_keyCode( "m" ) , ;
   KS_n              => hb_keyCode( "n" ) , ;
   KS_o              => hb_keyCode( "o" ) , ;
   KS_p              => hb_keyCode( "p" ) , ;
   KS_q              => hb_keyCode( "q" ) , ;
   KS_r              => hb_keyCode( "r" ) , ;
   KS_s              => hb_keyCode( "s" ) , ;
   KS_t              => hb_keyCode( "t" ) , ;
   KS_u              => hb_keyCode( "u" ) , ;
   KS_v              => hb_keyCode( "v" ) , ;
   KS_w              => hb_keyCode( "w" ) , ;
   KS_x              => hb_keyCode( "x" ) , ;
   KS_y              => hb_keyCode( "y" ) , ;
   KS_z              => hb_keyCode( "z" ) , ;
   KS_1              => hb_keyCode( "1" ) , ;
   KS_2              => hb_keyCode( "2" ) , ;
   KS_3              => hb_keyCode( "3" ) , ;
   KS_4              => hb_keyCode( "4" ) , ;
   KS_5              => hb_keyCode( "5" ) , ;
   KS_6              => hb_keyCode( "6" ) , ;
   KS_7              => hb_keyCode( "7" ) , ;
   KS_8              => hb_keyCode( "8" ) , ;
   KS_9              => hb_keyCode( "9" ) , ;
   KS_0              => hb_keyCode( "0" ) , ;
   KS_DOT            => hb_keyCode( "." ) , ;
   KS_COMMA          => hb_keyCode( "," ) , ;
   KS_CTRL_A         => K_CTRL_A          , ;
   KS_CTRL_B         => K_CTRL_B          , ;
   KS_CTRL_C         => K_CTRL_C          , ;
   KS_CTRL_D         => K_CTRL_D          , ;
   KS_CTRL_E         => K_CTRL_E          , ;
   KS_CTRL_F         => K_CTRL_F          , ;
   KS_CTRL_G         => K_CTRL_G          , ;
   KS_CTRL_H         => K_CTRL_H          , ;
   KS_CTRL_I         => K_CTRL_I          , ;
   KS_CTRL_M         => K_CTRL_M          , ;
   KS_CTRL_N         => K_CTRL_N          , ;
   KS_CTRL_R         => K_CTRL_R          , ;
   KS_CTRL_S         => K_CTRL_S          , ;
   KS_CTRL_T         => K_CTRL_T          , ;
   KS_CTRL_U         => K_CTRL_U          , ;
   KS_CTRL_V         => K_CTRL_V          , ;
   KS_CTRL_W         => K_CTRL_W          , ;
   KS_CTRL_X         => K_CTRL_X          , ;
   KS_CTRL_Y         => K_CTRL_Y          , ;
   KS_CTRL_Z         => K_CTRL_Z          , ;
   KS_ALT_A          => K_ALT_A           , ;
   KS_ALT_B          => K_ALT_B           , ;
   KS_ALT_C          => K_ALT_C           , ;
   KS_ALT_D          => K_ALT_D           , ;
   KS_ALT_E          => K_ALT_E           , ;
   KS_ALT_F          => K_ALT_F           , ;
   KS_ALT_G          => K_ALT_G           , ;
   KS_ALT_H          => K_ALT_H           , ;
   KS_ALT_I          => K_ALT_I           , ;
   KS_ALT_J          => K_ALT_J           , ;
   KS_ALT_K          => K_ALT_K           , ;
   KS_ALT_L          => K_ALT_L           , ;
   KS_ALT_M          => K_ALT_M           , ;
   KS_ALT_N          => K_ALT_N           , ;
   KS_ALT_O          => K_ALT_O           , ;
   KS_ALT_P          => K_ALT_P           , ;
   KS_ALT_Q          => K_ALT_Q           , ;
   KS_ALT_R          => K_ALT_R           , ;
   KS_ALT_S          => K_ALT_S           , ;
   KS_ALT_T          => K_ALT_T           , ;
   KS_ALT_U          => K_ALT_U           , ;
   KS_ALT_V          => K_ALT_V           , ;
   KS_ALT_W          => K_ALT_W           , ;
   KS_ALT_X          => K_ALT_X           , ;
   KS_ALT_Y          => K_ALT_Y           , ;
   KS_ALT_Z          => K_ALT_Z           , ;
   KS_ALT_1          => K_ALT_1           , ;
   KS_ALT_2          => K_ALT_2           , ;
   KS_ALT_3          => K_ALT_3           , ;
   KS_ALT_4          => K_ALT_4           , ;
   KS_ALT_5          => K_ALT_5           , ;
   KS_ALT_6          => K_ALT_6           , ;
   KS_ALT_7          => K_ALT_7           , ;
   KS_ALT_8          => K_ALT_8           , ;
   KS_ALT_9          => K_ALT_9           , ;
   KS_ALT_0          => K_ALT_0           , ;
   KS_F1             => K_F1              , ;
   KS_F2             => K_F2              , ;
   KS_F3             => K_F3              , ;
   KS_F4             => K_F4              , ;
   KS_F5             => K_F5              , ;
   KS_F6             => K_F6              , ;
   KS_F7             => K_F7              , ;
   KS_F8             => K_F8              , ;
   KS_F9             => K_F9              , ;
   KS_F10            => K_F10             , ;
   KS_F11            => K_F11             , ;
   KS_F12            => K_F12             , ;
   KS_SH_F1          => K_SH_F1           , ;
   KS_SH_F2          => K_SH_F2           , ;
   KS_SH_F3          => K_SH_F3           , ;
   KS_SH_F4          => K_SH_F4           , ;
   KS_SH_F5          => K_SH_F5           , ;
   KS_SH_F6          => K_SH_F6           , ;
   KS_SH_F7          => K_SH_F7           , ;
   KS_SH_F8          => K_SH_F8           , ;
   KS_SH_F9          => K_SH_F9           , ;
   KS_SH_F10         => K_SH_F10          , ;
   KS_SH_F11         => K_SH_F11          , ;
   KS_SH_F12         => K_SH_F12          , ;
   KS_CTRL_F1        => K_CTRL_F1         , ;
   KS_CTRL_F2        => K_CTRL_F2         , ;
   KS_CTRL_F3        => K_CTRL_F3         , ;
   KS_CTRL_F4        => K_CTRL_F4         , ;
   KS_CTRL_F5        => K_CTRL_F5         , ;
   KS_CTRL_F6        => K_CTRL_F6         , ;
   KS_CTRL_F7        => K_CTRL_F7         , ;
   KS_CTRL_F8        => K_CTRL_F8         , ;
   KS_CTRL_F9        => K_CTRL_F9         , ;
   KS_CTRL_F10       => K_CTRL_F10        , ;
   KS_CTRL_F11       => K_CTRL_F11        , ;
   KS_CTRL_F12       => K_CTRL_F12        , ;
   KS_ALT_F1         => K_ALT_F1          , ;
   KS_ALT_F2         => K_ALT_F2          , ;
   KS_ALT_F3         => K_ALT_F3          , ;
   KS_ALT_F4         => K_ALT_F4          , ;
   KS_ALT_F5         => K_ALT_F5          , ;
   KS_ALT_F6         => K_ALT_F6          , ;
   KS_ALT_F7         => K_ALT_F7          , ;
   KS_ALT_F8         => K_ALT_F8          , ;
   KS_ALT_F9         => K_ALT_F9          , ;
   KS_ALT_F10        => K_ALT_F10         , ;
   KS_ALT_F11        => K_ALT_F11         , ;
   KS_ALT_F12        => K_ALT_F12         , ;
   KS_BS             => K_BS              , ;
   KS_ESC            => K_ESC             , ;
   KS_TAB            => K_TAB             , ;
   KS_SH_TAB         => K_SH_TAB          , ;
   KS_ALT_BS         => K_ALT_BS          , ;
   KS_ALT_ESC        => K_ALT_ESC         , ;
   KS_ALT_TAB        => K_ALT_TAB         , ;
   KS_ENTER          => K_ENTER           , ;
   KS_CTRL_ENTER     => K_CTRL_ENTER      , ;
   KS_ALT_ENTER      => K_ALT_ENTER       , ;
   KS_PAD_ENTER      => HB_KP_ENTER       , ;
   KS_PAD_CTRL_ENTER => HB_KP_CTRL_ENTER  , ;
   KS_PAD_ALT_ENTER  => KP_ALT_ENTER      , ;
   KS_INS            => K_INS             , ;
   KS_DEL            => K_DEL             , ;
   KS_HOME           => K_HOME            , ;
   KS_END            => K_END             , ;
   KS_PGUP           => K_PGUP            , ;
   KS_PGDN           => K_PGDN            , ;
   KS_UP             => K_UP              , ;
   KS_LEFT           => K_LEFT            , ;
   KS_DOWN           => K_DOWN            , ;
   KS_RIGHT          => K_RIGHT           , ;
   KS_CTRL_INS       => K_CTRL_INS        , ;
   KS_CTRL_DEL       => K_CTRL_DEL        , ;
   KS_CTRL_HOME      => K_CTRL_HOME       , ;
   KS_CTRL_END       => K_CTRL_END        , ;
   KS_CTRL_PGUP      => K_CTRL_PGUP       , ;
   KS_CTRL_PGDN      => K_CTRL_PGDN       , ;
   KS_CTRL_UP        => K_CTRL_UP         , ;
   KS_CTRL_LEFT      => K_CTRL_LEFT       , ;
   KS_CTRL_DOWN      => K_CTRL_DOWN       , ;
   KS_CTRL_RIGHT     => K_CTRL_RIGHT      , ;
   KS_ALT_INS        => K_ALT_INS         , ;
   KS_ALT_DEL        => K_ALT_DEL         , ;
   KS_ALT_HOME       => K_ALT_HOME        , ;
   KS_ALT_END        => K_ALT_END         , ;
   KS_ALT_PGUP       => K_ALT_PGUP        , ;
   KS_ALT_PGDN       => K_ALT_PGDN        , ;
   KS_ALT_UP         => K_ALT_UP          , ;
   KS_ALT_LEFT       => K_ALT_LEFT        , ;
   KS_ALT_DOWN       => K_ALT_DOWN        , ;
   KS_ALT_RIGHT      => K_ALT_RIGHT       , ;
   KS_PAD_INS        => HB_KP_INS         , ;
   KS_PAD_DEL        => HB_KP_DEL         , ;
   KS_PAD_HOME       => HB_KP_HOME        , ;
   KS_PAD_END        => HB_KP_END         , ;
   KS_PAD_PGUP       => HB_KP_PG_UP       , ;
   KS_PAD_PGDN       => HB_KP_PG_DN       , ;
   KS_PAD_UP         => HB_KP_UP          , ;
   KS_PAD_LEFT       => HB_KP_LEFT        , ;
   KS_PAD_DOWN       => HB_KP_DOWN        , ;
   KS_PAD_RIGHT      => HB_KP_RIGHT       , ;
   KS_CTRL_PAD_INS   => HB_KP_CTRL_INS    , ;
   KS_CTRL_PAD_DEL   => HB_KP_CTRL_DEL    , ;
   KS_CTRL_PAD_HOME  => HB_KP_CTRL_HOME   , ;
   KS_CTRL_PAD_END   => HB_KP_CTRL_END    , ;
   KS_CTRL_PAD_PGUP  => HB_KP_CTRL_PG_UP  , ;
   KS_CTRL_PAD_PGDN  => HB_KP_CTRL_PG_DN  , ;
   KS_CTRL_PAD_UP    => HB_KP_CTRL_UP     , ;
   KS_CTRL_PAD_LEFT  => HB_KP_CTRL_LEFT   , ;
   KS_CTRL_PAD_DOWN  => HB_KP_CTRL_DOWN   , ;
   KS_CTRL_PAD_RIGHT => HB_KP_CTRL_RIGHT  , ;
   KS_PAD_DIV        => KP_ALT_SLASH      , ;
   KS_PAD_MUL        => KP_ALT_ASTERISK   , ;
   KS_PAD_MINUS      => KP_ALT_MINUS      , ;
   KS_PAD_PLUS       => KP_ALT_PLUS       , ;
   KS_CTRL_PAD_DIV   => KP_CTRL_SLASH     , ;
   KS_CTRL_PAD_MUL   => KP_CTRL_ASTERISK  , ;
   KS_CTRL_PAD_MINUS => KP_CTRL_MINUS     , ;
   KS_CTRL_PAD_PLUS  => KP_CTRL_PLUS      , ;
   KS_ALT_PAD_DIV    => HB_KP_ALT_SLASH   , ;
   KS_ALT_PAD_MUL    => HB_KP_ALT_STAR    , ;
   KS_ALT_PAD_MINUS  => HB_KP_ALT_MINUS   , ;
   KS_ALT_PAD_PLUS   => HB_KP_ALT_PLUS    , ;
   KS_PAD_1          => 0                 , ; /* no Harbour equivalent */
   KS_PAD_2          => 0                 , ; /* no Harbour equivalent */
   KS_PAD_3          => 0                 , ; /* no Harbour equivalent */
   KS_PAD_4          => 0                 , ; /* no Harbour equivalent */
   KS_PAD_5          => 0                 , ; /* no Harbour equivalent */
   KS_PAD_6          => 0                 , ; /* no Harbour equivalent */
   KS_PAD_7          => 0                 , ; /* no Harbour equivalent */
   KS_PAD_8          => 0                 , ; /* no Harbour equivalent */
   KS_PAD_9          => 0                 , ; /* no Harbour equivalent */
   KS_PAD_0          => 0                 , ; /* no Harbour equivalent */
   KS_PAD_DECIMAL    => 0                 }   /* no Harbour equivalent */

FUNCTION __hbct_key_c_to_n( cKey )

   IF HB_ISSTRING( cKey )
      RETURN hb_HGetDef( sc_hCnv, cKey, 0 )
   ENDIF

   RETURN NIL

FUNCTION __hbct_key_n_to_c( nKey )

   LOCAL hKey

   IF HB_ISNUMERIC( nKey )
      FOR EACH hKey IN sc_hCnv
         IF hKey:__enumValue() == nKey
            RETURN hKey:__enumKey()
         ENDIF
      NEXT
   ENDIF

   RETURN hb_BChar( 0 ) + hb_BChar( 0 )

FUNCTION SetKXLat( cOrgKeyValue, cNewKeyValue )

   SWITCH PCount()
   CASE 0 ; RETURN hbct_SetKXLat()
   CASE 1 ; RETURN hbct_SetKXLat( __hbct_key_c_to_n( cOrgKeyValue ) )
   ENDSWITCH

   RETURN hbct_SetKXLat( __hbct_key_c_to_n( cOrgKeyValue ), __hbct_key_c_to_n( cNewKeyValue ) )

FUNCTION GetKXLat( cKeyValue )

   LOCAL xKey := hbct_GetKXLat( __hbct_key_c_to_n( cKeyValue ) )

   /* doc is unclear. should this return a numeric in these cases? */
   IF HB_ISNUMERIC( xKey )
      RETURN xKey
   ENDIF

   RETURN __hbct_key_n_to_c( xKey )

FUNCTION SetKXTab( cTrs )

   LOCAL hTrs := { => }
   LOCAL tmp

   IF HB_ISSTRING( cTrs )
      FOR tmp := 1 TO hb_BLen( cTrs ) STEP 4
         hTrs[ __hbct_key_c_to_n( hb_BSubStr( cTrs, tmp, 2 ) ) ] := __hbct_key_c_to_n( hb_BSubStr( cTrs, tmp + 2, 2 ) )
      NEXT
   ENDIF

   RETURN hbct_SetKXTab( hTrs )

FUNCTION GetKXTab()

   LOCAL cTrs := ""
   LOCAL hTrs := hbct_GetKXTab()
   LOCAL hKey

   FOR EACH hKey IN hTrs
      cTrs += __hbct_key_n_to_c( hKey:__enumKey() ) + __hbct_key_n_to_c( hKey:__enumValue() )
   NEXT

   RETURN cTrs

/* Harbour extensions using standard numeric key values */

FUNCTION hbct_SetKXLat( nOrgKeyValue, nNewKeyValue )

   LOCAL lAccepted := .F.

   IF PCount() == 0
      IF hb_mutexLock( s_hMutex )
         lAccepted := .T.
         IF ! Empty( s_hTrs )
            hb_HClear( s_hTrs )
            hb_gtInfo( HB_GTI_INKEYFILTER, NIL )
         ENDIF
         hb_mutexUnlock( s_hMutex )
      ENDIF
   ELSE
      IF HB_ISNUMERIC( nOrgKeyValue ) .AND. nOrgKeyValue != 0
         IF hb_mutexLock( s_hMutex )
            IF PCount() == 1
               IF nOrgKeyValue $ s_hTrs
                  lAccepted := .T.
                  hb_HDel( s_hTrs, nOrgKeyValue )
                  IF Empty( s_hTrs )
                     hb_gtInfo( HB_GTI_INKEYFILTER, NIL )
                  ENDIF
               ENDIF
            ELSEIF HB_ISNUMERIC( nNewKeyValue )
               /* refuse overwriting custom HB_GTI_INKEYFILTER */
               IF hb_gtInfo( HB_GTI_INKEYFILTER ) == NIL .OR. ! Empty( s_hTrs )
                  lAccepted := .T.
                  IF Empty( s_hTrs )
                     hb_gtInfo( HB_GTI_INKEYFILTER, {| nKey | __hbct_kxlat( nKey ) } )
                  ENDIF
                  s_hTrs[ nOrgKeyValue ] := nNewKeyValue
               ENDIF
            ENDIF
            hb_mutexUnlock( s_hMutex )
         ENDIF
      ENDIF
   ENDIF

   RETURN lAccepted

FUNCTION hbct_GetKXLat( nKeyValue )

   LOCAL nNewValue := 0

   IF HB_ISNUMERIC( nKeyValue )
      IF hb_mutexLock( s_hMutex )
         IF nKeyValue $ s_hTrs
            nNewValue := s_hTrs[ nKeyValue ]
            IF nNewValue == 0
               nNewValue := -1
            ENDIF
         ENDIF
         hb_mutexUnlock( s_hMutex )
      ENDIF
   ENDIF

   RETURN nNewValue

FUNCTION hbct_SetKXTab( hTrs )

   LOCAL lAccepted := .F.

   IF HB_ISHASH( hTrs )
      IF hb_mutexLock( s_hMutex )
         IF hb_gtInfo( HB_GTI_INKEYFILTER ) == NIL .OR. ! Empty( s_hTrs )
            lAccepted := .T.
            IF Empty( s_hTrs ) .AND. ! Empty( hTrs )
               hb_gtInfo( HB_GTI_INKEYFILTER, {| nKey | __hbct_kxlat( nKey ) } )
            ELSEIF ! Empty( s_hTrs ) .AND. Empty( hTrs )
               hb_gtInfo( HB_GTI_INKEYFILTER, NIL )
            ENDIF
            hb_HClear( s_hTrs )
            hb_HCopy( hTrs, s_hTrs )
         ENDIF
         hb_mutexUnlock( s_hMutex )
      ENDIF
   ENDIF

   RETURN lAccepted

FUNCTION hbct_GetKXTab()

   LOCAL xRetVal

   IF hb_mutexLock( s_hMutex )
      xRetVal := hb_HClone( s_hTrs )
      hb_mutexUnlock( s_hMutex )
   ENDIF

   RETURN xRetVal

STATIC FUNCTION __hbct_kxlat( nKey )

   IF hb_mutexLock( s_hMutex )
      nKey := hb_HGetDef( s_hTrs, nKey, nKey )
      hb_mutexUnlock( s_hMutex )
   ENDIF

   RETURN nKey
