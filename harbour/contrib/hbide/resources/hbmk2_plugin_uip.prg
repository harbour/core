/*
 * $Id$
 */

/*
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
 *
 * See COPYING for licensing terms.
 */

#define I_( x )                 hb_i18n_gettext( x )

FUNCTION hbmk2_plugin_uip( hbmk2 )
   LOCAL cRetVal := ""

   LOCAL cHBQTUI_BIN

   LOCAL aUI
   LOCAL aUI_Dst

   LOCAL cSrc
   LOCAL cDst
   LOCAL tSrc
   LOCAL tDst

   LOCAL cCommand
   LOCAL nError
   LOCAL lBuildIt

   LOCAL tmp

   SWITCH hbmk2[ "cSTATE" ]
   CASE "pre_all"

      /* Gather input parameters */

      aUI := {}
      aUI_Dst := {}

      FOR EACH tmp IN hbmk2[ "params" ]
         IF Lower( hbmk2_FNameExtGet( tmp ) ) == ".ui"
            AAdd( aUI, tmp )
            AAdd( aUI_Dst, cDst := hbmk2_FNameDirExtSet( hbmk2_FNameNameGet( cSrc ), hbmk2[ "cWorkDir" ], ".uip" ) )
            hbmk2_AddInput_PRG( hbmk2, cDst )
         ENDIF
      NEXT

      hbmk2[ "vars" ][ "aUI" ] := aUI
      hbmk2[ "vars" ][ "aUI_Dst" ] := aUI_Dst

      EXIT

   CASE "pre_prg"

      IF ! Empty( hbmk2[ "vars" ][ "aUI" ] )

         /* Detect 'hbqtui' tool location */

         IF Empty( GetEnv( "HBQTUI_BIN" ) )
            cHBQTUI_BIN := hbmk2_FindInPath( "hbqtui", GetEnv( "PATH" ) )
            IF Empty( cHBQTUI_BIN )
               hbmk2_OutErr( hbmk2, "HBQTUI_BIN not set, could not autodetect" )
               RETURN NIL
            ENDIF
            IF hbmk2[ "lINFO" ]
               hbmk2_OutStd( hbmk2, "Using 'hbqtui' executable: " + cHBQTUI_BIN + " (autodetected)" )
            ENDIF
         ELSE
            IF hb_FileExists( GetEnv( "HBQTUI_BIN" ) )
               cHBQTUI_BIN := GetEnv( "HBQTUI_BIN" )
               IF hbmk2[ "lINFO" ]
                  hbmk2_OutStd( hbmk2, "Using 'hbqtui' executable: " + cHBQTUI_BIN )
               ENDIF
            ELSE
               hbmk2_OutErr( hbmk2, "HBQTUI_BIN points to non-existent file. Make sure to set it to full path and filename of hbqtui executable." )
               RETURN NIL
            ENDIF
         ENDIF

         /* Execute 'hbqtui' commands on input files */

         FOR EACH cSrc, cDst IN hbmk2[ "vars" ][ "aUI" ], hbmk2[ "vars" ][ "aUI_Dst" ]

            IF hbmk2[ "lINC" ] .AND. ! hbmk2[ "lREBUILD" ]
               lBuildIt := ! hb_FGetDateTime( cDst, @tDst ) .OR. ;
                           ! hb_FGetDateTime( cSrc, @tSrc ) .OR. ;
                           tSrc > tDst
            ELSE
               lBuildIt := .T.
            ENDIF

            IF lBuildIt

               cCommand := cHBQTUI_BIN +;
                           " " + hbmk2_FNameEscape( hbmk2_PathSepToTarget( hbmk2, cSrc ), hbmk2[ "nCmd_Esc" ], hbmk2[ "nCmd_FNF" ] ) +;
                           " -o" + hbmk2_FNameEscape( hbmk2_PathSepToTarget( hbmk2, cDst ), hbmk2[ "nCmd_Esc" ], hbmk2[ "nCmd_FNF" ] )

               IF hbmk2[ "lTRACE" ]
                  IF ! hbmk2[ "lQUIET" ]
                     hbmk2_OutStd( hbmk2, I_( "'hbqtui' command:" ) )
                  ENDIF
                  hbmk2_OutStdRaw( cCommand )
               ENDIF

               IF ! hbmk2[ "lDONTEXEC" ] .AND. ( nError := hb_processRun( cCommand ) ) != 0
                  hbmk2_OutErr( hbmk2, hb_StrFormat( I_( "Error: Running 'hbqtui' executable. %1$s" ), hb_ntos( nError ) ) )
                  IF ! hbmk2[ "lQUIET" ]
                     hbmk2_OutErrRaw( cCommand )
                  ENDIF
                  IF ! hbmk2[ "lIGNOREERROR" ]
                     cRetVal := "error"
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
         NEXT
      ENDIF

      EXIT

   CASE "post_all"

      IF ! hbmk2[ "lINC" ] .OR. hbmk2[ "lCLEAN" ]
         AEval( hbmk2[ "vars" ][ "aUI_Dst" ], {| tmp | FErase( tmp ) } )
      ENDIF

      EXIT

   ENDSWITCH

   RETURN cRetVal
