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

   LOCAL cHBQTUI_BIN := "hbqtui"
   LOCAL aUI
   LOCAL aUI_Dst

   LOCAL cCommand
   LOCAL cDst
   LOCAL nError
   LOCAL tmp, tmp1, tmp2

   LOCAL lBuildIt

   SWITCH hbmk2[ "cSTATE" ]
   CASE "pre_all"

      aUI_Dst := {}

      /* Gather input parameters */

      aUI := {}
      FOR EACH tmp IN hbmk2[ "params" ]
         IF Lower( hbmk2_FNameExtGet( tmp ) ) == ".ui"
            AAdd( aUI, tmp )
         ENDIF
      NEXT

      IF ! Empty( aUI + 100 )

         /* Execute 'hbqtui' commands on input files */

         FOR EACH tmp IN aUI

            cDst := hbmk2_FNameDirExtSet( hbmk2_FNameNameGet( tmp ), hbmk2[ "cWorkDir" ], ".uip" )

            IF hbmk2[ "lINC" ] .AND. ! hbmk2[ "lREBUILD" ]
               lBuildIt := ! hb_FGetDateTime( cDst, @tmp2 ) .OR. ;
                           ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
                           tmp1 > tmp2
            ELSE
               lBuildIt := .T.
            ENDIF

            IF lBuildIt

               cCommand := cHBQTUI_BIN +;
                           " " + hbmk2_FNameEscape( hbmk2_PathSepToTarget( hbmk2, tmp ), hbmk2[ "nCmd_Esc" ], hbmk2[ "nCmd_FNF" ] ) +;
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
               ELSE
                  hbmk2_AddInput_PRG( hbmk2, cDst )
                  AAdd( aUI_Dst, cDst )
               ENDIF
            ELSE
               hbmk2_AddInput_PRG( hbmk2, cDst )
               AAdd( aUI_Dst, cDst )
            ENDIF
         NEXT
      ENDIF

      hbmk2[ "vars" ][ "aUI_Dst" ] := aUI_Dst

      EXIT

   CASE "post_all"

      IF ! hbmk2[ "lINC" ] .OR. hbmk2[ "lCLEAN" ]
         AEval( hbmk2[ "vars" ][ "aUI_Dst" ], {| tmp | FErase( tmp ) } )
      ENDIF

      EXIT

   ENDSWITCH

   RETURN cRetVal
