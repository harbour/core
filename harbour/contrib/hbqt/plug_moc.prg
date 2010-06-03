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

FUNCTION hbmk2_plugin_moc( hbmk2 )
   LOCAL cRetVal := ""

   LOCAL cMOC_BIN
   LOCAL aMOC
   LOCAL aMOC_Dst

   LOCAL cCommand
   LOCAL cDst
   LOCAL nError
   LOCAL tmp, tmp1, tmp2

   LOCAL lBuildIt

   SWITCH hbmk2[ "cSTATE" ]
   CASE "pre_all"

      aMOC_Dst := {}

      /* Gather input parameters */

      aMOC := {}
      FOR EACH tmp IN hbmk2[ "params" ]
         IF Lower( hbmk2_FNameExtGet( tmp ) ) == ".h" .OR. ;
            Lower( hbmk2_FNameExtGet( tmp ) ) == ".hpp"
            AAdd( aMOC, tmp )
         ENDIF
      NEXT

      IF ! Empty( aMOC )

         /* Detect 'moc' tool location */

         cMOC_BIN := GetEnv( "MOC_BIN" )
         IF Empty( cMOC_BIN )
            IF Empty( GetEnv( "HB_QT_MOC_BIN" ) )
               IF hbmk2[ "cPLAT" ] == "win"
                  cMOC_BIN := GetEnv( "HB_WITH_QT" ) + "\..\bin\moc.exe"
                  IF ! hb_FileExists( cMOC_BIN )
                     hbmk2_OutErr( hbmk2, "HB_WITH_QT points to incomplete QT installation. moc executable not found." )
                     RETURN NIL
                  ENDIF
               ELSE
                  cMOC_BIN := hbmk2_FindInPath( "moc", GetEnv( "PATH" ) + hb_osPathListSeparator() + "/opt/qtsdk/qt/bin" )
                  IF Empty( cMOC_BIN )
                     cMOC_BIN := hbmk2_FindInPath( "moc-qt4", GetEnv( "PATH" ) + hb_osPathListSeparator() + "/opt/qtsdk/qt/bin" )
                     IF Empty( cMOC_BIN )
                        hbmk2_OutErr( hbmk2, "HB_QT_MOC_BIN not set, could not autodetect" )
                        RETURN NIL
                     ENDIF
                  ENDIF
               ENDIF
               hbmk2_OutStd( hbmk2, "Using QT 'moc' executable: " + cMOC_BIN + " (autodetected)" )
            ELSE
               IF hb_FileExists( GetEnv( "HB_QT_MOC_BIN" ) )
                  cMOC_BIN := GetEnv( "HB_QT_MOC_BIN" )
                  hbmk2_OutStd( hbmk2, "Using QT 'moc' executable: " + cMOC_BIN )
               ELSE
                  hbmk2_OutErr( hbmk2, "HB_QT_MOC_BIN points to non-existent file. Make sure to set it to full path and filename of moc executable." )
                  RETURN NIL
               ENDIF
            ENDIF
         ENDIF

         /* Execute 'moc' commands on input files */

         FOR EACH tmp IN aMOC

            cDst := hbmk2_FNameDirExtSet( "moc_" + hbmk2_FNameNameGet( tmp ), hbmk2[ "cWorkDir" ], ".cpp" )

            IF hbmk2[ "lINC" ] .AND. ! hbmk2[ "lREBUILD" ]
               lBuildIt := ! hb_FGetDateTime( cDst, @tmp2 ) .OR. ;
                           ! hb_FGetDateTime( tmp, @tmp1 ) .OR. ;
                           tmp1 > tmp2
            ELSE
               lBuildIt := .T.
            ENDIF

            IF lBuildIt
               cCommand := cMOC_BIN +;
                           " " + hbmk2_FNameEscape( hbmk2_PathSepToTarget( hbmk2, tmp ), hbmk2[ "nCmd_Esc" ] ) +;
                           " -o " + hbmk2_FNameEscape( hbmk2_PathSepToTarget( hbmk2, cDst ), hbmk2[ "nCmd_Esc" ] )

               IF hbmk2[ "lTRACE" ]
                  IF ! hbmk2[ "lQUIET" ]
                     hbmk2_OutStd( hbmk2, I_( "'moc' command:" ) )
                  ENDIF
                  hbmk2_OutStdRaw( cCommand )
               ENDIF

               IF ! hbmk2[ "lDONTEXEC" ] .AND. ( nError := hb_processRun( cCommand ) ) != 0
                  hbmk2_OutErr( hbmk2, hb_StrFormat( I_( "Error: Running 'moc' executable. %1$s" ), hb_ntos( nError ) ) )
                  IF ! hbmk2[ "lQUIET" ]
                     hbmk2_OutErrRaw( cCommand )
                  ENDIF
                  IF ! hbmk2[ "lIGNOREERROR" ]
                     cRetVal := "error"
                     EXIT
                  ENDIF
               ELSE
                  hbmk2_AddInput_CPP( hbmk2, cDst )
                  AAdd( aMOC_Dst, cDst )
               ENDIF
            ELSE
               hbmk2_AddInput_CPP( hbmk2, cDst )
               AAdd( aMOC_Dst, cDst )
            ENDIF
         NEXT
      ENDIF

      hbmk2[ "vars" ][ "aMOC_Dst" ] := aMOC_Dst

      EXIT

   CASE "post_all"

      IF ! hbmk2[ "lINC" ] .OR. hbmk2[ "lCLEAN" ]
         AEval( hbmk2[ "vars" ][ "aMOC_Dst" ], {| tmp | FErase( tmp ) } )
      ENDIF

      EXIT

   ENDSWITCH

   RETURN cRetVal
