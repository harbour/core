/*
 * $Id$
 */

/*
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 *
 * See COPYING for licensing terms.
 */

FUNCTION hbmk2_plugin_moc( cState, hbmk2, hVars )
   LOCAL cMOC_BIN

   SWITCH cState
   CASE "pre_c"

      cMOC_BIN := GetEnv( "MOC_BIN" )
      IF Empty( cMOC_BIN )
         IF Empty( GetEnv( "HB_QT_MOC_BIN" ) )
            IF hbmk2[ "cPLAT" ] == "win"
               cMOC_BIN := GetEnv( "HB_WITH_QT" ) + "\..\bin\moc.exe"
               IF ! hb_FileExists( cMOC_BIN )
                  hbmk2_OutErr( hbmk2[ "ctx" ], "HB_WITH_QT points to incomplete QT installation. moc executable not found." )
                  RETURN NIL
               ENDIF
            ELSE
               cMOC_BIN := hbmk2_FindInPath( "moc", "/opt/qtsdk/qt/bin" )
               IF Empty( cMOC_BIN )
                  cMOC_BIN := hbmk2_FindInPath( "moc", "/opt/qtsdk/qt/bin" )
                  IF Empty( cMOC_BIN )
                     hbmk2_OutErr( hbmk2[ "ctx" ], "HB_QT_MOC_BIN not set, could not autodetect" )
                     RETURN NIL
                  ENDIF
               ENDIF
            ENDIF
            hbmk2_OutStd( hbmk2[ "ctx" ], "Using QT 'moc' executable: " + cMOC_BIN + " (autodetected)" )
         ELSE
            IF hb_FileExists( GetEnv( "HB_QT_MOC_BIN" ) )
               cMOC_BIN := GetEnv( "HB_QT_MOC_BIN" )
               hbmk2_OutStd( hbmk2[ "ctx" ], "Using QT 'moc' executable: " + cMOC_BIN )
            ELSE
               hbmk2_OutErr( hbmk2[ "ctx" ], "HB_QT_MOC_BIN points to non-existent file. Make sure to set it to full path and filename of moc executable." )
               RETURN NIL
            ENDIF
         ENDIF
      ENDIF

      hVars[ "MOC_BIN" ] := cMOC_BIN

      EXIT
   CASE "post_all"
      hbmk2_OutStd( hbmk2[ "ctx" ], "POST_ALL: " + hVars[ "MOC_BIN" ] )
   OTHERWISE
      IF hbmk2[ "lTRACE" ]
         hbmk2_OutStd( hbmk2[ "ctx" ], "@@ Entered plugin: " + cState )
      ENDIF
   ENDSWITCH

   RETURN NIL
