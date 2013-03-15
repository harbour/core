/*
 * hbmk2 plugin script, implementing support for bison
 *
 * Copyright 2011-2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
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
 * their web site at http://www.gnu.org/).
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#define I_( x )                 hb_i18n_gettext( x )

#if defined( __HBSCRIPT__HBMK_PLUGIN )

FUNCTION hbmk_plugin_bison( hbmk )

   LOCAL cRetVal := ""

   LOCAL cSrc
   LOCAL cDst
   LOCAL tSrc
   LOCAL tDst

   LOCAL cCommand
   LOCAL nError
   LOCAL lBuildIt

   SWITCH hbmk[ "cSTATE" ]
   CASE "init"

      hbmk_Register_Input_File_Extension( hbmk, ".y" )

      EXIT

   CASE "pre_all"

      /* Gather input parameters */

      hbmk[ "vars" ][ "aBIS_Src" ] := {}
      FOR EACH cSrc IN hbmk[ "params" ]
         IF Left( cSrc, Len( "-bisonflag=" ) ) == "-bisonflag="
            /* TODO: process bison flags */
         ELSE
            SWITCH Lower( hb_FNameExt( cSrc ) )
            CASE ".y"
               AAdd( hbmk[ "vars" ][ "aBIS_Src" ], cSrc )
               EXIT
            ENDSWITCH
         ENDIF
      NEXT

      /* Create output file lists */

      hbmk[ "vars" ][ "aBIS_Dst" ] := {}
      FOR EACH cSrc IN hbmk[ "vars" ][ "aBIS_Src" ]
         cDst := hbmk_FNameDirExtSet( hb_FNameName( cSrc ) + "y", hbmk[ "cWorkDir" ], ".c" )
         AAdd( hbmk[ "vars" ][ "aBIS_Dst" ], cDst )
         hbmk_AddInput_C( hbmk, cDst )
      NEXT

      /* Detect tool locations */

      IF ! hbmk[ "lCLEAN" ]
         IF ! Empty( hbmk[ "vars" ][ "aBIS_Src" ] )
            hbmk[ "vars" ][ "cBIS_BIN" ] := tool_detect( hbmk, "bison" )
            IF Empty( hbmk[ "vars" ][ "cBIS_BIN" ] )
               cRetVal := I_( "Required 'bison' tool not found" )
            ENDIF
         ENDIF
      ENDIF

      EXIT

   CASE "pre_prg"

      EXIT

   CASE "pre_c"

      IF ! hbmk[ "lCLEAN" ] .AND. ;
         ! Empty( hbmk[ "vars" ][ "aBIS_Src" ] )

         IF ! Empty( hbmk[ "vars" ][ "cBIS_BIN" ] )

            /* Execute 'bison' commands on input files */

            FOR EACH cSrc, cDst IN hbmk[ "vars" ][ "aBIS_Src" ], hbmk[ "vars" ][ "aBIS_Dst" ]

               IF hbmk[ "lINC" ] .AND. ! hbmk[ "lREBUILD" ]
                  lBuildIt := ;
                     ! hb_FGetDateTime( cDst, @tDst ) .OR. ;
                     ! hb_FGetDateTime( cSrc, @tSrc ) .OR. ;
                     tSrc > tDst
               ELSE
                  lBuildIt := .T.
               ENDIF

               IF lBuildIt

                  cCommand := hbmk[ "vars" ][ "cBIS_BIN" ] + ;
                     " -d -p hb_comp" + ;
                     " -o " + hbmk_FNameEscape( hbmk, hbmk_PathSepToTarget( hbmk, cDst ) ) + ;
                     " " + hbmk_FNameEscape( hbmk, hbmk_PathSepToTarget( hbmk, cSrc ) )

                  IF hbmk[ "lTRACE" ]
                     IF ! hbmk[ "lQUIET" ]
                        hbmk_OutStd( hbmk, I_( "'bison' command:" ) )
                     ENDIF
                     hbmk_OutStdRaw( hbmk, cCommand )
                  ENDIF

                  IF ! hbmk[ "lDONTEXEC" ] .AND. ( nError := hb_processRun( cCommand ) ) != 0
                     hbmk_OutErr( hbmk, hb_StrFormat( I_( "Error: Running 'bison' executable. %1$s" ), hb_ntos( nError ) ) )
                     IF ! hbmk[ "lQUIET" ]
                        hbmk_OutErrRaw( hbmk, cCommand )
                     ENDIF
                     IF ! hbmk[ "lIGNOREERROR" ]
                        cRetVal := "error"
                        EXIT
                     ENDIF
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      ENDIF

      EXIT

   CASE "post_all"

      IF ! hbmk[ "lINC" ] .OR. hbmk[ "lCLEAN" ]
         AEval( hbmk[ "vars" ][ "aBIS_Dst" ], {| tmp | ;
            FErase( tmp ), ;
            FErase( hb_FNameExtSet( tmp, ".h" ) ) } )
      ENDIF

      EXIT

   ENDSWITCH

   RETURN cRetVal

STATIC FUNCTION tool_detect( hbmk, cName )

   LOCAL cBIN
   LOCAL aEnvList := { "HB_BISONPATH" }

   IF Empty( cBIN := GetEnv( "BISON_BIN" ) )

      cName += hbmk[ "cCCEXT" ]

      IF Empty( GetEnv( "HB_BISONPATH" ) ) .OR. ;
         ! hb_FileExists( cBIN := hb_DirSepAdd( GetEnv( "HB_BISONPATH" ) ) + cName )

         cBIN := hbmk_FindInPath( cName, GetEnv( "PATH" ) )
         IF Empty( cBIN )
            hbmk_OutErr( hbmk, hb_StrFormat( "%1$s not set, could not autodetect '%2$s' executable", hbmk_ArrayToList( aEnvList, ", " ), cName ) )
            RETURN NIL
         ENDIF
      ENDIF
   ENDIF

   RETURN cBIN

#else

PROCEDURE Main()

   ?? "Cannot be run in standalone mode. Use it with -plugin= option of hbmk2."
   ?

   RETURN

#endif
