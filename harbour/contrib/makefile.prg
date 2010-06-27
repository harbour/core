/*
 * $Id$
 */

/*
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * See COPYING for licensing terms.
 */

/* TODO:
     1. error handling / reporting / feedback
     2. copy headers and other stuff (hbide)
     3. add .dll generation for contrib libs
     4. first do all the 'clean's
*/

#pragma warninglevel=3

/* TOFIX: Ugly hack to avoid #include "directry.ch" */
#define F_NAME          1       /* File name */
#define F_ATTR          5       /* File attribute */

#define _PS_            hb_osPathSeparator()

STATIC s_lTest

PROCEDURE Main( ... )

   LOCAL cBase := "contrib/"

   LOCAL aImpLibs := {;
      "gtalleg/gtalleg.hbi"        ,;
      "hbblat/hbblat.hbi"          ,;
      "hbcairo/hbcairo.hbi"        ,;
      "hbcurl/hbcurl.hbi"          ,;
      "hbfbird/hbfbird.hbi"        ,;
      "hbfimage/hbfimage.hbi"      ,;
      "hbgd/hbgd.hbi"              ,;
      "hbhpdf/hbhpdf.hbi"          ,;
      "hbmysql/hbmysql.hbi"        ,;
      "hbpgsql/hbpgsql.hbi"        ,;
      "hbssl/hbssl.hbi"            ,;
      "rddads/rddads.hbi"          ,;
      "sddfb/sddfb.hbi"            ,;
      "sddmy/sddmy.hbi"            ,;
      "sddoci/sddoci.hbi"          ,;
      "sddpg/sddpg.hbi"            }

   LOCAL aLibsPass1 := {;
      "hbblink/hbblink.hbp"        ,;
      "hbbz2/hbbz2.hbp"            ,;
      "hbclipsm/hbclipsm.hbp"      ,;
      "hbcomm/hbcomm.hbp"          ,;
      "hbct/hbct.hbp"              ,;
      "hbfoxpro/hbfoxpro.hbp"      ,;
      "hbfship/hbfship.hbp"        ,;
      "hbgt/hbgt.hbp"              ,;
      "hbmemio/hbmemio.hbp"        ,;
      "hbmisc/hbmisc.hbp"          ,;
      "hbmzip/hbmzip.hbp"          ,;
      "hbnetio/hbnetio.hbp"        ,;
      "hbnf/hbnf.hbp"              ,;
      "hbodbc/hbodbc.hbp"          ,;
      "hbsms/hbsms.hbp"            ,;
      "hbsqlit3/hbsqlit3.hbp"      ,;
      "hbtpathy/hbtpathy.hbp"      ,;
      "hbwin/hbwin.hbp"            ,;
      "hbxpp/hbxpp.hbp"            ,;
      "rddbmcdx/hbbmcdx.hbp"       }

   LOCAL aLibsPass2 := {;
      "gtalleg/gtalleg.hbp"        ,;
      "gtalleg/gtallegs.hbp"       ,;
      "hbblat/hbblat.hbp"          ,;
      "hbcairo/hbcairo.hbp"        ,;
      "hbcups/hbcups.hbp"          ,;
      "hbcurl/hbcurl.hbp"          ,;
      "hbcurl/hbcurls.hbp"         ,;
      "hbfbird/hbfbird.hbp"        ,;
      "hbfimage/hbfimage.hbp"      ,;
      "hbgd/hbgd.hbp"              ,; /* uses: hbct */
      "hbhpdf/hbhpdf.hbp"          ,;
      "hbmysql/hbmysql.hbp"        ,;
      "hbpgsql/hbpgsql.hbp"        ,;
      "hbqt/hbqt.hbp"              ,;
      "hbqt/hbqtcore.hbp"          ,;
      "hbqt/hbqtcores.hbp"         ,;
      "hbqt/hbqtgui.hbp"           ,;
      "hbqt/hbqtguis.hbp"          ,;
      "hbqt/hbqtnetwork.hbp"       ,;
      "hbqt/hbqtnetworks.hbp"      ,;
      "hbqt/hbqts.hbp"             ,;
      "hbssl/hbssl.hbp"            ,;
      "hbssl/hbssls.hbp"           ,;
      "hbxbp/hbxbp.hbp"            ,; /* uses: hbqt */
      "rddads/rddads.hbp"          ,;
      "rddsql/rddsql.hbp"          ,;
      "sddfb/sddfb.hbp"            ,; /* uses: rddsql */
      "sddmy/sddmy.hbp"            ,; /* uses: rddsql */
      "sddoci/sddoci.hbp"          ,; /* uses: rddsql */
      "sddodbc/sddodbc.hbp"        ,; /* uses: rddsql */
      "sddpg/sddpg.hbp"            ,; /* uses: rddsql */
      "sddsqlt3/sddsqlt3.hbp"      }  /* uses: rddsql */

   LOCAL aLibsPass3 := {;
      "gtwvg/gtwvg.hbp"            ,; /* uses: hbwin */
      "hbtip/hbtip.hbp"            ,;
      "hbtip/hbtipssl.hbp"         ,; /* uses: hbssl */
      "hbziparc/hbziparc.hbp"      ,; /* uses: hbmzip */
      "xhb/xhb.hbp"                }  /* uses: hbct, hbtip, hbwin */

   LOCAL aUtils := {;
      "hbnetio/utils/netiosrv.hbp" ,; /* uses: hbnetio */
      "hbqt/utils/hbqtui.hbp"      ,; /* uses: hbqt */
      "hbide/hbide.hbp"            }  /* uses: hbxbp, hbqt */

   LOCAL aAll := {;
      aImpLibs ,;
      aLibsPass1 ,;
      aLibsPass2 ,;
      aLibsPass3 ,;
      aUtils,;
      hb_ATokens( GetEnv( "HB_ADDONS_LIB" ),, .T. ),;
      hb_ATokens( GetEnv( "HB_ADDONS_BIN" ),, .T. ) }

   LOCAL aAllType := {;
      "implib" ,;
      "lib" ,;
      "lib" ,;
      "lib" ,;
      "bin" ,;
      "lib" ,;
      "bin" }

   LOCAL chunk
   LOCAL cProject
   LOCAL cProjectDir
   LOCAL cType
   LOCAL cInstallDirVar
   LOCAL cTargetDir

   LOCAL cOptions := ""
   LOCAL lBuildName

   LOCAL cFilter := GetEnv( "HB_CONTRIBLIBS" )
   LOCAL aFilter
   LOCAL lFilterNegative

   LOCAL aParams := hb_AParams()
   LOCAL tmp

   IF Empty( GetEnv( "HB_PLATFORM" ) ) .OR. ;
      Empty( GetEnv( "HB_COMPILER" ) ) .OR. ;
      Empty( GetEnv( "HB_HOST_BIN_DIR" ) )
      ErrorLevel( 9 )
      RETURN
   ENDIF

   IF cFilter == "no"
      ErrorLevel( 0 )
      RETURN
   ENDIF

   SetCancel( .F. )

   /* Converting build options to hbmk2 options */

   IF GetEnv( "HB_BUILD_MODE" ) == "cpp"
      cOptions += " -cpp=yes"
   ELSEIF GetEnv( "HB_BUILD_MODE" ) == "c"
      cOptions += " -cpp=no"
   ENDIF
   IF GetEnv( "HB_BUILD_DEBUG" ) == "yes"
      cOptions += " -debug"
   ENDIF
   IF ! Empty( GetEnv( "HB_BUILD_NAME" ) )
      cOptions += " -build=" + GetEnv( "HB_BUILD_NAME" )
      lBuildName := .T.
   ELSE
      lBuildName := .F.
   ENDIF

   FOR EACH tmp IN aParams
      tmp := Lower( tmp )
   NEXT

   s_lTest := AScan( aParams, "test" ) > 0

   /* Parse filter */

   aFilter := iif( Empty( cFilter ), {}, hb_ATokens( cFilter,, .T. ) )
   IF Len( aFilter ) >= 1 .AND. aFilter[ 1 ] == "no"
      hb_ADel( aFilter, 1, .T. )
      lFilterNegative := .T.
   ELSE
      lFilterNegative := .F.
   ENDIF

   /* Start building */

   FOR EACH chunk, cType IN aAll, aAllType
      FOR EACH cProject IN chunk

         IF ! Empty( cProject )

            cProject := StrTran( cProject, "\", "/" )

            IF ( tmp := At( "/", cProject ) ) > 0
               cProjectDir := Left( cProject, tmp - 1 )
            ELSE
               cProjectDir := ""
            ENDIF

            IF Empty( aFilter ) .OR. ;
               iif( lFilterNegative,;
                  AScan( aFilter, {| tmp | tmp == cProjectDir } ) == 0,;
                  AScan( aFilter, {| tmp | tmp == cProjectDir } ) > 0 )

               SWITCH cType
               CASE "lib"
               CASE "implib"
                  cTargetDir := "lib/${hb_plat}/${hb_comp}" + iif( lBuildName, "/${hb_build}", "" )
                  cInstallDirVar := "HB_LIB_INSTALL_"
                  EXIT
               CASE "bin"
                  cTargetDir := "bin/${hb_plat}/${hb_comp}" + iif( lBuildName, "/${hb_build}", "" )
                  cInstallDirVar := "HB_BIN_INSTALL_"
                  EXIT
               ENDSWITCH

               IF AScan( aParams, "clean" ) > 0
                  call_hbmk2( cBase + cProject, cTargetDir, cOptions + " -clean", .F. )
               ENDIF
               IF AScan( aParams, "install" ) > 0 .AND. ;
                  ! Empty( GetEnv( cInstallDirVar ) ) .AND. ;
                  ( ! ( cType == "implib" ) .OR. GetEnv( "HB_BUILD_IMPLIB" ) == "yes" )
                  call_hbmk2( cBase + cProject, cTargetDir, cOptions + " -instpath=${" + cInstallDirVar + "}", .F. )
               ELSEIF !( Len( aParams ) == 1 .AND. aParams[ 1 ] == "clean" )
                  call_hbmk2( cBase + cProject, cTargetDir, cOptions + " -inc", .F. )
               ENDIF

               IF cType == "lib" .AND. GetEnv( "HB_BUILD_CONTRIB_DLL" ) == "yes" .AND. ;
                  hb_FileExists( FN_ExtSet( cBase + cProject, ".hbc" ) )

                  cInstallDirVar := "HB_BIN_INSTALL_"

                  IF AScan( aParams, "clean" ) > 0
                     call_hbmk2( cBase + cProject, cTargetDir, cOptions + " -clean", .T. )
                  ENDIF
                  IF AScan( aParams, "install" ) > 0 .AND. ;
                     ! Empty( GetEnv( cInstallDirVar ) ) .AND. ;
                     ( ! ( cType == "implib" ) .OR. GetEnv( "HB_BUILD_IMPLIB" ) == "yes" )
                     call_hbmk2( cBase + cProject, cTargetDir, cOptions + " -instpath=${" + cInstallDirVar + "}", .T. )
                  ELSEIF !( Len( aParams ) == 1 .AND. aParams[ 1 ] == "clean" )
                     call_hbmk2( cBase + cProject, cTargetDir, cOptions + " -inc", .T. )
                  ENDIF
               ENDIF
            ELSE
               /* OutStd( hb_StrFormat( "'%s' project skipped" + hb_osNewLine(), cProject ) ) */
            ENDIF
         ENDIF
      NEXT
   NEXT

   ErrorLevel( 0 )

   RETURN

STATIC FUNCTION call_hbmk2( cProject, cTargetDir, cOptions, lDyn )
   LOCAL nErrorLevel

   LOCAL cDir, cName

   hb_FNameSplit( cProject, @cDir, @cName )

   /* TOFIX: Add -implib option. It collides with static lib so needs to be resolved. */

   nErrorLevel := mk_hb_processRun( GetEnv( "HB_HOST_BIN_DIR" ) + _PS_ + "hbmk2" +;
                                    " -quiet -lang=en -width=1000 -q0" + cOptions +;
                                    " " + cProject +;
                                    iif( lDyn, " -hbdyn -nohblib- " + FN_ExtSet( cProject, ".hbc" ), "" ) +;
                                    " -workdir=" + cTargetDir + "/${hb_work}/" + cName + "${hb_workdynsub}" +;
                                    " -o" + cTargetDir + "/" )
   IF nErrorLevel != 0
      OutStd( hb_StrFormat( "'%s' returned status: %s" + hb_osNewLine(), cProject, hb_ntos( nErrorLevel ) ) )
      RETURN .F.
   ENDIF

   RETURN .T.

STATIC FUNCTION mk_hb_processRun( cCommand )

   OutStd( cCommand + hb_osNewLine() )

   RETURN iif( s_lTest, 0, hb_processRun( cCommand ) )

STATIC FUNCTION FN_ExtSet( cFileName, cExt )
   LOCAL cDir, cName

   hb_FNameSplit( cFileName, @cDir, @cName )

   RETURN hb_FNameMerge( cDir, cName, cExt )
