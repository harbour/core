;
; $Id$
;

; ---------------------------------------------------------------
; NSIS installer script for Harbour Project
;
; Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
; See COPYING for licensing terms.
; ---------------------------------------------------------------

; TODO: option: install .dlls to system paths

SetCompressor /solid lzma

  !include "MUI2.nsh"

  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Header\win.bmp"
  !define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
  !define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\doc\readme.txt"
  !define MUI_ICON "..\harb_win.ico"

SetDateSave on
SetDatablockOptimize on
CRCCheck on

RequestExecutionLevel user

!define PKG_NODJGPP

!define /date NOW "%Y%m%d"

!ifdef PKG_FULL
   ; The name of the installer
   Name "Harbour Project 2.1 (full)"
   ; The file to write
   OutFile "harbour-2.1.0-win-full.exe"
!else
   !ifdef PKG_MINI
      Name "Harbour Project 2.1 (minimal)"
      OutFile "harbour-2.1.0-win-mini.exe"
   !else
      Name "Harbour Project 2.1"
      OutFile "harbour-2.1.0-win.exe"
   !endif
!endif

InstallDir C:\hb21

;--------------------------------
; Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
; Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE $(MUILicense)
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH

;--------------------------------
; Languages

  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
; License Language String

LicenseLangString MUILicense ${LANG_ENGLISH} "$%HB_ROOT%hb21\COPYING"

;--------------------------------
; The stuff to install

Section "Main components" hb_main

  SectionIn RO

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR

  File /nonfatal "$%HB_ROOT%hb21\RELNOTES"

  File /nonfatal "$%HB_ROOT%hb21\INSTALL"
  File /nonfatal "$%HB_ROOT%hb21\COPYING"
  File /nonfatal "$%HB_ROOT%hb21\TODO"
  File /nonfatal "$%HB_ROOT%hb21\ChangeLog*"

  SetOutPath $INSTDIR\bin
  File "$%HB_ROOT%hb21\bin\harbour-21.dll"
  File "$%HB_ROOT%hb21\bin\harbourmt-21.dll"
  File "$%HB_ROOT%hb21\bin\harbour.exe"
  File "$%HB_ROOT%hb21\bin\hbi18n.exe"
  File "$%HB_ROOT%hb21\bin\hbmk2.exe"
  File "$%HB_ROOT%hb21\bin\hbmk2.*.hbl"
  File "$%HB_ROOT%hb21\bin\hbpp.exe"
  File "$%HB_ROOT%hb21\bin\hbrun.exe"
  File "$%HB_ROOT%hb21\bin\hbtest.exe"
  File "$%HB_ROOT%hb21\bin\hbformat.exe"
  File "$%HB_ROOT%hb21\bin\hbnetio.exe"
  File /nonfatal "$%HB_ROOT%hb21\bin\hbmk.hbc"
  File "$%HB_ROOT%hb21\bin\upx*.*"

  SetOutPath $INSTDIR\include
  File "$%HB_ROOT%hb21\include\*.*"

  SetOutPath $INSTDIR\doc
  File "$%HB_ROOT%hb21\doc\*.*"

  SetOutPath $INSTDIR\addons
  File "$%HB_ROOT%hb21\addons\HARBOUR_README_ADDONS"

  ; Write the installation path into the registry
; WriteRegStr HKLM "SOFTWARE\Harbour Project 2.1" "InstallDir" "$INSTDIR"
  WriteRegStr HKCU "Software\Harbour Project 2.1" "InstallDir" "$INSTDIR"

  ; Write the uninstall keys for Windows
; WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour Project 2.1" "DisplayName" "Harbour Project"
; WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour Project 2.1" "UninstallString" '"$INSTDIR\uninstall.exe"'
; WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour Project 2.1" "NoModify" 1
; WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour Project 2.1" "NoRepair" 1
  WriteUninstaller "uninstall.exe"

SectionEnd

Section /o "x64 tools" hb_main_x64
  SetOutPath $INSTDIR\bin
  File "$%HB_ROOT%hb21\bin\harbour-x64.exe"
  File "$%HB_ROOT%hb21\bin\hbi18n-x64.exe"
  File "$%HB_ROOT%hb21\bin\hbmk2-x64.exe"
  File "$%HB_ROOT%hb21\bin\hbpp-x64.exe"
  File "$%HB_ROOT%hb21\bin\hbrun-x64.exe"
  File "$%HB_ROOT%hb21\bin\hbtest-x64.exe"
  File "$%HB_ROOT%hb21\bin\hbformat-x64.exe"
  File "$%HB_ROOT%hb21\bin\hbnetio-x64.exe"
SectionEnd

!ifndef PKG_NO_MINGW
Section "MinGW compiler" hb_mingw
  SetOutPath $INSTDIR\comp\mingw
  File /r "$%HB_ROOT%hb21\comp\mingw\*.*"
SectionEnd
!endif

!ifdef PKG_FULL
Section "MinGW x64 compiler" hb_mingw64
  SetOutPath $INSTDIR\comp\mingw64
  File /r "$%HB_ROOT%hb21\comp\mingw64\*.*"
SectionEnd

Section "MinGW WinCE/ARM compiler" hb_mingwarm
  SetOutPath $INSTDIR\comp\mingwarm
  File /r "$%HB_ROOT%hb21\comp\mingwarm\*.*"
SectionEnd
!endif

Section "Libs for MinGW" hb_lib_mingw
  SectionIn RO
  SetOutPath $INSTDIR\lib\win\mingw
  File "$%HB_ROOT%hb21\lib\win\mingw\*.*"
  SetOutPath $INSTDIR\comp\mingw
  File "$%HB_ROOT%hb21\comp\mingw\HARBOUR_README_MINGW"
SectionEnd

Section /o "Libs for MinGW x64" hb_lib_mingw64
  SetOutPath $INSTDIR\lib\win\mingw64
  File "$%HB_ROOT%hb21\lib\win\mingw64\*.*"
  SetOutPath $INSTDIR\comp\mingw64
  File "$%HB_ROOT%hb21\comp\mingw64\HARBOUR_README_MINGW64"
SectionEnd

Section /o "Libs for MinGW WinCE/ARM" hb_lib_mingwarm
  SetOutPath $INSTDIR\lib\wce\mingwarm
  File "$%HB_ROOT%hb21\lib\wce\mingwarm\*.*"
  SetOutPath $INSTDIR\comp\mingwarm
  File "$%HB_ROOT%hb21\comp\mingwarm\HARBOUR_README_MINGWARM"
SectionEnd

Section "Libs for MSVC 2008" hb_lib_msvc
  SetOutPath $INSTDIR\lib\win\msvc
  File "$%HB_ROOT%hb21\lib\win\msvc\*.*"
SectionEnd

Section /o "Libs for MSVC 2008 x64" hb_lib_msvc64
  SetOutPath $INSTDIR\lib\win\msvc64
  File "$%HB_ROOT%hb21\lib\win\msvc64\*.*"
SectionEnd

Section "Libs for Borland C" hb_lib_bcc
  SetOutPath $INSTDIR\bin
  File "$%HB_ROOT%hb21\bin\harbour-21-bcc.dll"
  File "$%HB_ROOT%hb21\bin\harbourmt-21-bcc.dll"
  SetOutPath $INSTDIR\lib\win\bcc
  File "$%HB_ROOT%hb21\lib\win\bcc\*.*"
SectionEnd

Section /o "Libs for Open Watcom" hb_lib_watcom
  SetOutPath $INSTDIR\lib\win\watcom
  File "$%HB_ROOT%hb21\lib\win\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ROOT%hb21\comp\watcom\HARBOUR_README_WATCOM"
SectionEnd

!ifdef PKG_ADDPOCC
Section /o "Libs for Pelles C" hb_lib_pocc
  SetOutPath $INSTDIR\lib\win\pocc
  File "$%HB_ROOT%hb21\lib\win\pocc\*.*"
  SetOutPath $INSTDIR\comp\pocc
  File "$%HB_ROOT%hb21\comp\pocc\HARBOUR_README_POCC"
SectionEnd

Section /o "Libs for Pelles C x64" hb_lib_pocc64
  SetOutPath $INSTDIR\lib\win\pocc64
  File "$%HB_ROOT%hb21\lib\win\pocc64\*.*"
SectionEnd

Section /o "Libs for Pelles C WinCE/ARM" hb_lib_poccarm
  SetOutPath $INSTDIR\lib\wce\poccarm
  File "$%HB_ROOT%hb21\lib\wce\poccarm\*.*"
SectionEnd
!endif

!ifndef PKG_NOLINUX
Section /o "Libs for Open Watcom Linux" hb_lib_linux
  SetOutPath $INSTDIR\lib\linux\watcom
  File "$%HB_ROOT%hb21\lib\linux\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ROOT%hb21\comp\watcom\HARBOUR_README_WATCOM"
SectionEnd
!endif

!ifndef PKG_NOOS2
Section /o "Libs for Open Watcom OS/2" hb_lib_os2
  SetOutPath $INSTDIR\lib\os2\watcom
  File "$%HB_ROOT%hb21\lib\os2\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ROOT%hb21\comp\watcom\HARBOUR_README_WATCOM"
SectionEnd
!endif

!ifndef PKG_NODOS
Section /o "Libs for Open Watcom MS-DOS" hb_lib_dos
  SetOutPath $INSTDIR\lib\dos\watcom
  File "$%HB_ROOT%hb21\lib\dos\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ROOT%hb21\comp\watcom\HARBOUR_README_WATCOM"
SectionEnd
!endif

!ifndef PKG_NODJGPP
Section /o "Libs for DJGPP MS-DOS" hb_lib_djgpp
  SetOutPath $INSTDIR\lib\dos\djgpp
  File "$%HB_ROOT%hb21\lib\dos\djgpp\*.*"
  SetOutPath $INSTDIR\comp\djgpp
  File "$%HB_ROOT%hb21\comp\djgpp\HARBOUR_README_DJGPP"
SectionEnd
!endif

Section /o "Dlls for x64" hb_dlls_x64
  SetOutPath $INSTDIR\bin
  File "$%HB_ROOT%hb21\bin\harbour-21-x64.dll"
  File "$%HB_ROOT%hb21\bin\harbourmt-21-x64.dll"
SectionEnd

Section /o "Dlls for WinCE/ARM" hb_dlls_arm
  SetOutPath $INSTDIR\bin
  File "$%HB_ROOT%hb21\bin\harbour-21-wce-arm.dll"
  File "$%HB_ROOT%hb21\bin\harbourmt-21-wce-arm.dll"
SectionEnd

!ifdef _NEVER_
!ifndef PKG_NOOS2
Section /o "Dlls for OS/2" hb_dlls_os2
  SetOutPath $INSTDIR\bin
  ; TOFIX: .dll name collision with MS-DOS
  File "$%HB_ROOT%hb21\bin\harbour.dll"
  File "$%HB_ROOT%hb21\bin\harbourm.dll"
SectionEnd
!endif

!ifndef PKG_NODOS
Section /o "Dlls for MS-DOS" hb_dlls_dos
  SetOutPath $INSTDIR\bin
  ; TOFIX: .dll name collision with OS/2
  File "$%HB_ROOT%hb21\bin\harbour.dll"
  File "$%HB_ROOT%hb21\bin\harbourm.dll"
SectionEnd
!endif
!endif

; The stuff to install
Section "Examples" hb_examples
  SetOutPath $INSTDIR\examples
  File /r "$%HB_ROOT%hb21\examples\*.*"
  SetOutPath $INSTDIR\tests
  File /r "$%HB_ROOT%hb21\tests\*.*"
  SetOutPath $INSTDIR\contrib
  File /r "$%HB_ROOT%hb21\contrib\*.*"
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu and Desktop icons" hb_shortcuts

  ; this will be the working dir for shortcuts
  SetOutPath $INSTDIR

  CreateShortCut     "$DESKTOP\Harbour Project 2.1.lnk" "$INSTDIR" "" "$INSTDIR" 0
  CreateDirectory "$SMPROGRAMS\Harbour Project 2.1"
  CreateShortCut  "$SMPROGRAMS\Harbour Project 2.1\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut  "$SMPROGRAMS\Harbour Project 2.1\Harbour Project (Command line).lnk" "cmd.exe" "/k cd $INSTDIR\bin" "cmd.exe" 0
  CreateShortCut  "$SMPROGRAMS\Harbour Project 2.1\Harbour Project.lnk" "$INSTDIR" "" "$INSTDIR" 0
  CreateShortCut  "$SMPROGRAMS\Harbour Project 2.1\hbrun.lnk" "$INSTDIR\bin\hbrun.exe" "-v" "$INSTDIR\bin\hbrun.exe" 0
  CreateDirectory "$SMPROGRAMS\Harbour Project 2.1\Links"
  WriteINIStr     "$SMPROGRAMS\Harbour Project 2.1\Links\Homepage.url"                   "InternetShortcut" "URL" "http://harbour-project.org/"
  WriteINIStr     "$SMPROGRAMS\Harbour Project 2.1\Links\Sourceforge Page.url"           "InternetShortcut" "URL" "http://sourceforge.net/projects/harbour-project/"
  WriteINIStr     "$SMPROGRAMS\Harbour Project 2.1\Links\Users' Mailing List.url"        "InternetShortcut" "URL" "http://groups.google.com/group/harbour-users/"
  WriteINIStr     "$SMPROGRAMS\Harbour Project 2.1\Links\Developers' Mailing List.url"   "InternetShortcut" "URL" "http://groups.google.com/group/harbour-devel/"
  WriteINIStr     "$SMPROGRAMS\Harbour Project 2.1\Links\Development Timeline.url"       "InternetShortcut" "URL" "http://sourceforge.net/apps/trac/harbour-project/timeline"

SectionEnd

;--------------------------------
; Descriptions

  ; Language strings
  LangString DESC_hb_main         ${LANG_ENGLISH} "Harbour main components"
  LangString DESC_hb_shortcuts    ${LANG_ENGLISH} "Add icons to Start Menu and Desktop"
  LangString DESC_hb_examples     ${LANG_ENGLISH} "Harbour samples and tests"
  LangString DESC_hb_main_x64     ${LANG_ENGLISH} "Harbour x64 tools"
  LangString DESC_hb_dlls_x64     ${LANG_ENGLISH} "Harbour dlls for x64"
  LangString DESC_hb_dlls_arm     ${LANG_ENGLISH} "Harbour dlls for WinCE/ARM"
!ifdef _NEVER_
!ifndef PKG_NOOS2
  LangString DESC_hb_dlls_os2     ${LANG_ENGLISH} "Harbour dlls for OS/2"
!endif
!ifndef PKG_NODOS
  LangString DESC_hb_dlls_dos     ${LANG_ENGLISH} "Harbour dlls for MS-DOS"
!endif
!endif
  LangString DESC_hb_mingw        ${LANG_ENGLISH} "MinGW compiler"
!ifdef PKG_FULL
  LangString DESC_hb_mingw64      ${LANG_ENGLISH} "MinGW x64 compiler"
  LangString DESC_hb_mingwarm     ${LANG_ENGLISH} "MinGW WinCE/ARM compiler"
!endif
  LangString DESC_hb_lib_mingw    ${LANG_ENGLISH} "Harbour libs for MinGW"
  LangString DESC_hb_lib_mingw64  ${LANG_ENGLISH} "Harbour libs for MinGW x64"
  LangString DESC_hb_lib_mingwarm ${LANG_ENGLISH} "Harbour libs for MinGW WinCE/ARM"
  LangString DESC_hb_lib_msvc     ${LANG_ENGLISH} "Harbour libs for MSVC 2008"
  LangString DESC_hb_lib_msvc64   ${LANG_ENGLISH} "Harbour libs for MSVC 2008 x64"
  LangString DESC_hb_lib_bcc      ${LANG_ENGLISH} "Harbour libs for Borland C"
  LangString DESC_hb_lib_watcom   ${LANG_ENGLISH} "Harbour libs for Open Watcom"
!ifdef PKG_ADDPOCC
  LangString DESC_hb_lib_pocc     ${LANG_ENGLISH} "Harbour libs for Pelles C"
  LangString DESC_hb_lib_pocc64   ${LANG_ENGLISH} "Harbour libs for Pelles C x64"
  LangString DESC_hb_lib_poccarm  ${LANG_ENGLISH} "Harbour libs for Pelles C WinCE/ARM"
!endif
!ifndef PKG_NOLINUX
  LangString DESC_hb_lib_linux    ${LANG_ENGLISH} "Harbour libs for Open Watcom Linux"
!endif
!ifndef PKG_NOOS2
  LangString DESC_hb_lib_os2      ${LANG_ENGLISH} "Harbour libs for Open Watcom OS/2"
!endif
!ifndef PKG_NODOS
  LangString DESC_hb_lib_dos      ${LANG_ENGLISH} "Harbour libs for Open Watcom MS-DOS"
!endif
!ifndef PKG_NODJGPP
  LangString DESC_hb_lib_djgpp    ${LANG_ENGLISH} "Harbour libs for DJGPP MS-DOS"
!endif

  ; Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_main}         $(DESC_hb_main)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_shortcuts}    $(DESC_hb_shortcuts)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_examples}     $(DESC_hb_examples)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_main_x64}     $(DESC_hb_main_x64)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_dlls_x64}     $(DESC_hb_dlls_x64)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_dlls_arm}     $(DESC_hb_dlls_arm)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_mingw}        $(DESC_hb_mingw)
!ifdef PKG_FULL
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_mingw64}      $(DESC_hb_mingw64)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_mingwarm}     $(DESC_hb_mingwarm)
!endif
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_mingw}    $(DESC_hb_lib_mingw)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_mingw64}  $(DESC_hb_lib_mingw64)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_mingwarm} $(DESC_hb_lib_mingwarm)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_msvc}     $(DESC_hb_lib_msvc)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_msvc64}   $(DESC_hb_lib_msvc64)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_bcc}      $(DESC_hb_lib_bcc)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_watcom}   $(DESC_hb_lib_watcom)
!ifdef PKG_ADDPOCC
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_pocc}     $(DESC_hb_lib_pocc)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_pocc64}   $(DESC_hb_lib_pocc64)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_poccarm}  $(DESC_hb_lib_poccarm)
!endif
!ifndef PKG_NOLINUX
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_linux}    $(DESC_hb_lib_linux)
!endif
!ifndef PKG_NOOS2
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_os2}      $(DESC_hb_lib_os2)
!endif
!ifndef PKG_NODOS
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_dos}      $(DESC_hb_lib_dos)
!endif
!ifndef PKG_NODJGPP
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_djgpp}    $(DESC_hb_lib_djgpp)
!endif
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
; Uninstaller

Section "Uninstall"

  ; Remove files and uninstaller
  RMDir /r $INSTDIR

  ; Remove directories used
  Delete "$SMPROGRAMS\Harbour Project 2.1\Links\*.*"
  RMDir  "$SMPROGRAMS\Harbour Project 2.1\Links"
  Delete "$SMPROGRAMS\Harbour Project 2.1\*.*"
  RMDir  "$SMPROGRAMS\Harbour Project 2.1"
  RMDir  "$INSTDIR"

  Delete "$DESKTOP\Harbour Project 2.1.lnk"

; DeleteRegKey HKLM "SOFTWARE\Harbour Project 2.1"
  DeleteRegKey HKCU "Software\Harbour Project 2.1"

SectionEnd
