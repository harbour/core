;
; $Id$
;

; ---------------------------------------------------------------
; NSIS installer script for Harbour Project
;
; Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
; See doc/license.txt for licensing terms.
; ---------------------------------------------------------------

; TODO: tests/examples
; TODO: option: install .dlls to system paths

  SetCompressor /solid lzma

  !include "MUI.nsh"

  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Header\win.bmp"
  !define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
  !define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\doc\readme.txt"

SetDateSave on
SetDatablockOptimize on
CRCCheck on

; The name of the installer
Name "Harbour Project"

; The file to write
OutFile "harbour-$%HB_VERSION%-$%HB_ARCHITECTURE%-$%HB_COMPILER%.exe"

InstallDir C:\harbour-$%HB_COMPILER%

;--------------------------------
;Interface Settings

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
;Languages

  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;License Language String

LicenseLangString MUILicense ${LANG_ENGLISH} "$%HB_DOC_INSTALL%\license.txt"

;--------------------------------
; The stuff to install

Section "Main components" hb_main

  SectionIn RO

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  File /oname=COPYING "$%HB_DOC_INSTALL%\license.txt"

  SetOutPath $INSTDIR\bin
  File /x *.tds "$%HB_BIN_INSTALL%\*.*"

  SetOutPath $INSTDIR\lib
  File "$%HB_LIB_INSTALL%\*.*"

  SetOutPath $INSTDIR\include
  File "$%HB_INC_INSTALL%\*.*"

  SetOutPath $INSTDIR\doc
  File /r "$%HB_DOC_INSTALL%\*.*"

  ; Write the installation path into the registry
; WriteRegStr HKLM "Software\Harbour" "Install_Dir" "$INSTDIR"

  ; Write the uninstall keys for Windows
; WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Harbour" "DisplayName" "Harbour Project"
; WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Harbour" "UninstallString" '"$INSTDIR\uninstall.exe"'
; WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Harbour" "NoModify" 1
; WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Harbour" "NoRepair" 1
  WriteUninstaller "uninstall.exe"

SectionEnd

; The stuff to install
Section "Examples" hb_examples

  SectionIn RO

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR\tests

SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu and Desktop icons" hb_shortcuts

  ; this will be the working dir for shortcuts
  SetOutPath $INSTDIR

  CreateShortCut "$DESKTOP\Harbour Project.lnk" "$INSTDIR" "" "$INSTDIR" 0
  CreateDirectory "$SMPROGRAMS\Harbour Project"
  CreateShortCut "$SMPROGRAMS\Harbour Project\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\Harbour Project\Harbour Project (Command line).lnk" "cmd.exe" "/k cd $INSTDIR\bin" "cmd.exe" 0
  CreateShortCut "$SMPROGRAMS\Harbour Project\Harbour Project.lnk" "$INSTDIR" "" "$INSTDIR" 0
  CreateShortCut "$SMPROGRAMS\Harbour Project\hbrun.lnk" "$INSTDIR\bin\hbrun.exe" "-v" "$INSTDIR\bin\hbrun.exe" 0
  CreateDirectory "$SMPROGRAMS\Harbour Project\Links"
  WriteINIStr "$SMPROGRAMS\Harbour Project\Links\Home.url"                       "InternetShortcut" "URL" "http://www.harbour-project.org/"
  WriteINIStr "$SMPROGRAMS\Harbour Project\Links\Mailing lists.url"              "InternetShortcut" "URL" "http://www.harbour-project.org/mailing.html"
  WriteINIStr "$SMPROGRAMS\Harbour Project\Links\Developers' mail archives.url"  "InternetShortcut" "URL" "http://lists.harbour-project.org/pipermail/harbour/"
; WriteINIStr "$SMPROGRAMS\Harbour Project\Links\Users' mail archives.url"       "InternetShortcut" "URL" "http://lists.harbour-project.org/pipermail/harbour-users/"
  WriteINIStr "$SMPROGRAMS\Harbour Project\Links\Sourceforge page.url"           "InternetShortcut" "URL" "https://sourceforge.net/projects/harbour-project/"
  WriteINIStr "$SMPROGRAMS\Harbour Project\Links\Development timeline.url"       "InternetShortcut" "URL" "https://apps.sourceforge.net/trac/harbour-project/timeline"

SectionEnd

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_hb_main ${LANG_ENGLISH} "Main components."
  LangString DESC_hb_shortcuts ${LANG_ENGLISH} "Add icons to Start Menu and Desktop."
  LangString DESC_hb_examples ${LANG_ENGLISH} "Samples and tests"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_main} $(DESC_hb_main)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_shortcuts} $(DESC_hb_shortcuts)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_examples} $(DESC_hb_examples)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
; Uninstaller

Section "Uninstall"

  ; Remove files and uninstaller
  RMDir /r $INSTDIR

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\Harbour Project\*.*"

  ; Remove directories used
  RMDir "$SMPROGRAMS\Harbour Project\Links"
  RMDir "$SMPROGRAMS\Harbour Project"
  RMDir "$INSTDIR"

  Delete "$DESKTOP\Harbour Project.lnk"

SectionEnd
