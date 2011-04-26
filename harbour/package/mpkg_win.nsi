;
; $Id$
;

; ---------------------------------------------------------------
; Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
; See COPYING for licensing terms.
;
; Harbour Project Nullsoft installer script (for Windows/DOS)
; [ Do not try to use this script directly. It won't work. ]
;
; Please read INSTALL for further information.
; ---------------------------------------------------------------

  SetCompressor /solid lzma

  !include "MUI2.nsh"

  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Header\win.bmp"
  !define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
  !define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\doc\readme.txt"
  !define MUI_ICON "harb_win.ico"
  !define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\orange-uninstall.ico"

SetDateSave on
SetDatablockOptimize on
CRCCheck on

RequestExecutionLevel user

; The name of the installer
Name "Harbour Project"

; The file to write
OutFile "$%HB_TOP%\$%HB_PKGNAME%.exe"

InstallDir C:\$%HB_PKGNAMI%

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

LicenseLangString MUILicense ${LANG_ENGLISH} "$%HB_INSTALL_PREFIX%\COPYING"

;--------------------------------
; The stuff to install

Section "Main components" hb_main

  SectionIn RO

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR

  File /nonfatal "$%HB_INSTALL_PREFIX%\COPYING"
  File /nonfatal "$%HB_INSTALL_PREFIX%\INSTALL"
  File /nonfatal "$%HB_INSTALL_PREFIX%\NEWS"
  File /nonfatal "$%HB_INSTALL_PREFIX%\TODO"
  ; Let it work also for DOS packages where short filename is used here
  File /nonfatal "$%HB_INSTALL_PREFIX%\Change*"

  SetOutPath $INSTDIR\bin
  File "$%HB_INSTALL_PREFIX%\bin\*.*"

  SetOutPath $INSTDIR\lib
  File /r "$%HB_INSTALL_PREFIX%\lib\*.*"

  SetOutPath $INSTDIR\include
  File "$%HB_INSTALL_PREFIX%\include\*.*"

  SetOutPath $INSTDIR\doc
  File "$%HB_INSTALL_PREFIX%\doc\*.*"

  ; Write the installation path into the registry
; WriteRegStr HKLM "SOFTWARE\Harbour Project" "InstallDir" "$INSTDIR"
  WriteRegStr HKCU "Software\Harbour Project" "InstallDir" "$INSTDIR"

  ; Write the uninstall keys for Windows
; WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour Project" "DisplayName" "Harbour Project"
; WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour Project" "UninstallString" '"$INSTDIR\uninstall.exe"'
; WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour Project" "NoModify" 1
; WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour Project" "NoRepair" 1
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
  CreateShortCut  "$SMPROGRAMS\Harbour Project\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut  "$SMPROGRAMS\Harbour Project\Harbour Project (Command line).lnk" "cmd.exe" "/k cd $INSTDIR\bin" "cmd.exe" 0
  CreateShortCut  "$SMPROGRAMS\Harbour Project\Harbour Project.lnk" "$INSTDIR" "" "$INSTDIR" 0
  CreateShortCut  "$SMPROGRAMS\Harbour Project\hbrun.lnk" "$INSTDIR\bin\hbrun.exe" "-v" "$INSTDIR\bin\hbrun.exe" 0
  CreateDirectory "$SMPROGRAMS\Harbour Project\Links"
  WriteINIStr     "$SMPROGRAMS\Harbour Project\Links\Homepage.url"                   "InternetShortcut" "URL" "http://harbour-project.org/"
  WriteINIStr     "$SMPROGRAMS\Harbour Project\Links\Sourceforge Page.url"           "InternetShortcut" "URL" "http://sourceforge.net/projects/harbour-project/"
  WriteINIStr     "$SMPROGRAMS\Harbour Project\Links\Users' Mailing List.url"        "InternetShortcut" "URL" "http://groups.google.com/group/harbour-users/"
  WriteINIStr     "$SMPROGRAMS\Harbour Project\Links\Developers' Mailing List.url"   "InternetShortcut" "URL" "http://groups.google.com/group/harbour-devel/"
  WriteINIStr     "$SMPROGRAMS\Harbour Project\Links\Development Timeline.url"       "InternetShortcut" "URL" "http://sourceforge.net/apps/trac/harbour-project/timeline"

SectionEnd

;--------------------------------
; Descriptions

  ; Language strings
  LangString DESC_hb_main ${LANG_ENGLISH} "Main components"
  LangString DESC_hb_shortcuts ${LANG_ENGLISH} "Add icons to Start Menu and Desktop"
  LangString DESC_hb_examples ${LANG_ENGLISH} "Samples and tests"

  ; Assign language strings to sections
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

  ; Remove directories used
  Delete "$SMPROGRAMS\Harbour Project\Links\*.*"
  RMDir  "$SMPROGRAMS\Harbour Project\Links"
  Delete "$SMPROGRAMS\Harbour Project\*.*"
  RMDir  "$SMPROGRAMS\Harbour Project"
  RMDir  "$INSTDIR"

  Delete "$DESKTOP\Harbour Project.lnk"

; DeleteRegKey HKLM "SOFTWARE\Harbour Project"
  DeleteRegKey HKCU "Software\Harbour Project"

SectionEnd
