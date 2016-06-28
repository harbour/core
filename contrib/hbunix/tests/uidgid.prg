/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbunix"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL nGID, cUserInfo, cHomeDir, cShell

   ? posix_getgrnam( "wheel" )
   ? posix_getpwnam( hb_UserName(), @nGID, @cUserInfo, @cHomeDir, @cShell )
   ? nGID, cUserInfo, cHomeDir, cShell

   ? posix_getgrnam( "nonexistent_group_" )
   ? posix_getpwnam( "nonexistent_user_", @nGID, @cUserInfo, @cHomeDir, @cShell )
   ? nGID, cUserInfo, cHomeDir, cShell

   RETURN
