/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbosx"

PROCEDURE Main()

   LOCAL cFile := hb_DirBase() + "test.txt"

   hb_MemoWrit( cFile, "test" )

   ? osx_EraseToTrash( cFile )

   RETURN
