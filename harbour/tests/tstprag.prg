/*
 * $Id$
 */

#pragma TracePragmas=On
#pragma ExitSeverity=1

/* Unknow pragmas will be ignored silently */
#pragma BadPragma=off
#pragma /Y+

function Main()

#pragma ShortCutting=On
/* or #pragma /Z+ */

  if .t. .and. .f.
    ? "Always"
  endif

  if .f. .and. .t.
    ? "Never"
  endif

#pragma /Z-
/* or #pragma ShortCutting=Off */

/* Pragmas with bad values will cause an error  */
#pragma WarningLevel=8

return nil
