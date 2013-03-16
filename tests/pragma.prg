//NOTEST

#pragma TracePragmas=On
#pragma ExitSeverity=1

/* Unknown pragmas will be ignored silently */
#pragma BadPragma=off
#pragma /Y+

PROCEDURE Main()

#pragma Shortcut=On

#pragma Shortcut= Off

#pragma Shortcut = On

#pragma Shortcut(OFF)

#pragma Shortcut( On)

#pragma Shortcut( OFF )

#pragma Shortcut( On )

#pragma Shortcut( OFF )

#pragma Shortcut( ON

   /* or #pragma /Z+ */

   IF .T. .AND. .F.
      ? "Always"
   ENDIF

   IF .F. .AND. .T.
      ? "Never"
   ENDIF

#pragma /Z-
/* or #pragma Shortcut=Off */

#pragma ExitSeverity=0
#pragma ExitSeverity=1
#pragma ExitSeverity(0)
#pragma ExitSeverity( 1 )
#pragma ExitSeverity( 0 )
#pragma ExitSeverity= 2
#pragma ExitSeverity= 1

/* Pragmas with bad values will cause an error  */
#pragma WarningLevel=8

   RETURN
