STATIC cScrn

Procedure __XSAVESCREEN()
cScrn := {Row(), Col(), SaveScreen()}

procedure __XRESTSCREEN()
IF cScrn <> NIL
   RestScreen( ,,,,cScrn[3] )
   SetPos( cScrn[1], cScrn[2] )
ENDIF
cScrn := NIL
