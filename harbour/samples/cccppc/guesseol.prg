/*
 * $Id$
 */

//*******************************************************************
// guesseol.prg: A kital lt sorv‚gjel t rol sa.
// 1999, Csisz r Levente

*********************************************************************
static gEol
static gEol_preSet

*********************************************************************
function setPGuessEol(eol_preSet)
   if (eol_preSet!=nil)
      gEol_preSet:=eol_preSet
   endif
return nil

*********************************************************************
function setGuessEol(eol)
   if (gEol==nil)
      gEol:=eol
   endif
return nil

*********************************************************************
function guessedEol()
   if (gEol==nil)
      return gEol_preset
   endif
return gEol

*********************************************************************

