/*
 * $Id$
 */

/* This file contains all the Run-Time library init symbols */

  Arrays__InitSymbols();
  Classes__InitSymbols();
  Console__InitSymbols();
  Dates__InitSymbols();
  Descend__InitSymbols();
  Dir__InitSymbols();
  Environ__InitSymbols();
  Files__InitSymbols();
  HardCR__InitSymbols();
  Math__InitSymbols();
  Memotran__InitSymbols();
  Set__InitSymbols();
  Strings__InitSymbols(); 
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY  
  Strings__InitInfinity();
#endif  
  Transfrm__InitSymbols();

