/*
 * $Id$
 */

#ifndef HB_INITSYMC_H_
#define HB_INITSYMC_H_

/* This file contains all the Run-Time library init symbols */

  Arrays__InitSymbols();
  Classes__InitSymbols();
  Console__InitSymbols();
  CopyFile__InitSymbols();
  Dates__InitSymbols();
  Dates2__InitSymbols();
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

#endif /* HB_INITSYMC_H_ */
