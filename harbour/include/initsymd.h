/*
 * $Id$
 */

#ifndef HB_INITSYMD_H_
#define HB_INITSYMD_H_

/* This file contains all the Run-Time library init symbols */

extern void Arrays__InitSymbols( void );
extern void Classes__InitSymbols( void );
extern void Console__InitSymbols( void );
extern void CopyFile__InitSymbols( void );
extern void Dates__InitSymbols( void );
extern void Dates2__InitSymbols( void );
extern void Descend__InitSymbols( void );
extern void Dir__InitSymbols( void );
extern void Environ__InitSymbols( void );
extern void Files__InitSymbols( void );
extern void HardCR__InitSymbols( void );
extern void Math__InitSymbols( void );
extern void Memotran__InitSymbols( void );
extern void Set__InitSymbols( void );
extern void Strings__InitSymbols( void );
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY  
extern void Strings__InitInfinity( void );
#endif
extern void Transfrm__InitSymbols( void );

#endif /* HB_INITSYMD_H_ */
