/*
 * $Id$
 */

//*******************************************************************
// hparser.ch: Amiket a hparser.prg-nek ‚s a hparser2.prg-nek is 
//             l tnia kell.
// 1999, Csisz r Levente

//*******************************************************************

#define HPRERRGROUP       "hparser"

#define HPRERR_INVALIDMN      {HPRERRGROUP,"invalidmn"}
#define HPRERR_SDEFINE        {HPRERRGROUP,"sdefine"}
#define HPRERR_LDEFINE        {HPRERRGROUP,"ldefine"}
#define HPRERR_PDEFINE        {HPRERRGROUP,"pdefine"}
#define HPRERR_SUNDEF         {HPRERRGROUP,"sundef"}
#define HPRERR_MDUPLICATE     {HPRERRGROUP,"mduplicate"}
#define HPRERR_SXTRANSLATE    {HPRERRGROUP,"sxtranslate"}
#define HPRERR_XTRANSLATEEOL  {HPRERRGROUP,"xtranslateeol"}
#define HPRERR_XTRRNESTED     {HPRERRGROUP,"xtrrnested"}
#define HPRERR_XTRLUNCLOSED   {HPRERRGROUP,"xtrlunclosed"}
#define HPRERR_XTRUNDEFRM     {HPRERRGROUP,"xtrundefrm"}
#define HPRERR_IFDEFNL        {HPRERRGROUP,"ifdefnl"}
#define HPRERR_SIFDEF         {HPRERRGROUP,"sifdef"}
#define HPRERR_ELSE           {HPRERRGROUP,"else"}
#define HPRERR_ELSE2          {HPRERRGROUP,"else2"}
#define HPRERR_ENDIF          {HPRERRGROUP,"endif"}
#define HPRERR_ENDIFMISSING   {HPRERRGROUP,"endifmissing"}
#define HPRERR_INCLUDE        {HPRERRGROUP,"include"}
#define HPRERR_INCLUDEOPEN    {HPRERRGROUP,"includeopen"}
#define HPRERR_INCLUDEFIND    {HPRERRGROUP,"includefind"}
#define HPRERR_INCLUDENEST    {HPRERRGROUP,"includenest"}
#define HPRERR_USER           {HPRERRGROUP,"user"}
// #define HPRERR_ {HPRERRGROUP,""}

//*******************************************************************
// Ilyen ifdef-ek lehets‚gesek.
#define IFDEFTYPE_IFDEF    "ifdef"
#define IFDEFTYPE_IFNDEF   "ifndef"
#define IFDEFTYPE_NONE     "none" 

//*******************************************************************
// Egy #ifdef elemz‚se k”zben melyik  gon (if vagy else) vagyunk.

#define IFB_IFBRANCH         "ifbranch"
#define IFB_ELSEBRANCH       "elsebranch"
#define IFB_NONEBRANCH       "nonebranch"

//*******************************************************************

