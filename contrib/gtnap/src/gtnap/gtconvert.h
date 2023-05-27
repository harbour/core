/*
    This is part of gtnap
    TODO: More info
*/


//
// FRAN!!!!!!! Must be deleted this file
//
// Must be used: 'hbapicdp.h'
//



#ifndef HB_GTCONVERT_H_
#define HB_GTCONVERT_H_

#include "core.hxx"

uint8_t gtconvert_UTF8_to_1252_char(const uint32_t cp);

// uint32_t gtconvert_1252_to_UTF8_char(const uint8_t cp);

String *gtconvert_1252_to_UTF8(const char_t *str);

String *gtconvert_UTF8_to_1252(const char_t *str);

#endif

