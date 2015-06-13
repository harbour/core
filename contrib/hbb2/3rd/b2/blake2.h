/*
   BLAKE2 reference source code package - optimized C implementations

   Written in 2012 by Samuel Neves <sneves@dei.uc.pt>

   To the extent possible under law, the author(s) have dedicated all copyright
   and related and neighboring rights to this software to the public domain
   worldwide. This software is distributed without any warranty.

   You should have received a copy of the CC0 Public Domain Dedication along with
   this software. If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.
*/
#pragma once
#ifndef __BLAKE2_H__
#define __BLAKE2_H__

#include <stddef.h>
#include <stdint.h>

#if defined(_MSC_VER)
  #define ALIGN(x) __declspec(align(x))
  #if defined(DYNAMIC_LIB)
    #if defined(DYNAMIC_LIB_EXPORT)
      #define EXPORT_DLL __declspec(dllexport)
    #else
      #define EXPORT_DLL __declspec(dllimport)
    #endif
  #else
    #define EXPORT_DLL
  #endif
#else
  #define EXPORT_DLL
   /* clang seems to crash in certain conditions
      due to alignment hints */
  #if defined(__clang__)
    #define ALIGN(x)
  #else
    #define ALIGN(x) __attribute__ ((__aligned__(x)))
  #endif
#endif

#if defined(__cplusplus)
extern "C" {
#endif

  enum blake2s_constant
  {
    BLAKE2S_BLOCKBYTES = 64,
    BLAKE2S_OUTBYTES   = 32,
    BLAKE2S_KEYBYTES   = 32,
    BLAKE2S_SALTBYTES  = 8,
    BLAKE2S_PERSONALBYTES = 8
  };

  enum blake2b_constant
  {
    BLAKE2B_BLOCKBYTES = 128,
    BLAKE2B_OUTBYTES   = 64,
    BLAKE2B_KEYBYTES   = 64,
    BLAKE2B_SALTBYTES  = 16,
    BLAKE2B_PERSONALBYTES = 16
  };

#pragma pack(push, 1)
  typedef struct __blake2s_param
  {
    uint8_t  digest_length; // 1
    uint8_t  key_length;    // 2
    uint8_t  fanout;        // 3
    uint8_t  depth;         // 4
    uint32_t leaf_length;   // 8
    uint8_t  node_offset[6];// 14
    uint8_t  node_depth;    // 15
    uint8_t  inner_length;  // 16
    // uint8_t  reserved[0];
    uint8_t  salt[BLAKE2S_SALTBYTES]; // 24
    uint8_t  personal[BLAKE2S_PERSONALBYTES];  // 32
  } blake2s_param;

  ALIGN( 64 ) typedef struct __blake2s_state
  {
    uint32_t h[8];
    uint32_t t[2];
    uint32_t f[2];
    uint8_t  buf[2 * BLAKE2S_BLOCKBYTES];
    uint32_t buflen;
    uint8_t  outlen;
    uint8_t  last_node;
  } blake2s_state;

  typedef struct __blake2b_param
  {
    uint8_t  digest_length; // 1
    uint8_t  key_length;    // 2
    uint8_t  fanout;        // 3
    uint8_t  depth;         // 4
    uint32_t leaf_length;   // 8
    uint64_t node_offset;   // 16
    uint8_t  node_depth;    // 17
    uint8_t  inner_length;  // 18
    uint8_t  reserved[14];  // 32
    uint8_t  salt[BLAKE2B_SALTBYTES]; // 48
    uint8_t  personal[BLAKE2B_PERSONALBYTES];  // 64
  } blake2b_param;

  ALIGN( 64 ) typedef struct __blake2b_state
  {
    uint64_t h[8];
    uint64_t t[2];
    uint64_t f[2];
    uint8_t  buf[2 * BLAKE2B_BLOCKBYTES];
    uint32_t buflen;
    uint8_t  outlen;
    uint8_t  last_node;
  } blake2b_state;

  ALIGN( 64 ) typedef struct __blake2sp_state
  {
    blake2s_state S[8][1];
    blake2s_state R[1];
    uint8_t  buf[8 * BLAKE2S_BLOCKBYTES];
    uint32_t buflen;
    uint8_t  outlen;
  } blake2sp_state;

  ALIGN( 64 ) typedef struct __blake2bp_state
  {
    blake2b_state S[4][1];
    blake2b_state R[1];
    uint8_t  buf[4 * BLAKE2B_BLOCKBYTES];
    uint32_t buflen;
    uint8_t  outlen;
  } blake2bp_state;
#pragma pack(pop)

  // Streaming API
  EXPORT_DLL int blake2s_init( blake2s_state *S, size_t outlen );
  EXPORT_DLL int blake2s_init_key( blake2s_state *S, size_t outlen, const void *key, size_t keylen );
  EXPORT_DLL int blake2s_init_param( blake2s_state *S, const blake2s_param *P );
  EXPORT_DLL int blake2s_update( blake2s_state *S, const uint8_t *in, size_t inlen );
  EXPORT_DLL int blake2s_final( blake2s_state *S, uint8_t *out, size_t outlen );

  EXPORT_DLL int blake2b_init( blake2b_state *S, size_t outlen );
  EXPORT_DLL int blake2b_init_key( blake2b_state *S, size_t outlen, const void *key, size_t keylen );
  EXPORT_DLL int blake2b_init_param( blake2b_state *S, const blake2b_param *P );
  EXPORT_DLL int blake2b_update( blake2b_state *S, const uint8_t *in, size_t inlen );
  EXPORT_DLL int blake2b_final( blake2b_state *S, uint8_t *out, size_t outlen );

  EXPORT_DLL int blake2sp_init( blake2sp_state *S, size_t outlen );
  EXPORT_DLL int blake2sp_init_key( blake2sp_state *S, size_t outlen, const void *key, size_t keylen );
  EXPORT_DLL int blake2sp_update( blake2sp_state *S, const uint8_t *in, size_t inlen );
  EXPORT_DLL int blake2sp_final( blake2sp_state *S, uint8_t *out, size_t outlen );

  EXPORT_DLL int blake2bp_init( blake2bp_state *S, size_t outlen );
  EXPORT_DLL int blake2bp_init_key( blake2bp_state *S, size_t outlen, const void *key, size_t keylen );
  EXPORT_DLL int blake2bp_update( blake2bp_state *S, const uint8_t *in, size_t inlen );
  EXPORT_DLL int blake2bp_final( blake2bp_state *S, uint8_t *out, size_t outlen );

  // Simple API
  EXPORT_DLL int blake2s( uint8_t *out, const void *in, const void *key, size_t outlen, size_t inlen, size_t keylen );
  EXPORT_DLL int blake2b( uint8_t *out, const void *in, const void *key, size_t outlen, size_t inlen, size_t keylen );

  EXPORT_DLL int blake2sp( uint8_t *out, const void *in, const void *key, size_t outlen, size_t inlen, size_t keylen );
  EXPORT_DLL int blake2bp( uint8_t *out, const void *in, const void *key, size_t outlen, size_t inlen, size_t keylen );

  static inline int blake2( uint8_t *out, const void *in, const void *key, size_t outlen, size_t inlen, size_t keylen )
  {
    return blake2b( out, in, key, outlen, inlen, keylen );
  }

#if defined(__cplusplus)
}
#endif

#endif

