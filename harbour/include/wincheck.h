/* The purpose for this Harbour include file is to determine if
   windows.h should be included and to set set additional manifest
   constants should this be the case. When used, this include file
   must be the very first include file that is included! */

#if defined(_Windows) || defined(WINNT)
   #define HARBOUR_USE_WIN
   #define WIN32_LEAN_AND_MEAN
   #include <windows.h>
   #if defined(__GNUC__)
      #define HB_DONT_DEFINE_BASIC_TYPES
   #endif
#endif
