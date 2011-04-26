/*
 * $Id$
 */

#include "hbapi.h"
#include "hbvm.h"
#include "hbdroid.h"

/* MT support? */
static JNIEnv * s_env;
static jobject  s_obj;

HB_EXPORT JNIEnv * __hbdroid_jni_env( void )
{
   return s_env;
}

HB_EXPORT jobject  __hbdroid_jni_obj( void )
{
   return s_obj;
}

JNIEXPORT void JNICALL Java_test_test_Harbour_vmInit( JNIEnv * env, jobject obj, jint i )
{
   s_env = env;
   s_obj = obj;

   hb_vmInit( i );
}
