/*
 * $Id$
 */

#include "hbapi.h"
#include "hbdroid.h"

HB_FUNC( JNI_MSGINFO )
{
   JNIEnv * jni_env = __hbdroid_jni_env();
   jobject  jni_obj = __hbdroid_jni_obj();

   jclass cls = ( * jni_env )->GetObjectClass( jni_env, jni_obj );
   jmethodID mid = ( * jni_env )->GetMethodID( jni_env, cls, "MsgInfo", "(Ljava/lang/String;)V" );

   if( mid )
      ( * jni_env )->CallVoidMethod( jni_env, jni_obj, mid, ( * jni_env )->NewStringUTF( jni_env, hb_parcx( 1 ) ) );
}
