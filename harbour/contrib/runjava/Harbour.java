/*
 * $Id$
 */

/**
 * Harbour JAVA interface
 * @author   Matteo Baccan
 * @version  1.0
 */

public class Harbour {

   public static native long Run( int[] pCode );

   // DLL to load
   static { System.loadLibrary("hbrunjav"); }
}
