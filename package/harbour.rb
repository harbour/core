class Harbour < Formula
  desc "Portable, xBase-compatible programming language and environment"
  homepage "https://github.com/vszakats/harbour-core/"

  stable do
    url "https://github.com/vszakats/harbour-core/archive/v3.0.0.tar.gz"
    sha256 "34196df52c5f9994b57936fd231f09b7307462a63cfdaa42fe8d3e1a8a388dfd"

    # Fix missing header that was deprecated by libcurl @ version 7.12.0
    # and deleted sometime after Harbour 3.0.0 release.
    patch :DATA
  end

  devel do
    url "https://github.com/vszakats/harbour-core/archive/141a288ab7ece839bda123b7008dcc60140ef9e5.tar.gz"
    sha256 "959469eb09104e4210e3380347d618e7e6d00dbd4427a3156a3e5f4524a3d174"
    version "3.4.0"
  end

  head "https://github.com/vszakats/harbour-core.git"

  # This one is "vendored", but is used if found on the system
  depends_on "pcre"

  depends_on "allegro" => :optional
  depends_on "cairo" => :optional
  depends_on "freeimage" => :optional
  depends_on "gd" => :optional
  depends_on "ghostscript" => :optional
  depends_on "icu4c" => :optional
  depends_on "libmagic" => :optional
  depends_on "mariadb" => :optional
  depends_on :mysql => :optional
  depends_on "ncurses" => :optional
  depends_on "openssl" => :optional
  depends_on :postgresql => :optional
  depends_on "qt5" => :optional
  depends_on "s-lang" => :optional
  depends_on "unixodbc" => :optional
  depends_on :x11 => :optional

  def install
    ENV["HB_INSTALL_PREFIX"] = prefix
    ENV["HB_WITH_X11"] = "no" if build.without? "x11"

    system "make", "install"

    # This is not longer needed in recent builds
    rm Dir[bin/"hbmk2.*.hbl"] if build.stable?
  end

  test do
    (testpath/"hello.prg").write <<-EOS.undent
      procedure Main()
         OutStd( ;
            "Hello, world!" + hb_eol() + ;
            OS() + hb_eol() + ;
            Version() + hb_eol() )
         return
    EOS

    assert_match /Hello, world!/, shell_output("#{bin}/hbmk2 hello.prg -run")
  end
end

__END__
diff --git a/contrib/hbcurl/core.c b/contrib/hbcurl/core.c
index 00caaa8..53618ed 100644
--- a/contrib/hbcurl/core.c
+++ b/contrib/hbcurl/core.c
@@ -53,8 +53,12 @@
  */

 #include <curl/curl.h>
-#include <curl/types.h>
-#include <curl/easy.h>
+#if LIBCURL_VERSION_NUM < 0x070A03
+#  include <curl/easy.h>
+#endif
+#if LIBCURL_VERSION_NUM < 0x070C00
+#  include <curl/types.h>
+#endif

 #include "hbapi.h"
 #include "hbapiitm.h"
