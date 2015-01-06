class Harbour < Formula
  homepage "https://github.com/vszakats/harbour-core/"
  head "https://github.com/vszakats/harbour-core/core.git"
  version "3.4.0"

  depends_on "pcre"
  depends_on :x11 => :recommended

  def install
    ENV["HB_INSTALL_PREFIX"] = prefix
    ENV["HB_WITH_X11"] = "no" if build.without? "x11"

    ENV.deparallelize

    system "make", "install"
  end

  test do
    (testpath/"hello.prg").write("procedure Main();? 'Hello, world!';? OS();? Version();return")
    system "#{bin}/hbmk2", "hello.prg", "-run"
  end
end
