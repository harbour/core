require "formula"

class Harbour < Formula
  homepage "https://github.com/vszakats/harbour-core/"
  url "https://github.com/vszakats/harbour-core/archive/master.tar.gz"

  depends_on "pcre"
  depends_on :x11 => :recommended

  def install
    ENV["HB_INSTALL_PREFIX"] = prefix
    ENV["HB_WITH_X11"] = "no" if build.without? "x11"

    ENV.deparallelize

    system "make", "install"

    rm Dir[bin/"hbmk2.*.hbl"]
    rm bin/"contrib.hbr"
    rm bin/"harbour.ucf"
  end

  test do
    (testpath/"hello_world.prg").write("procedure Main();?'Hello, world!';?;?OS();?Version();return")
    system "#{bin}/hbmk2", "-run", "hello_world.prg"
  end
end
