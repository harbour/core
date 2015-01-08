class Harbour < Formula
  homepage "https://github.com/vszakats/harbour-core/"
  url "https://github.com/vszakats/harbour-core/archive/9b7752a6355d992eb6c30cf0811771ced154922e.tar.gz"
  sha256 "f8f708b93b7583bd8ae819a76dbdcc3a6192973f9aae4a49b612b88dbbd7b256"
  version "3.4.0"

  head "https://github.com/vszakats/harbour-core/core.git"

  depends_on "pcre"
  depends_on :x11 => :recommended

  def install
    ENV["HB_INSTALL_PREFIX"] = prefix
    ENV["HB_WITH_X11"] = "no" if build.without? "x11"

    ENV.deparallelize

    system "make", "install"
  end

  test do
    system "#{bin}/hbtest"

    (testpath/"hello.prg").write <<-EOS.undent
      procedure Main()
         OutStd( "Hello, world!" + hb_eol() )
         OutStd( OS() + hb_eol() )
         OutStd( Version() + hb_eol() )
         return
    EOS

    assert shell_output("#{bin}/hbmk2 hello.prg -run").include?("Hello, world!")
  end
end
