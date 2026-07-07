require 'formula'

class Harbour < Formula
  url 'https://github.com/harbour/core/releases/download/v3.2.0/harbour-3.2.0.src.tar.gz'
  homepage 'harbour.github.io/'
  # Update sha256 after the release archive is published.

  head 'https://github.com/harbour/core.git'

  # depends_on 'pcre' if ARGV.include? '--with-pcre'

  def install
    ENV[ 'HB_INSTALL_PREFIX' ] = "#{HOMEBREW_PREFIX}"
    system "make", "install"
  end
end
