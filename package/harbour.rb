require 'formula'

class Harbour < Formula
  url 'https://downloads.sourceforge.net/harbour-project/source/3.0.0/harbour-3.0.0.tar.bz2'
  homepage 'harbour-project.org/'
  sha1 '66c21d666ac24c45485179eeaa9f90458b552e92'

  head 'https://github.com/harbour/core.git'

  # depends_on 'pcre' if ARGV.include? '--with-pcre'

  def install
    ENV[ 'HB_INSTALL_PREFIX' ] = "#{HOMEBREW_PREFIX}"
    system "make", "install"
  end
end
