#
# $Id$
#

require 'formula'

class Harbour < Formula
  url 'http://downloads.sourceforge.net/project/harbour-project/source/3.0.0/harbour-3.0.0.tar.bz2'
  homepage 'harbour-project.sourceforge.net/'
  sha1 '66c21d666ac24c45485179eeaa9f90458b552e92'

  head 'https://harbour-project.svn.sourceforge.net/svnroot/harbour-project/trunk/harbour'

  # depends_on 'pcre' if ARGV.include? '--with-pcre'

  def install
    ENV[ 'HB_INSTALL_PREFIX' ] = "#{HOMEBREW_PREFIX}"
    system "make", "install"
  end
end
