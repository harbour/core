Name:                  QT (GUI) [multiplatform, free, open-source]
URL:                   https://qt-project.org/
                       https://download.qt-project.org/official_releases/qt/
Environment variable:  HB_WITH_QT=C:\Qt\include (version 4.5.0 or upper is required)
Install (debian):      sudo apt-get install libqt5-dev
Install (rpm):         qt5-devel
Install (OS X):        brew install qt5


You can override autodetection of QT 'moc' tool by using HB_QTPATH and
optionally HB_QTPOSTFIX envvars. This may only be necessary on some *nix
systems. F.e.:

   HB_QTPATH=/opt/qt5/bin/
   HB_QTPOSTFIX=
