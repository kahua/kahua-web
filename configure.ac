dnl
dnl Configuring kahua-web
dnl  process this file with "autoconf" to generate 'configure'.
dnl $Id: configure.ac,v 1.2 2004/04/02 10:04:58 nobsun Exp $
AC_PREREQ(2.54)
AC_INIT(kahua-web, 0.1, shiro@acm.org)

dnl Check for programs.
AC_PATH_PROGS([GOSH], gosh)
AC_PATH_PROGS([KAHUA_INSTALL], kahua-install)
AC_PATH_PROGS([KAHUA_SERVER], kahua-server)

if test X$GOSH = X; then 
  AC_MSG_ERROR([Cannot find Gauche installed.  Please install Gauche-0.7.4.2 or later, and make sure to set PATH properly.])
fi
if test X$KAHUA_INSTALL = X; then 
  AC_MSG_ERROR([Cannot find Kahua installed.  Please install Kahua-0.2.1 or later, and make sure to set PATH properly.])
fi

dnl Add more test

dnl Output
echo $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile)
