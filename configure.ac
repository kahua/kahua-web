dnl
dnl Configuring kahua-web
dnl  process this file with "autoconf" to generate 'configure'.
dnl $Id: configure.ac,v 1.4 2004/09/13 12:44:23 nobsun Exp $
AC_PREREQ(2.54)
AC_INIT(kahua-web, 0.2, shiro@acm.org)

dnl Check for programs.
AC_PATH_PROGS([GOSH], gosh)
AC_PATH_PROGS([KAHUA_INSTALL], kahua-install)
AC_PATH_PROGS([KAHUA_SERVER], kahua-server)

dnl Check for other program
AC_PROG_INSTALL

if test X$GOSH = X; then 
  AC_MSG_ERROR([Cannot find Gauche installed.  Please install Gauche-0.8.1 or later, and make sure to set PATH properly.])
fi
if test X$KAHUA_INSTALL = X; then 
  AC_MSG_ERROR([Cannot find Kahua installed.  Please install Kahua-0.2.8 or later, and make sure to set PATH properly.])
fi

dnl Add more test

dnl Output
echo $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile)
