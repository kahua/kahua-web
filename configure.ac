dnl
dnl Configuring kahua-web
dnl  process this file with "autoconf" to generate 'configure'.
dnl $Id: configure.ac,v 1.33 2007/07/04 03:54:26 bizenn Exp $
AC_PREREQ(2.59)
AC_INIT(kahua-web, 0.4_pre0, info@kahua.org)

AC_SUBST(PACKAGE_NAME)
AC_SUBST(PACKAGE_VERSION)

dnl Check for programs.  Set up PATH according to prefix.
if test "$prefix" != NONE; then
  tpath=$prefix/bin:$prefix/bin/kahua:$PATH
else
  tpath=$PATH
fi

AC_PATH_PROGS([GOSH], gosh)
AC_PATH_PROGS([KAHUA_INSTALL], kahua-install, [], $tpath)

dnl Check for other program
AC_PROG_INSTALL

if test X$GOSH = X; then 
  AC_MSG_ERROR([Cannot find Gauche installed.  Please install Gauche-0.8.9 or later, and make sure to set PATH properly.])
fi
if test X$KAHUA_INSTALL = X; then 
  AC_MSG_ERROR([Cannot find Kahua installed.  Please install Kahua-1.0.4 or later, and make sure to set PATH properly.])
fi

KAHUACONFIG_OPTION=' -c $(sysconfdir)/kahua.conf'
AC_ARG_WITH(site-bundle,
  AC_HELP_STRING([--with-site-bundle=/path/to/bundle],
                 [Target site bundle path. Default: none]),
  [
    case "$with_site_bundle" in
      no) KAHUACONFIG_OPTION=' -c $(sysconfdir)/kahua.conf' ;;
      *)  KAHUACONFIG_OPTION=" -S $with_site_bundle" ;;
    esac
  ])
AC_SUBST(KAHUACONFIG_OPTION)

dnl Add more test

dnl Output
echo $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile)
