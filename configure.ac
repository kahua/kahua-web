dnl
dnl Configuring kahua-web
dnl  process this file with "autoconf" to generate 'configure'.
dnl $Id: configure.ac,v 1.21 2007/05/22 15:25:36 bizenn Exp $
AC_PREREQ(2.59)
AC_INIT(kahua-web, 0.3_pre7, info@kahua.org)

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
  AC_MSG_ERROR([Cannot find Gauche installed.  Please install Gauche-0.8.10 or later, and make sure to set PATH properly.])
fi
if test X$KAHUA_INSTALL = X; then 
  AC_MSG_ERROR([Cannot find Kahua installed.  Please install Kahua-1.1 or later, and make sure to set PATH properly.])
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
