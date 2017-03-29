

dnl AC_PROG_GREPSTDOUT(var-to-set, program-to-find, string to grep for,
dnl 	value-if-found, value-if-unfound)
dnl try all programs in $PATH named program-to-find, grepping their
dnl stdout for the indicated string.  First one to pass wins.
dnl
AC_DEFUN([AC_PROG_GREPSTDOUT],
[# Extract the first word of "$2", so it can be a program name with args.
set dummy $2; ac_word=[$]2
AC_MSG_CHECKING([for suitable $ac_word])
AC_CACHE_VAL(ac_cv_prog_$1,
[if test -n "[$]$1"; then
  ac_cv_prog_$1="[$]$1" # Let the user override the test.
else
  IFS="${IFS=   }"; ac_save_ifs="$IFS"; IFS=":"
dnl $ac_dummy forces splitting on constant user-supplied paths.
dnl POSIX.2 word splitting is done only on the output of word expansions,
dnl not every word.  This closes a longstanding sh security hole.
  ac_dummy="$PATH"
  for ac_dir in $ac_dummy; do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/$ac_word; then
      if ($ac_dir/$2 | grep $3 >/dev/null) ; then
	   ac_cv_prog_$1="$4"
	else
	  ac_bogus=$ac_dir/$ac_word
	  ac_prog_rejected=yes
	  continue
      fi
      break
    fi
  done
  IFS="$ac_save_ifs"
if test "$ac_prog_rejected" = yes; then
  # We found a bogon in the path, so make sure we never use it.
  set dummy [$]ac_cv_prog_$1
  shift
  if test [$]# -gt 0; then
    # We chose a different compiler from the bogus one.
    # However, it has the same basename, so the bogon will be chosen
    # first if we set $1 to just the basename; use the full file name.
    shift
    set dummy "$ac_dir/$ac_word" "[$]@"
    shift
    ac_cv_prog_$1="[$]@"
ifelse([$2], [$5], dnl
[  else
    # Default is a loser.
    AC_MSG_ERROR([$1=$ac_bogus unacceptable, but no other $5 found in [\$]PATH])
])dnl
  fi
fi
dnl If no 5th arg is given, leave the cache variable unset,
dnl so AC_PROG_GREPSTDOUT will keep looking.
ifelse([$5], , , [  test -z "[$]ac_cv_prog_$1" && ac_cv_prog_$1="$5"
])dnl
fi])dnl
$1="$ac_cv_prog_$1"
if test -n "[$]$1"; then
  AC_MSG_RESULT([$]$1)
else
  AC_MSG_RESULT(no)
fi
AC_SUBST($1)dnl
])

