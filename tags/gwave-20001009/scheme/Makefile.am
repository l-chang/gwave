# Makefile.am: process this file with automake to produce Makefile.in

guiledir=$(datadir)/guile/app/gwave

guile_DATA=system.gwaverc cmds.scm  dynlink.scm globals.scm minimal.scm \
	std-args.scm std-menus.scm std-toolbar.scm \
	extra-menus.scm visiblewave-ops.scm gwave-startup.scm

EXTRA_DIST=$(guile_DATA)

# ggtkhack_DATA gets set by configure to .scm files needed to patch around 
# guile-gtk problems on specific installations.
ggtkhackdir=@ggtk_hack_dir@
ggtkhack_DATA=@ggtkhack_files@
