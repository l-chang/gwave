
XPM=drag-no.xpm wave-drag-ok.xpm

../pixmaps.c: $(XPM) Makefile
	>$@
	sed -e 's/noname/drag_no_xpm/' -e 's/static//' drag-no.xpm >> $@
	sed -e 's/noname/wave_drag_ok_xpm/' -e 's/static//' wave-drag-ok.xpm >> $@

