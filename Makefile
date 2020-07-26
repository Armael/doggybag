all:
	dune build

clean:
	dune clean

APPIMAGETOOL:=$(shell which appimagetool)
PATCHELF:=$(shell which patchelf)

pkg/doggybag: all pkg/appdir/AppRun pkg/appdir/doggybag.desktop
	dune exec bin/doggybag.exe -- _build/default/bin/doggybag.exe pkg/appdir/usr/bin/doggybag
	cp $(APPIMAGETOOL) pkg/appdir/usr/bin/appimagetool
	cp $(PATCHELF) pkg/appdir/usr/bin/patchelf
	appimagetool pkg/appdir pkg/doggybag

.PHONY: all clean
