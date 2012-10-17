PREFIX ?= /usr/local
LIBDIR = $(PREFIX)/lib
INCLUDEDIR = $(PREFIX)/share/ada/adainclude
ALIDIR = $(PREFIX)/lib/ada/adalib/meshaware

INSTALL = /usr/bin/install

SOVERSION := 1
SONAME := libmeshaware.so.$(SOVERSION)

.PHONY: doc shared static install all

all: shared doc static

obj-shared/libmeshaware.so:
	gnatmake -p -Pbuild_libmeshaware.gpr \
	-XLIBRARY_KIND=dynamic -XOBJ_DIR=$(dir $@@) -XSONAME=$(SONAME)

obj-static/libmeshaware.a:
	gnatmake -p -Pbuild_libmeshaware.gpr \
	-XLIBRARY_KIND=static -XOBJ_DIR=$(dir $@@)

shared:	obj-shared/libmeshaware.so

static:	obj-static/libmeshaware.a

doc: shared static
	if [ ! -d doc/html/ ]; then mkdir -p doc/html; fi
	cd src && gnathtml -f  -p../build_libmeshaware.gpr -o../doc/html -I../lib -ext html -T../gnathtml.list

install: shared static
	if [ ! -d $(ALIDIR) ]; then mkdir -p $(ALIDIR); fi
	if [ ! -d $(LIBDIR) ]; then mkdir -p $(LIBDIR); fi
	if [ ! -d $(INCLUDEDIR)/meshaware ]; then mkdir -p $(INCLUDEDIR)/meshaware; fi
	$(INSTALL) --mode 0444 lib/*.ali $(ALIDIR)
	$(INSTALL)             lib/libmeshaware.a $(LIBDIR)
	$(INSTALL)             lib/libmeshaware.so.$(SOVERSION) $(LIBDIR)
	$(INSTALL)             src/*.ad* $(INCLUDEDIR)/meshaware
	$(INSTALL)             meshaware.gpr $(INCLUDEDIR)
	cd $(LIBDIR) && ln -sf libmeshaware.so.$(SOVERSION) libmeshaware.so

uninstall:
	rm -r $(ALIDIR)
	rm    $(LIBDIR)/libmeshaware*
	rm -r $(INCLUDEDIR)/meshaware/
	rm    $(INCLUDEDIR)/meshaware.gpr

clean:
	if [ -d lib/ ]; then rm -r lib/; fi
	if [ -d obj-shared/ ]; then rm -r obj-shared/; fi
	if [ -d obj-static/ ]; then rm -r obj-static/; fi
	if [ -d doc/html/ ]; then rm -r doc/html/; fi
