CC = cc
TOP ?= $(shell pwd)
#PWD := $(shell pwd)
#PATH := $(PWD)/bin:$PATH
#.SHELLFLAGS := -o pipefail -c
export PKG_CONFIG_PATH := /usr/local/lib/pkgconfig

GUILE_CFLAGS = $(shell pkg-config guile-2.0 --cflags)

EMACSY_LDFLAGS = /Users/shane/School/uvm/CSYS-395-evolutionary-robotics/bullet-2.79/Demos/GuileDemo/noweb-emacsy/libemacsy.a

EMACSY_CFLAGS = -I/Users/shane/School/uvm/CSYS-395-evolutionary-robotics/bullet-2.79/Demos/GuileDemo/noweb-emacsy

FANN_LDFLAGS = -L/usr/local/lib -lm -ldoublefann  

CPPFLAGS = -g $(GUILE_CFLAGS) $(EMACSY_CFLAGS) $(shell pkg-config bullet --cflags)

LDFLAGS = $(shell pkg-config guile-2.0 --libs) $(shell pkg-config libglfw --libs) -lVLCore -lVLGraphics $(EMACSY_LDFLAGS) -lstdc++ $(shell pkg-config bullet --libs) $(shell pkg-config liblo --libs) $(FANN_LDFLAGS)

TARGET = eracs
VERSION = 0.1

LITSRCS = eracs.nw main.nw render.nw physics.nw primitive-procedures.nw vlref-smob.nw scene-smob.nw sim-smob.nw rigid-body-smob.nw osc.nw nn.nw physics-buffer.nw camera.nw boiler-plate.nw

TEXS = eracs.tex main.tex render.tex physics.tex primitive-procedures.tex vlref-smob.tex scene-smob.tex  sim-smob.tex rigid-body-smob.tex osc.tex nn.tex physics-buffer.tex camera.tex boiler-plate.tex

DEFS = eracs.defs main.defs render.defs physics.defs primitive-procedures.defs vlref-smob.defs scene-smob.defs sim-smob.defs rigid-body-smob.defs osc.defs nn.defs physics-buffer.defs camera.defs boiler-plate.defs

SRCS = main.cpp render.cpp physics.cpp primitive-procedures.cpp vlref-smob.cpp scene-smob.cpp sim-smob.cpp rigid-body-smob.cpp osc.c nn.c dummy-opengl-context.cpp physics-buffer.scm camera.scm 

TESTS = 

HDRS = render.h physics.h primitive-procedures.h vlref-smob.hpp scene-smob.h sim-smob.h rigid-body-smob.h osc.h nn.h dummy-opengl-context.hpp

OBJS = main.o render.o physics.o primitive-procedures.o vlref-smob.o scene-smob.o sim-smob.o rigid-body-smob.o osc.o nn.o dummy-opengl-context.o

BIBS = 
 
STYS = 

DIST = Makefile README $(LITSRCS) $(TARGET)doc.tex $(SRCS) $(HDRS) $(BIBS) $(STYS)

GRAPHICS_PATH = 

NOTANGLE_LISP_FLAGS = -filter 'docs2comments -one -scm' -L
NOTANGLE_LISP_FLAGS = -L
#NOTANGLE_C_FLAGS = -L'\#line %L "%F"%N' -filter 'docs2comments -one -c' 
#NOTANGLE_C_FLAGS = -filter 'docs2comments -one -c' 
NOTANGLE_C_FLAGS = -L -markup 'mymarkup'
#NOTANGLE_C_FLAGS = -markup 'mymarkup'
NOTANGLE = $(TOP)/bin/mynotangle $@
.PHONY : all

.SUFFIXES : .nw .c .h .cpp .hpp .x

%.tex: %.nw all.defs
	noweave -n -delay -indexfrom all.defs $< | cpif $@

%.c %.cpp %.h %.hpp: %.nw
	$(NOTANGLE) $(NOTANGLE_C_FLAGS) -R"file:$@" $^
#	notangle $(NOTANGLE_C_FLAGS) -R"file:$@" $^ | cpif $@; if [ $$? -eq 3 ]; then false; else true; fi

#; if [ $${PIPESTATUS[0]} -eq 3 ]; then false; fi

%.scm: %.nw
	$(NOTANGLE) $(NOTANGLE_LISP_FLAGS) -R"file:$@" $^ 

%.dvi: %.tex
	noindex $<
	latex $<

%.cpp.x: %.cpp 
	guile-snarf -o $@ $(GUILE_CFLAGS) $<

%.c.x: %.c 
	guile-snarf -o $@ $(GUILE_CFLAGS) $<

%.pdf: %.tex
	TEXINPUTS=.:$(GRAPHICS_PATH): pdflatex -shell-escape -halt-on-error $<
	noindex $<
	TEXINPUTS=.:$(GRAPHICS_PATH): pdflatex -shell-escape -halt-on-error $<

# Where should all the global defs go?
%.defs: %.nw
	nodefs $< > $@

all: 
	$(MAKE) source
	$(MAKE) $(TARGET)

doc:
	$(MAKE) $(TARGET).pdf

source: $(HDRS) $(SRCS)

all.defs: $(DEFS)
	sort -u $^ | cpif $@

tar: $(TARGET)doc.tex
	mkdir $(TARGET)-$(VERSION)
	cp -R $(DIST) $(TARGET)-$(VERSION)
	tar -zcf $(TARGET)-$(VERSION).tar.gz $(TARGET)-$(VERSION)
	rm -rf $(TARGET)-$(VERSION)

distribution: all tar $(TARGET).pdf 

clean:
	$(RM) $(OBJS) $(SRCS) $(TESTS) $(HDRS) $(TEXS) $(DEFS) all.defs *.log *.dvi *~ *.blg *.lint *.nwi $(TARGET).pdf $(TARGET) *.x

veryclean: clean
	$(RM) *.aux *.bbl *.out

preview: $(TARGET).pdf
	open -a Skim.app $<

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $(TARGET)

$(TARGET).pdf: $(TEXS) 

TAGS: $(SRCS) $(TESTS)
	etags $(SRCS) $(TESTS)

main.cpp: main.nw eracs.nw

dummy-opengl-context.hpp dummy-opengl-context.cpp: render.nw
	$(NOTANGLE) $(NOTANGLE_C_FLAGS) -R"file:$@" $^

physics.o: physics.cpp physics.cpp.x

primitive-procedures.o: primitive-procedures.cpp primitive-procedures.cpp.x

rigid-body-smob.o: rigid-body-smob.cpp rigid-body-smob.cpp.x

scene-smob.o: scene-smob.cpp scene-smob.cpp.x

vlref-smob.o: vlref-smob.cpp vlref-smob.cpp.x

osc.o: osc.c osc.c.x

nn.o: nn.c nn.c.x

sim-smob.o: sim-smob.cpp sim-smob.cpp.x

main.cpp: main.nw camera.nw nn.nw osc.nw physics-buffer.nw physics-ui.nw primitive-procedures.nw rigid-body-smob.nw scene-smob.nw sim-smob.nw vlref-smob.nw boiler-plate.nw

nn.h: nn.nw boiler-plate.nw 
