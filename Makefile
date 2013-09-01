CC = cc
TOP ?= $(shell pwd)
#PWD := $(shell pwd)
#PATH := $(PWD)/bin:$PATH
#.SHELLFLAGS := -o pipefail -c
export PKG_CONFIG_PATH := /usr/local/lib/pkgconfig

GUILE_CFLAGS = $(shell pkg-config guile-2.0 --cflags)
GUILE_LDFLAGS = $(shell pkg-config guile-2.0 --libs)

GSL_CFLAGS = $(shell pkg-config gsl --cflags)
GSL_LDFLAGS = $(shell pkg-config gsl --libs)

LOG4C_CFLAGS = $(shell pkg-config guile-logging --cflags)
LOG4C_LDFLAGS = $(shell pkg-config guile-logging --libs)

OSC_CFLAGS = $(shell pkg-config guile-osc --cflags)
OSC_LDFLAGS = $(shell pkg-config guile-osc --libs)

MC_CFLAGS = $(shell  pkg-config guile-minimal-cognition --cflags)
MC_LDFLAGS = $(shell pkg-config guile-minimal-cognition --libs)

LIB_EMACSY = /Users/shane/School/uvm/CSYS-395-evolutionary-robotics/noweb-eracs/emacsy/src/emacsy/.libs/libemacsy.a

EMACSY_LDFLAGS = $(LIB_EMACSY)

EMACSY_CFLAGS = -Iemacsy/src/emacsy

FANN_LDFLAGS = -L/usr/local/lib -lm -ldoublefann  

CPPFLAGS = -ferror-limit=3 -fmacro-backtrace-limit=1 -g $(GUILE_CFLAGS) $(EMACSY_CFLAGS) $(shell pkg-config bullet guile-bullet --cflags) $(GSL_CFLAGS) $(LOG4C_CFLAGS) $(OSC_CFLAGS)

LDFLAGS = $(GUILE_LDFLAGS) $(shell pkg-config libglfw --libs) -lVLCore -lVLGraphics $(EMACSY_LDFLAGS) -lstdc++ $(shell pkg-config bullet guile-bullet --libs) $(shell pkg-config liblo --libs) $(FANN_LDFLAGS) $(LOG4C_LDFLAGS) $(GSL_LDFLAGS) $(OSC_LDFLAGS)

TARGET = eracs
VERSION = 0.1

LITSRCS = eracs.nw main.nw render.nw physics.nw primitive-procedures.nw vlref-smob.nw scene-smob.nw   nn.nw physics-buffer.nw camera.nw boiler-plate.nw physics-ui.nw linear-spline.nw util.nw util-cpp.nw 

TEXS := $(patsubst %.nw, %.tex, $(LITSRCS))

DEFS := $(patsubst %.nw, %.defs, $(LITSRCS))

SRCS = main.cpp render.cpp physics.cpp primitive-procedures.cpp vlref-smob.cpp scene-smob.cpp   nn.c dummy-opengl-context.cpp physics-buffer.scm camera.scm physics-ui.scm  linear-spline.scm  util.c util-cpp.cpp scene-smob.scm util.scm 

TESTS =  vlref-smob.test.scm linear-spline.test.scm 

TESTS_OUT := $(patsubst %.test.scm, %.test.out, $(TESTS))

HDRS = render.h physics.h primitive-procedures.h vlref-smob.hpp scene-smob.h   nn.h dummy-opengl-context.hpp vl.h  util.h util-cpp.hpp 

OBJS = main.o render.o physics.o primitive-procedures.o vlref-smob.o scene-smob.o  nn.o dummy-opengl-context.o  util.o util-cpp.o


BIBS = 
 
STYS = 

LIBS = 

DIST = Makefile README $(LITSRCS) $(TARGET)doc.tex $(SRCS) $(HDRS) $(BIBS) $(STYS)

GRAPHICS_PATH = 
#docs2comment does NOT WORK with -L
#NOTANGLE_LISP_FLAGS = -filter 'docs2comments -one -scm' -L
#NOTANGLE_LISP_FLAGS = -L
NOTANGLE_LISP_FLAGS = 
#NOTANGLE_C_FLAGS = -L'\#line %L "%F"%N' -filter 'docs2comments -one -c' 
#NOTANGLE_C_FLAGS = -filter 'docs2comments -one -c' 
#NOTANGLE_C_FLAGS = -L -markup 'mymarkup'
NOTANGLE_C_FLAGS = -markup 'mymarkup'
NOTANGLE = $(TOP)/bin/mynotangle $@
.PHONY : all

.SUFFIXES : .nw .c .h .cpp .hpp .x .scm .test.scm .test.out

%.tex: %.nw all.defs
	noweave -n -delay -indexfrom all.defs $< | cpif $@

%.paper.nw: %.nw paper-wrapper.nw
	cat paper-wrapper.nw > $@
	echo "\\input{$$(basename -s .nw $<)} \\end{document}" >> $@

%.c %.cpp %.h %.hpp: %.nw boiler-plate.nw
	$(NOTANGLE) $(NOTANGLE_C_FLAGS) -R"file:$@" $^ 

%.scm: %.nw boiler-plate.nw
	$(NOTANGLE) $(NOTANGLE_LISP_FLAGS) -R"file:$@" $^ 

%.test.scm: %.nw boiler-plate.nw
	$(NOTANGLE) $(NOTANGLE_LISP_FLAGS) -R"file:$@" $^ 

%.test.out: %.test.scm %.scm
	./eracs -l $< | tee $@; if [[ "${PIPESTATUS[0]}" -ne 0 ]]; then exit 1; else rm $@; fi

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
	make -C emacsy
	$(MAKE) source
	$(MAKE) $(TARGET)

doc:
	$(MAKE) $(TARGET).pdf

source: $(HDRS) $(SRCS) $(TESTS)

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

eracs: $(OBJS) $(LIB_EMACSY) Makefile
	$(CC) $(LDFLAGS) $(OBJS) -o $(TARGET)
	install_name_tool -change libVLCore.2011.9.dylib /usr/local/lib/libVLCore.2011.9.dylib eracs
	install_name_tool -change libVLGraphics.2011.9.dylib /usr/local/lib/libVLGraphics.2011.9.dylib eracs
	install_name_tool -change libdoublefann.2.dylib /usr/local/lib/libdoublefann.2.dylib eracs


$(TARGET).pdf: $(TEXS) 

TAGS: $(SRCS) $(TESTS)
	etags $(SRCS) $(TESTS)

main.cpp: main.nw eracs.nw

dummy-opengl-context.hpp dummy-opengl-context.cpp: render.nw boiler-plate.nw
	$(NOTANGLE) $(NOTANGLE_C_FLAGS) -R"file:$@" $^ 

vl.h: main.nw boiler-plate.nw
	$(NOTANGLE) $(NOTANGLE_C_FLAGS) -R"file:$@" $^ 

# scm-logging.h scm-logging.c: logging.nw boiler-plate.nw
# 	$(NOTANGLE) $(NOTANGLE_C_FLAGS) -R"file:$@" $^ 

# util.h: rigid-body-smob.nw boiler-plate.nw
# 	$(NOTANGLE) $(NOTANGLE_C_FLAGS) -R"file:$@" $^ 


physics.o: physics.cpp physics.cpp.x

primitive-procedures.o: primitive-procedures.cpp primitive-procedures.cpp.x

scene-smob.o: scene-smob.cpp scene-smob.cpp.x

vlref-smob.o: vlref-smob.cpp vlref-smob.cpp.x

nn.o: nn.c nn.c.x

render.o: render.cpp render.cpp.x

main.o: main.cpp main.cpp.x

# Must be careful here. This ends up inadvertently controlling the order in
# which global chunks are concatenated.
main.cpp: main.nw render.nw nn.nw primitive-procedures.nw  scene-smob.nw physics-buffer.nw vlref-smob.nw physics.nw camera.nw physics-ui.nw 

# nn.h: nn.nw boiler-plate.nw 

# camera.scm: camera.nw boiler-plate.nw 

test: eracs $(SRCS) $(TESTS) $(LIBS)
	for test in $(TESTS); do \
	 ./eracs -l $$test || exit 1; \
	done

check: eracs $(SRCS) $(TESTS) $(LIBS) $(TESTS_OUT)

linear-spline.scm: linear-spline.nw

# To create a small PDF document for X.nw, add the following rule:
#
# X.paper.pdf: X.tex
# 
# Then one can create the X.paper.pdf.  See paper-wrapper.nw for LaTeX
# environment that it creates.
linear-spline.paper.pdf: linear-spline.tex

debug: eracs
	gdb --args ./eracs
