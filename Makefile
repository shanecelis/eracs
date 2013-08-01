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

LIB_EMACSY = /Users/shane/School/uvm/CSYS-395-evolutionary-robotics/noweb-eracs/emacsy/src/emacsy/.libs/libemacsy.a

EMACSY_LDFLAGS = $(LIB_EMACSY)

EMACSY_CFLAGS = -Iemacsy/src/emacsy

FANN_LDFLAGS = -L/usr/local/lib -lm -ldoublefann  

CPPFLAGS = -ferror-limit=3 -fmacro-backtrace-limit=1 -g $(GUILE_CFLAGS) $(EMACSY_CFLAGS) $(shell pkg-config bullet --cflags) $(LOG4C_CFLAGS) $(GSL_CFLAGS)

LDFLAGS = $(GUILE_LDFLAGS) $(shell pkg-config libglfw --libs) -lVLCore -lVLGraphics $(EMACSY_LDFLAGS) -lstdc++ $(shell pkg-config bullet --libs) $(shell pkg-config liblo --libs) $(FANN_LDFLAGS) $(LOG4C_LDFLAGS) $(GSL_LDFLAGS)

TARGET = eracs
VERSION = 0.1

LITSRCS = eracs.nw main.nw render.nw physics.nw primitive-procedures.nw vlref-smob.nw scene-smob.nw sim-smob.nw rigid-body-smob.nw osc.nw nn.nw physics-buffer.nw camera.nw boiler-plate.nw physics-ui.nw nsga2.nw linear-spline.nw util.nw util-cpp.nw 

TEXS := $(patsubst %.nw, %.tex, $(LITSRCS))

DEFS := $(patsubst %.nw, %.defs, $(LITSRCS))

SRCS = main.cpp render.cpp physics.cpp primitive-procedures.cpp vlref-smob.cpp scene-smob.cpp sim-smob.cpp rigid-body-smob.cpp nn.c dummy-opengl-context.cpp physics-buffer.scm camera.scm physics-ui.scm nsga2.c nsga2.scm osc.c osc.scm linear-spline.scm  util.c util-cpp.cpp scene-smob.scm util.scm 

TESTS = nsga2.test.scm vlref-smob.test.scm sim-smob.test.scm linear-spline.test.scm 

#TESTS = sim-smob.test.scm

TESTS_OUT := $(patsubst %.test.scm, %.test.out, $(TESTS))

HDRS = render.h physics.h primitive-procedures.h vlref-smob.hpp scene-smob.h sim-smob.h rigid-body-smob.h osc.h nn.h dummy-opengl-context.hpp vl.h  util.h util-cpp.hpp 

OBJS = main.o render.o physics.o primitive-procedures.o vlref-smob.o scene-smob.o sim-smob.o rigid-body-smob.o nn.o dummy-opengl-context.o  util.o util-cpp.o


BIBS = 
 
STYS = 

LIBS = libguile-nsga2.dylib libguile-osc.dylib

DIST = Makefile README $(LITSRCS) $(TARGET)doc.tex $(SRCS) $(HDRS) $(BIBS) $(STYS)

GRAPHICS_PATH = 
#docs2comment does NOT WORK with -L
#NOTANGLE_LISP_FLAGS = -filter 'docs2comments -one -scm' -L
NOTANGLE_LISP_FLAGS = -L
#NOTANGLE_C_FLAGS = -L'\#line %L "%F"%N' -filter 'docs2comments -one -c' 
#NOTANGLE_C_FLAGS = -filter 'docs2comments -one -c' 
NOTANGLE_C_FLAGS = -L -markup 'mymarkup'
#NOTANGLE_C_FLAGS = -markup 'mymarkup'
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
	make -C ctrnn
	make -C guile-mathematica
	$(MAKE) source
	$(MAKE) $(LIBS)
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

eracs: $(OBJS) $(LIB_EMACSY)
	$(CC) $(LDFLAGS) $(OBJS) -o $(TARGET)

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

rigid-body-smob.o: rigid-body-smob.cpp rigid-body-smob.cpp.x

scene-smob.o: scene-smob.cpp scene-smob.cpp.x

vlref-smob.o: vlref-smob.cpp vlref-smob.cpp.x

osc.o: osc.c osc.c.x

nn.o: nn.c nn.c.x

sim-smob.o: sim-smob.cpp sim-smob.cpp.x

render.o: render.cpp render.cpp.x

main.o: main.cpp main.cpp.x

# Must be careful here. This ends up inadvertently controlling the order in
# which global chunks are concatenated.
main.cpp: main.nw render.nw nn.nw osc.nw primitive-procedures.nw rigid-body-smob.nw scene-smob.nw physics-buffer.nw sim-smob.nw vlref-smob.nw physics.nw camera.nw physics-ui.nw 

# nn.h: nn.nw boiler-plate.nw 

# camera.scm: camera.nw boiler-plate.nw 

nsga2.c: nsga2.nw

nsga2.scm: nsga2.nw

NSGA2_HOME = nsga2-gnuplot-v1.1.6

libguile-nsga2.dylib: nsga2.c $(NSGA2_HOME)/allocate.c $(NSGA2_HOME)/auxiliary.c $(NSGA2_HOME)/crossover.c $(NSGA2_HOME)/crowddist.c $(NSGA2_HOME)/decode.c $(NSGA2_HOME)/display.c $(NSGA2_HOME)/dominance.c $(NSGA2_HOME)/eval.c $(NSGA2_HOME)/fillnds.c $(NSGA2_HOME)/initialize.c $(NSGA2_HOME)/list.c $(NSGA2_HOME)/merge.c $(NSGA2_HOME)/mutation.c $(NSGA2_HOME)/nsga2r.c $(NSGA2_HOME)/rand.c $(NSGA2_HOME)/rank.c $(NSGA2_HOME)/report.c $(NSGA2_HOME)/sort.c $(NSGA2_HOME)/tourselect.c util.c
	$(CC) -g -I $(NSGA2_HOME) $(GUILE_CFLAGS) $(LOG4C_CFLAGS) $(GUILE_LDFLAGS) $(LOG4C_LDFLAGS) $(EMACSY_LDFLAGS) -shared -o $@ -fPIC $^

osc.o: osc.c.x

libguile-osc.dylib: osc.o
	$(CC) -g $(GUILE_CFLAGS) $(GUILE_LDFLAGS) $(LDFLAGS) -shared -o $@ -fPIC $^

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

osc.paper.pdf: osc.tex

debug: eracs
	gdb --args ./eracs
