CC = cc

CFLAGS = -g $(shell PKG_CONFIG_PATH=/usr/local/lib/pkgconfig pkg-config guile-2.0 --cflags)

TARGET = hello
VERSION = 0.1

LITSRCS = hello.nw

TEXS = hello.tex 

DEFS = hello.defs

SRCS = hello.c 

TESTS = hello-tests.scm

HDRS = hello.h

OBJS = hello.o

BIBS = 

STYS = 

DIST = Makefile README $(LITSRCS) $(TARGET)doc.tex $(SRCS) $(HDRS) $(BIBS) $(STYS)

GRAPHICS_PATH = 

NOTANGLE_LISP_FLAGS = -L'\#line %L "$<"%N' -filter 'docs2comments -one -scm' 

.PHONY : all

.SUFFIXES : .nw .c .h

%.tex: %.nw all.defs
	noweave -n -delay -indexfrom all.defs $< | cpif $@

%.c: %.nw
	notangle -R$@ $< | cpif $@

%.h: %.nw
	notangle -R$@ $< | cpif $@

%.hw: %.w
	cp $< $@

%.dvi: %.tex
	noindex $<
	latex $<

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
	$(RM) $(OBJS) $(SRCS) $(TESTS) $(HDRS) $(TEXS) $(DEFS) all.defs *.log *.dvi *~ *.blg *.lint *.aux *.out *.nwi $(TARGET).pdf $(TARGET)

veryclean: clean
	$(RM) *.aux *.bbl *.out

preview: $(TARGET).pdf
	open -a Skim.app $<

$(TARGET): $(OBJS)
	$(CC) -o $(TARGET) $(OBJS)

$(TARGET).pdf: $(TEXS) 

TAGS: $(SRCS) $(TESTS)
	etags $(SRCS) $(TESTS)

hello: hello.o
