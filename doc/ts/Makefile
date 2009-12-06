MAKEINFO=/usr/bin/makeinfo
TEXI2DVI=/usr/bin/texi2dvi
TEXI2PDF=/usr/bin/texi2pdf
DVIPS=/usr/bin/dvips

FILES=$(wildcard *.texi)
MASTERFILE=ts-mode.texi

.PHONY : all clean veryclean

all : info htmlsplit htmlnosplit pdf dvi ps

info: ts-mode.info

ts-mode.info: $(FILES)
	@$(MAKEINFO) $(MASTERFILE)

htmlsplit : ts-mode/index.html

htmlnosplit : ts-mode.html

pdf : ts-mode.pdf

ts-mode.pdf : $(FILES)
	@$(TEXI2PDF) -E $(MASTERFILE)

ps : ts-mode.ps

ts-mode.ps : ts-mode.dvi
	@$(DVIPS) $<

dvi : ts-mode.dvi

ts-mode.dvi : $(FILES)
	@$(TEXI2DVI) -E $(MASTERFILE)

ts-mode/index.html : $(FILES)
	@$(MAKEINFO) --html $(MASTERFILE)

ts-mode.html : $(FILES)
	@$(MAKEINFO) --no-split --html $(MASTERFILE)

clean :
	@rm -f *.log *.aux *.toc *.dvi *.cp *.fn *.fns \
	       *.ky *.kys *.vr *.vrs *.pg *.tp

veryclean : clean
	@rm -rf *.info *.pdf *.ps *.html *~ ts-mode
