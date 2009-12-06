MAKEINFO=/usr/bin/makeinfo
TEXI2DVI=/usr/bin/texi2dvi
TEXI2PDF=/usr/bin/texi2pdf
DVIPS=/usr/bin/dvips

FILES=$(wildcard *.texi)
MASTERFILE=t3php-mode.texi

.PHONY : all clean veryclean

all : info htmlsplit htmlnosplit pdf dvi ps

info: t3php-mode.info

t3php-mode.info: $(FILES)
	@$(MAKEINFO) $(MASTERFILE)

htmlsplit : t3php-mode/index.html

htmlnosplit : t3php-mode.html

pdf : t3php-mode.pdf

t3php-mode.pdf : $(FILES)
	@$(TEXI2PDF) -E $(MASTERFILE)

ps : t3php-mode.ps

t3php-mode.ps : t3php-mode.dvi
	@$(DVIPS) $<

dvi : t3php-mode.dvi

t3php-mode.dvi : $(FILES)
	@$(TEXI2DVI) -E $(MASTERFILE)

t3php-mode/index.html : $(FILES)
	@$(MAKEINFO) --html $(MASTERFILE)
	@mkdir t3php-mode/images/
	@cp images/* t3php-mode/images/

t3php-mode.html : $(FILES)
	@$(MAKEINFO) --no-split --html $(MASTERFILE)

clean :
	@rm -f *.log *.aux *.toc *.dvi *.cp *.fn *.fns \
	       *.ky *.kys *.vr *.vrs *.pg *.tp

veryclean : clean
	@rm -rf *.info *.pdf *.ps *.html *~ t3php-mode
