MAKEINFO=/usr/bin/makeinfo
TEXI2DVI=/usr/bin/texi2dvi
TEXI2PDF=/usr/bin/texi2pdf
DVIPS=/usr/bin/dvips

FILES=$(wildcard *.texi)
MASTERFILE=typo3-emacs.texi

.PHONY : all clean veryclean

all : htmlsplit htmlnosplit pdf dvi ps

htmlsplit : typo3-emacs/index.html

htmlnosplit : typo3-emacs.html

pdf : typo3-emacs.pdf

typo3-emacs.pdf : $(FILES)
	@$(TEXI2PDF) -E $(MASTERFILE)

ps : typo3-emacs.ps

typo3-emacs.ps : typo3-emacs.dvi
	@$(DVIPS) $<

dvi : typo3-emacs.dvi

typo3-emacs.dvi : $(FILES)
	@$(TEXI2DVI) -E $(MASTERFILE)

typo3-emacs/index.html : $(FILES)
	@$(MAKEINFO) --html $(MASTERFILE)
	@mkdir typo3-emacs/images/
	@cp images/* typo3-emacs/images/

typo3-emacs.html : $(FILES)
	@$(MAKEINFO) --no-split --html $(MASTERFILE)

clean :
	@rm -f *.log *.aux *.toc *.dvi *.cp *.fn *.fns \
	       *.ky *.kys *.vr *.vrs *.pg *.tp

veryclean : clean
	@rm -rf *.pdf *.ps *.html *~ typo3-emacs
