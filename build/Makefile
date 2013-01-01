# This Makefile creates PasDoc generated documentation for Almagesto.
# Documentation is created in subdirectories named after pasdoc's --format
# names (html/, latex/ etc.)
#
# Targets:
#
#   html latex pdf dvi ps htmlhelp rtf
#     Make documentation in requested format.
#
#   clean-doc
#     Delete all generated documentation.
#
# This makefile is based on PasDoc Autodoc Makefile [http://pasdoc.svn.sourceforge.net/viewvc/pasdoc/trunk/source/autodoc/Makefile]


# Making documentation ----------------------------------------

# Name of pasdoc binary. Remember that you can override it at
# make's command-line, like `make PASDOC=my_pasdoc'
PASDOC=pasdoc

# Directory where to create the documentation
DOCSDIR=../docs

# Directory where to look for the sources
SOURCESDIR=../src

# These options will be passed to every pasdoc invocation,
# i.e. for every output format.
PASDOC_OPTIONS=--include $(SOURCESDIR) $(SOURCESDIR)/*.pas \
  -DFPC  \
  --title "Almagesto Documentation" --write-uses-list  --auto-abstract \
  --introduction=$(DOCSDIR)/docintro.txt

GRAPHVIZ_COMMANDS=dot -Grankdir=LR -Tpng -oGVClasses.png GVClasses.dot; \
                  dot -Grankdir=LR -Tpng -oGVUses.png GVUses.dot

# These options will be passed only when making docs in format that
# supports including graphviz graphs.
PASDOC_GRAPHVIZ_OPTIONS=--graphviz-classes --graphviz-uses --link-gv-classes png --link-gv-uses png

# How to call hhc (html help compiler) ?
# Default value means that it must be available on path.
# As usual, remember you can override it at make's command-line.
HHC=hhc

.PHONY: html latex pdf dvi ps htmlhelp rtf clean-doc

html:
	mkdir -p $(DOCSDIR)/html/
	$(PASDOC) $(PASDOC_OPTIONS) $(PASDOC_GRAPHVIZ_OPTIONS) \
	  --output=$(DOCSDIR)/html/ --use-tipue-search
	cd $(DOCSDIR)/html/; $(GRAPHVIZ_COMMANDS)

# Ignore exit code from hhc (it's always 1 ???)
htmlhelp:
	mkdir -p $(DOCSDIR)/htmlhelp/
	$(PASDOC) $(PASDOC_OPTIONS) $(PASDOC_GRAPHVIZ_OPTIONS) \
	  --format=htmlhelp --output=$(DOCSDIR)/htmlhelp/
	cd $(DOCSDIR)/htmlhelp/; $(GRAPHVIZ_COMMANDS)
	-cd htmlhelp/; $(HHC) docs.hhp

# Just shortcuts
latex: latex/docs.tex
pdf: latex/docs.pdf
dvi: latex/docs.dvi
ps: latex/docs.ps
rtf: latex2rtf/docs.rtf

latex/docs.tex:
	mkdir -p $(DOCSDIR)/latex/
	$(PASDOC) $(PASDOC_OPTIONS) \
	  --format=latex --output=$(DOCSDIR)/latex/

# (Yes, I must do `cd latex' before calling pdflatex, I can't simply call
# `pdflatex latex/docs.tex' because then many output files of pdflatex
# would be placed in current dir)
latex/docs.pdf: latex/docs.tex
	cd $(DOCSDIR)/latex; pdflatex --file-line-error-style -interaction=nonstopmode docs.tex
	cd $(DOCSDIR)/latex; pdflatex --file-line-error-style -interaction=nonstopmode docs.tex

latex/docs.dvi: latex/docs.tex
	cd $(DOCSDIR)/latex; latex --file-line-error-style -interaction=nonstopmode docs.tex
	cd $(DOCSDIR)/latex; latex --file-line-error-style -interaction=nonstopmode docs.tex

latex/docs.ps: latex/docs.dvi
	cd $(DOCSDIR)/latex; dvips docs.dvi -o docs.ps

latex2rtf/docs.tex:
	mkdir -p $(DOCSDIR)/latex2rtf/
	$(PASDOC) $(PASDOC_OPTIONS) \
	  --format=latex2rtf --output=$(DOCSDIR)/latex2rtf/

# I must have latex/docs.tex as a prerequisite, to run latex over it
# to generate docs.aux file. Then I can use latex/docs.aux file with latex2rtf
# so that latex2rtf can make appropriate links.
latex2rtf/docs.rtf: latex2rtf/docs.tex latex/docs.tex
	cd $(DOCSDIR)/latex/; latex --file-line-error-style -interaction=nonstopmode docs.tex
	cd $(DOCSDIR)/latex2rtf/; latex2rtf -a ../latex/docs.aux docs.tex

clean-doc:
	cd $(DOCSDIR); rm -Rf html/ latex/ latex2rtf/ htmlhelp/

