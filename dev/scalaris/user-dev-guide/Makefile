LATEX=pdflatex
FILE=main.tex
all:
	$(LATEX) $(FILE) | awk -f ./latex-col.awk
	makeindex main
	$(LATEX) $(FILE) | awk -f ./latex-col.awk
	@while ( grep "Rerun to get cross-references" \
		$(subst .tex,.log, $(FILE)) > /dev/null ); do \
		echo '*** Going to Rerun LaTeX ***'; \
		$(LATEX) $(FILE) | awk -f ./latex-col.awk; \
	done

clean:
	rm -f *.aux *.ind *.ilg *.idx *.log *.out *.toc
