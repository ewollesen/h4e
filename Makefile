.SUFFIXES: .pdf .fdf .pl

.fdf.pdf:
	pdftk CSheet.pdf fill_form $< output $@ 

.pl.fdf:
	./genfdf.pl $< > $@

#pontus.pl:
#	pdftk CSheet.pdf dump_data_fields | perl fields2pl.pl > CSheet.pl

pontus.pdf: pontus.fdf
pontus.fdf: pontus.pl