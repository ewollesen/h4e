.SUFFIXES: .pdf .fdf .pl
.PHONY: tags

tags:
	find . -iname "*.hs" -print | xargs hasktags
	rm -f tags

.fdf.pdf: $<
	pdftk CharacterSheetFillable.pdf fill_form $< output $@

%.fdf: *.hs
	runghc $*.hs > $@
