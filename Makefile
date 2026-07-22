README.md: README.qmd
	quarto render README.qmd

.PHONY: build check install checkfull checkv clean man docker checkd

build: docs
	R CMD build .

check: docs
	Rscript -e "devtools::check()"

install: docs
	Rscript -e "devtools::install()"

docs:
	Rscript -e "devtools::document()"