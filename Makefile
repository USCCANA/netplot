inst/NEWS: NEWS.md
	Rscript -e "rmarkdown::pandoc_convert('NEWS.md', 'plain', output='inst/NEWS')"&& \
	head -n 80 inst/NEWS

README.md: README.qmd
	quarto render README.qmd

.PHONY: checfull checkv clean

check: 
	Rscript -e "devtools::check()"

install:
	Rscript -e "devtools::install()"

checkfull: R/*.R inst/NEWS README.md
	R CMD build . \&& 
		R CMD check --as-cran $(PKGNAME)_$(VERSION).tar.gz

checkv: $(PKGNAME)_$(VERSION).tar.gz
	R CMD check --as-cran --use-valgrind $(PKGNAME)_$(VERSION).tar.gz

clean:
	cd ../ && \
		rm -rf $(PKGNAME).Rcheck $(PKGNAME)_$(VERSION).tar.gz

.PHONY: man docker
man: R/* 
	Rscript --vanilla -e 'roxygen2::roxygenize()'

docker:
	docker run -v$(pwd):/pkg/ -w/pkg --rm -i uscbiostats/fmcmc:latest make check

checkd:
	apt update && \
		tlmgr install amsmath && \
		install2.r igraph gridGraphics gridBase gridExtra magrittr tinytest sna \
		ggraph rmarkdown knitr intergraph igraphdata pkgdown markdown && \
		apt install libglpk-dev --no-install-recommends -y && \
		R CMD build . && \
		R CMD check --no-manual $(PKGNAME)_$(VERSION).tar.gz
