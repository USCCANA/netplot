FROM rocker/r-base

RUN apt-get update && \
	apt-get install -y --no-install-recommends \
	r-cran-igraph r-cran-gridgraphics \
	r-cran-gridbase r-cran-gridextra r-cran-magrittr \
	r-cran-sna r-cran-ggraph r-cran-rmarkdown r-cran-knitr && \
	Rscript -e 'utils::install.packages(c("intergraph", "igraphdata"))'


CMD ["bash"]

