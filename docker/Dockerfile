FROM rocker/drd

RUN apt-get update && apt-get install \
	-y --no-install-recommends \
	libssl-dev
RUN Rscript -e 'utils::install.packages(c("igraph", "sna", "intergraph", "rmarkdown", "knitr", "ggraph"))'
RUN Rscript -e 'utils::install.packages(c("markdown", "igraphdata", "gridGraphics", "gridExtra", "gridBase", "magrittr"))'

CMD ["bash"]

