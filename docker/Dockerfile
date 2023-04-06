FROM rocker/r-ver:4.2.2

RUN install2.r \
	igraph gridgraphics \
	gridbase gridextra magrittr \
	tinytest \
	sna ggraph rmarkdown knitr intergraph igraphdata pkgdown

#RUN apt-get install -y --no-install-recommends \
#	libharfbuzz-dev libfribidi-dev \
#	libssl-dev libxml2-dev libfontconfig1-dev libcurl4-openssl-dev \
#	libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev


CMD ["bash"]

