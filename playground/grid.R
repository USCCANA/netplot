library(lattice)
library(grid)

x <- rnorm(10)
y <- rnorm(10)

xyplot(y~x)

grid.ls()
seekViewport("plot_01.xlab.vp")

grid.text("Hola!", name = "hola")

# List all viewports
grid.ls(viewports = TRUE, grobs =FALSE, fullNames = TRUE)

# List all grub
grid.ls(viewports = FALSE, grobs = TRUE, fullNames = TRUE)

# Changing points pch and color
grid.edit(
  "plot_01.xyplot.points.panel.1.1", grep=TRUE,
  gp = gpar(col="blue"), pch=19, cex=2
)

# Updating xlabel

grid.remove("hola")


barchart(Party ~ Amount_Donated, sortedTotals)


xyplot(mpg ~ disp, mtcars, main="Fast Cars")
seekViewport("plot_01.panel.1.1.vp")
grid.abline(slope=0, intercept = 25, gp = gpar(col="black", lty=2))
grid.text(
  label = "Pontiac Firebird",
  x = unit(400, "native"),
  y = unit(19.2, "native"),
  just = c(0,0)
  )

# Adding a plot in the bottom left
vp <- viewport(
  width=.5, height=.5, just = c(0,1),
  name = trellis.grobname("GRAPH", ""),
  )
grid.newpage()
pushViewport(vp)
print(xyplot(mpg ~ disp, mtcars), newpage=FALSE)
grid.ls(viewports = TRUE)


# Working w/ ggplot2
library(ggplot2)
qplot(disp, mpg, data=mtcars, main="Fast Cars")
grid.force()
grid.ls()
seekViewport("title.2-4-2-4")
grid.rect(.5, .5, gp=gpar(lty=2, fill="lightblue"), name = "myrect")
grid.edit(
  "GRID.text.551",
  x=unit(1, "npc"),
  y=unit(.5, "npc"),
  vjust = .5,
  hjust = 1
  )

grid.remove("myrect")

library(netplot)
library(gridGraphics)

nplot(igraph::make_ring(4))
grid.echo(newpage = TRUE)
grid.ls()
grid.edit("graphics-plot-1-rect-1", gp = gpar(fill="lightgray"))
grid.edit("graphics-plot-1-polygon-11", gp = gpar(fill="white"))


plot(mpg~disp, mtcars, pch=16, main="Fast Cars")
grid.echo(newpage = TRUE)
grid.ls()
grid.edit("graphics-plot-1-main-1", gp = gpar(col="red"))
grid.remove("graphics-plot-1-main-1")


grid.newpage()
vpgen    <- viewport(name = "gen")
vptop    <- viewport(.25, .75, width = .5, height = .5, name="vp-top")
vpbottom <- viewport(.75, .25, width = .5, height = .5, name="vp-bottom")

pushViewport(vpgen)
pushViewport(vptop)
upViewport()
pushViewport(bottom)

grid.rect(gp = gpar(lty=2), vp=vpgen)
grid.rect(gp = gpar(lty=1), vp=vptop)
grid.text(y=.75, label = "Some drawing in 1",vp=vptop)
grid.text(y=.25, label = "More drawing in 1",vp=vptop)

grid.rect(gp = gpar(lty=1), vp=vpbottom)
grid.text(y=.75, label = "Some drawing in 2",vp=vpbottom)
grid.text(y=.25, label = "More drawing in 2",vp=vpbottom)

grid.ls(viewports = TRUE)


library(netplot)
library(grid)

net <- igraph::sample_smallworld(1, 10, 4, .1)

data(UKfaculty, package="igraphdata")

ans <- nplot(UKfaculty)

# aspectt ratio -----------------------------------------------------------------
asp <- list(
  unit(min(1,diff(ans$xlim)/diff(ans$ylim)), "snpc"),
  unit(min(1,diff(ans$ylim)/diff(ans$xlim)), "snpc")
  )

# Creating the viewport
top <- viewport(
  width  = asp[[1]], # aspectt ratio preserved
  height = asp[[2]],
  xscale = ans$xlim + .04*diff(ans$xlim)*c(-1,1),
  yscale = ans$ylim + .04*diff(ans$ylim)*c(-1,1)
  )

grid.newpage()
pushViewport(top)

# Drawing ----------------------------------------------------------------------

# Drawing edges
for (i in seq_along(ans$edge.coords)) {

  # Computing vector of colors
  n     <- nrow(ans$edge.coords[[i]])
  col   <- ans$edge.color[[i]](seq(0, 1, length.out = n-1))
  col   <- rgb(col[,1:3], alpha = col[,4], maxColorValue = 255)
  coord <- ans$edge.coords[[i]]

  grid.polyline(
    as.vector(t(cbind(coord[-n,1], coord[-1,1]))),
    as.vector(t(cbind(coord[-n,2], coord[-1,2]))),
    default.units = "native",
    name = sprintf("edge.coords%i",i),
    id = sort(rep(1:(n-1), 2)),
    gp = gpar(col = col, lwd=ans$edge.width[i], lineend=1)
    )

  ans$edge.color[[i]] <- col[n-1]
}

# Drawing arrows
for (i in seq_along(ans$edge.arrow.coords))
  grid.polygon(
    ans$edge.arrow.coords[[i]][,1],
    ans$edge.arrow.coords[[i]][,2],
    default.units = "native",
    name = sprintf("edge.arrow.coords%i",i),
    gp = gpar(col = ans$edge.color[[i]], fill=ans$edge.color[[i]],
              lwd=ans$edge.width[i])
    )

# Drawing vertices
for (i in seq_along(ans$vertex.coords)) {
  grid.polygon(
    ans$vertex.coords[[i]][,1],
    ans$vertex.coords[[i]][,2],
    default.units = "native",
    gp = gpar(fill = ans$vertex.color[i], col = ans$vertex.color[i]),
    name = sprintf("vertex.coords%i",i)
  )

  grid.polygon(
    ans$vertex.frame.coords[[i]][,1],
    ans$vertex.frame.coords[[i]][,2],
    default.units = "native",
    gp = gpar(fill = ans$vertex.frame.color[i], col = ans$vertex.frame.color[i]),
    name = sprintf("vertex.frame.coords%i",i)
  )
}


# Exporting --------------------------------------------------------------------
grid.DLapply(function(x) {

  if (is.grob(x)) {

    if (grepl("vertex\\.coords", x$name))
      gridSVG::garnishGrob(
        x,
        `stroke-linecap`="butt",
        onclick = sprintf("alert('This is element: %s')", x$name),
        onmouseover="evt.target.setAttribute('opacity', '0.5');",
        onmouseout="evt.target.setAttribute('opacity','1)');"
        )
    else
      gridSVG::garnishGrob(
        x,
        `stroke-linecap`="butt"
      )

  } else
    x

})

gridSVG::grid.export("playground/grid.svg", progress = TRUE, htmlWrapper = TRUE, )
