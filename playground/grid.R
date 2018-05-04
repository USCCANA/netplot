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

view


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
