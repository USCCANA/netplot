library(netplot)
x <- igraph::make_ring(5)

ans <- nplot2(x)

library(grid)
grid.newpage()

lo <- grid::grid.layout(ncol = 2, widths = unit(c(1, 4), c("null", "cm")))

pushViewport(right  <- viewport(layout = lo, layout.pos.col = 2))
# grid.rect(gp=gpar(col="black", fill="transparent"), vp=right)
grid.circle(
  x=seq(.2, .7, length.out = 5),
  y=rep(.75, 5), r=seq(.05, .1, length.out = 5),
  gp=gpar(fill="transparent", col="gray", lwd=2),
  default.units = "npc", vp=right)
grid.text(
  "Scale",
  y = .8, vp=right
)

pushViewport(left <- viewport(layout = lo, layout.pos.col = 1, name="leftVP"))
grid.draw(ans$grob)
# upViewport()

