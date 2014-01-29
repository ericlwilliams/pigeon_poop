library(ggplot2)
library(gridSVG)
library(proto)
library(rjson)
mtcars2 <- data.frame(mtcars, names = rownames(mtcars))

geom_point2 <- function (...) {
  GeomPoint2$new(...)
}

GeomPoint2 <- proto(ggplot2:::Geom, {
  objname <- "point"

  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {    
    data <- remove_missing(data, na.rm, 
                           c("x", "y", "size", "shape"), name = "geom_point")
    if (empty(data)) return(zeroGrob())
    name <- paste(.$my_name(), data$PANEL[1], sep = ".")
    with(coord_transform(coordinates, data, scales), 
         ggname(name, pointsGrob(x, y, size=unit(size, "mm"), pch=shape, 
                                        gp=gpar(
                                          col=alpha(colour, alpha),
                                          fill = alpha(fill, alpha),  
                                          label = label, 
                                          fontsize = size * .pt)))
    )
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data,
         pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape, 
                    gp=gpar(
                      col = alpha(colour, alpha), 
                      fill = alpha(fill, alpha), 
                      label = label,
                      fontsize = size * .pt)
         )
    )
  }

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(shape=16, colour="black", size=2, 
                                 fill = NA, alpha = NA, label = NA)

})

p <- ggplot(mtcars2, aes(mpg, wt, label = names)) + geom_point2() +facet_wrap(~ gear)
print(p)

grob_names <- grid.ls(print = FALSE)$name
point_grob_names <- sort(grob_names[grepl("point", grob_names)])
point_grobs_labels <- lapply(point_grob_names, function(x) grid.get(x)$gp$label)

jlabel <- toJSON(point_grobs_labels)

grid.text("value", 0.05, 0.05, just = c(0, 0), name = "text_place", gp = gpar(col = "red"))

script <- '
var txt = null;
function f() {
    var id = this.id.match(/geom_point.([0-9]+)\\.points.*\\.([0-9]+)$/);
    txt.textContent = label[id[1]-1][id[2]-1];
}

window.addEventListener("load",function(){
    var es = document.getElementsByTagName("circle");
    for (i=0; i<es.length; ++i) es[i].addEventListener("mouseover", f, false);

    txt = (document.getElementById("text_place").getElementsByTagName("tspan"))[0];

},false);
'
grid.script(script = paste("var label = ", jlabel))
grid.script(script = script)

gridToSVG()