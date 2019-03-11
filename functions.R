vertices <- read.csv("texture_vertices.csv", stringsAsFactors = FALSE)

# This function converts from percent texture to x coordinates
tex.to.x <- function(sand.pct, clay.pct) {
  if (!is.numeric(sand.pct)) {
    stop("sand.pct must be a numeric value")
  }
  
  if (sand.pct > 100 | sand.pct < 0) {
    stop("sand.pct must be between 0 and 100")
  }
  
  if (sand.pct < 1 & sand.pct > 0) {
    message(paste0("sand.pct is ", sand.pct, " and will be used as such, but did you mean ", sand.pct*10, "?"))
  }
  
  if (!is.numeric(clay.pct)) {
    stop("clay.pct must be a numeric value")
  }
  
  if (clay.pct > 100 | clay.pct < 0) {
    stop("clay.pct must be between 0 and 100")
  }
  
  if (clay.pct < 1 & clay.pct > 0) {
    message(paste0("clay.pct is ", clay.pct, " and will be used as such, but did you mean ", clay.pct*10, "?"))
  }
  
  # The convention is that the left of the triangle base is 100% sand and the right is 0%
  # This will convert so that 100% is at 0 and 0% is at 100
  # It also relies on the ratios of 30-60-90 triangles
  x <- (100 - sand.pct) - clay.pct/2
  
  return(x)
}

tex.to.y <- function(clay.pct) {
  if (!is.numeric(clay.pct)) {
    stop("clay.pct must be a numeric value")
  }
  
  if (clay.pct > 100 | clay.pct < 0) {
    stop("clay.pct must be between 0 and 100")
  }
  
  if (clay.pct < 1 & clay.pct > 0) {
    message(paste0("clay.pct is ", clay.pct, " and will be used as such, but did you mean ", clay.pct*10, "?"))
  }
  
  # Get the y coordinate component
  # This is thanks to 30-60-90 triangle ratios
  y <- clay.pct * sqrt(3)/2
  
  return(y)
}

df.xy <- dplyr::bind_rows(lapply(X = 1:nrow(vertices),
                                 FUN = function(X, dataframe){
                                   sand.pct <- dataframe$sand[X]
                                   clay.pct <- dataframe$clay[X]
                                   x <- tex.to.x(sand.pct, clay.pct)
                                   y <- tex.to.y(clay.pct)
                                   return(data.frame("x" = x, "y" = y, stringsAsFactors = FALSE))
                                 },
                                 dataframe = vertices))

vertices <- cbind(vertices, df.xy)

library(ggplot2)
ggplot(vertices) +
  geom_polygon(aes(x = x, y = y, fill = texture.class),
               color = "gray") +
  # scale_fill_discrete(palette = colorRampPalette(colors = c("darkblue", "lightblue"))(12)) +
  coord_fixed() +
  labs(fill = "Texture Class") +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
