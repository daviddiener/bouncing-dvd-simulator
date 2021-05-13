## External window
x11()
par(mar = c(1, 1, 1, 1), bg = "black")

## setup field
plot.new()
plot.window(xlim = c(0,100), ylim = c(0,100))
# draw borders
lines(c(0, 0, 100, 100, 0), c(0, 100, 100, 0, 0), type = "l", lwd = 10, col = "white")


# field boundaries
xmin <- 2.5
xmax <- 88
ymin <- 2.5
ymax <- 95.5

## place init
x <- 50
y <- 50

## Set direction
dx <- runif(1, 1, 2)
dy <- runif(1, 1, 2)

## colorcode
color <- "purple"
chooseColor = function(){
  xRand <- sample(1:8)
  if(xRand == 1) colorRand = "purple"
  if(xRand == 2) colorRand = "blue"
  if(xRand == 3) colorRand = "orange"
  if(xRand == 4) colorRand = "green"
  if(xRand == 5) colorRand = "white"
  if(xRand == 6) colorRand = "red"
  if(xRand == 7) colorRand = "yellow"
  if(xRand == 8) colorRand = "lightblue"
  return (colorRand)
}
while(1) {
  Sys.sleep(.1)
  
  points(x+5, y, pch = 15, col = "black", cex = 11)
  
  # Move logo
  x <- x + dx
  y <- y + dy
  
  # Collision detection
  if (x > xmax | x < xmin) {
    if (x < xmin) x <- xmin
    if (x > xmax) x <- xmax
    dx <- -dx * runif(1, .9, 1.1)
    color = chooseColor()
  }
  if (y < ymin | y > ymax) {
    if (y < ymin) y <- ymin
    if (y > ymax) y <- ymax
    dy <- -dy * runif(1, .9, 1.1)
    color = chooseColor()
  }
  
  # Draw logo
  points(x, y, pch = 'D', col = color, cex = 2)
  points(x+5, y, pch = 'V', col = color, cex = 2)
  points(x+10, y, pch = 'D', col = color, cex = 2)
  
  # Draw borders
  lines(c(0, 0, 100, 100, 0), c(0, 100, 100, 0, 0), type = "l", lwd = 10, col = "white")
  
}