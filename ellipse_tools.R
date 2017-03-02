# Ellipse Tools

library(shiny)


X0 <- 5
Y0 <- 5


ellipseBoundariesX <- function(y, x0, y0, rx, ry, alpha) {
  a1 = cos(alpha) / rx
  a2 = sin(alpha) / ry
  c1 = (-x0 * cos(alpha) + (y - y0) * sin(alpha)) / rx
  c2 = (-x0 * sin(alpha) - (y - y0) * cos(alpha)) / ry
  s = sqrt(a1^2 + a2^2 - (a1 * c2 - a2 * c1)^2)
  xmin = (- (a1 * c1 + a2 * c2) - s) / (a1^2 + a2^2)
  xmax = (- (a1 * c1 + a2 * c2) + s) / (a1^2 + a2^2)
  c(xmin, xmax)
}


ellipseBoundariesY <- function(x, x0, y0, rx, ry, alpha) {
  a1 = sin(alpha) / rx
  a2 = -cos(alpha) / ry
  c1 = (-y0 * sin(alpha) + (x - x0) * cos(alpha)) / rx
  c2 = (y0 * cos(alpha) + (x - x0) * sin(alpha)) / ry
  s = sqrt(a1^2 + a2^2 - (a1 * c2 - a2 * c1)^2)
  xmin = (- (a1 * c1 + a2 * c2) - s) / (a1^2 + a2^2)
  xmax = (- (a1 * c1 + a2 * c2) + s) / (a1^2 + a2^2)
  c(xmin, xmax)
}


ellipseRangeX <- function(x0, y0, rx, ry, alpha) {
  theta1 = atan(-ry / rx * tan(alpha))
  theta2 = theta1 + pi
  x1 = x0 + rx * cos(theta1) * cos(alpha) - ry * sin(theta1) * sin(alpha)
  x2 = x0 + rx * cos(theta2) * cos(alpha) - ry * sin(theta2) * sin(alpha)
  c(x1, x2)
}


ellipseRangeY <- function(x0, y0, rx, ry, alpha) {
  theta1 = atan(ry / rx / tan(alpha))
  theta2 = theta1 + pi
  y1 = y0 + ry * sin(theta1) * cos(alpha) + rx * cos(theta1) * sin(alpha)
  y2 = y0 + ry * sin(theta2) * cos(alpha) + rx * cos(theta2) * sin(alpha)
  c(y1, y2)
}


plotEllipse <- function(x0, y0, a, b, alpha, ..., add = TRUE) {
  theta <- seq(0, 2 * pi, length=100)
  x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
  y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
  if (add) lines(x, y, type = "l", ...) else plot(x, y, type = "l", ...)
}


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("axis1", "First Axis Radius:", min = 1, max = 10, value = 2),
      sliderInput("axis2", "Second Axis Radius:", min = 1, max = 10, value = 2),
      sliderInput("alpha", "Rotation Angle:", min = 0, max = 359, value = 0)
    ),
    mainPanel(plotOutput("ellipsePlot", click = "plot_click"))
  )
)


server <- function(input, output) {
  output$ellipsePlot <- renderPlot({
    plotEllipse(X0, Y0, input$axis1, input$axis2, input$alpha / 360 * 2 * pi,
                add = FALSE, xlim = c(-10, 20), ylim = c(-10, 20))
    
    xrange <- ellipseRangeX(X0, Y0, input$axis1, input$axis2, input$alpha / 360 * 2 * pi)
    yrange <- ellipseRangeY(X0, Y0, input$axis1, input$axis2, input$alpha / 360 * 2 * pi)
    print(xrange)
    print(yrange)
    abline(v = xrange[1], col = "blue")
    abline(v = xrange[2], col = "blue")
    abline(h = yrange[1], col = "blue")
    abline(h = yrange[2], col = "blue")
    
    y <- input$plot_click$y
    if (! is.null(y)) {
      xrange <- ellipseBoundariesX(y, X0, Y0, input$axis1, input$axis2, input$alpha / 360 * 2 * pi)
      lines(xrange, rep(y, 2), col = "red", lwd = 2)
    }
    x <- input$plot_click$x
    if (! is.null(x)) {
      yrange <- ellipseBoundariesY(x, X0, Y0, input$axis1, input$axis2, input$alpha / 360 * 2 * pi)
      lines(rep(x, 2), yrange, col = "red", lwd = 2)
    }
  })
}


shinyApp(ui = ui, server = server)