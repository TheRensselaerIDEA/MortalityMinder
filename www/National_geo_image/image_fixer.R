# Uses imagicmagik via 'magick'
install.packages('magick')
library(magick)

# Add transparency to images
frink <- image_read("Despair.png")
frink_x <- image_transparent(frink, "white", fuzz = 0)
image_write(frink_x, "Despair.png")

frink <- image_read("All Cause.png")
frink_x <- image_transparent(frink, "white", fuzz = 0)
image_write(frink_x, "All Cause.png")

frink <- image_read("Cancer.png")
frink_x <- image_transparent(frink, "white", fuzz = 0)
image_write(frink_x, "Cancer.png")

frink <- image_read("Assault.png")
frink_x <- image_transparent(frink, "white", fuzz = 0)
image_write(frink_x, "Assault.png")

frink <- image_read("Cardiovascular.png")
frink_x <- image_transparent(frink, "white", fuzz = 0)
image_write(frink_x, "Cardiovascular.png")

