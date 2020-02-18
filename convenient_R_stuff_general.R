

# Colors
# See also: https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r 
# (especially look at all responses in that thread)

library(randomcoloR)
# Generate 100 distinct colors
n <- 100
colors_random_100 <- distinctColorPalette(n)
pie(rep(1, n), col=colors_random_100)