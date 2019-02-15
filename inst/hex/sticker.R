library(hexSticker)
library(extrafont)

sticker(
   # package name specifications
   package = "CrossR",
   p_size = 7, p_color = "#000000", p_family = "Impact",
   # subplot specifications
   subplot = "inst/hex/assign-strength-to-weakness.jpeg",
   s_x = 1, s_y = .85, s_width = .7, s_height = 11,
   # hexagon border specifications
   h_size = 1.8, h_fill = "#ffffff", h_color = "#000000",
   # url specifications
   url = "https://github.com/jeffboichuk/CrossR/",
   u_size = .9, u_color = "#000000", u_family = "Impact",
   # output specifications
   filename = "man/figures/logo.png", dpi = 1000
)
