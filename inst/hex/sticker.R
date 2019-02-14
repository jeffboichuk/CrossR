library(hexSticker)
library(showtext)
font_add_google("Berkshire Swash")
font_add_google("Roboto")
showtext_auto()

sticker(subplot = "inst/hex/barbell.png",
        package = "CrossR",
        s_x = 1, s_y = .75, s_width = .4, s_height = .01,
        p_size = 7, p_color = "#232D4B", p_family = "Berkshire Swash",
        h_size = 1.8, h_color = "#E57200",
        url = "https://github.com/jeffboichuk/CrossR/",
        u_size = 1.1, u_color = "#232D4B", u_family = "Roboto",
        filename = "man/figures/logo.png")
