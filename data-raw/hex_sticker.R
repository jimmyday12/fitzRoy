# Script to generate hexsticker

library(hexSticker)

sticker(
  package = "fitzRoy", # package name to display on sticker
  p_size = 8, # size of package name
  p_y = 1.5, # y of package name
  p_color = "#C9B128", # color of package name
  subplot = "man/figures/footy.png", # sticker feature
  s_x = 1, # x of feature
  s_y = 0.9, # y of feature
  s_width = 0.55, # width of feature - maintains aspect ratio
  h_size = 2, # border
  h_color = "#C9B128", # color of border
  h_fill = "white", # color of background
  url = "jimmyday12.github.io/fitzRoy", # url at the bottom
  u_color = "#C9B128", # color of url at the bottom
  u_size = 1.5, # size of url at the bottom
  filename = "man/figures/fitz_hex.png" # location to save the image
)
