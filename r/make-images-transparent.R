## Make background of images transparent

library(magick)
library(tidyverse)

images_path <- fs::dir_ls("images-mash-up")
fs::dir_info("images-mash-up")
images <- map(images_path, image_read)
images <- map(images, image_transparent, "white")

new_dir <- "images-edited"

fs::dir_create(new_dir)

iwalk(images, ~image_write(.x, path = fs::path(new_dir, str_remove(.y, ".*/"))))
