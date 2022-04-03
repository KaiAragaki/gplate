# Source:
# https://en.wikipedia.org/wiki/Microplate#Manufacture_and_composition
# Added a one well 'plate'

plate_formats <- data.frame(wells = c(1, 6, 12, 24, 48, 96, 384, 1536, 3456),
                            rows = c(1, 2, 3, 4, 6, 8, 16, 32, 48),
                            cols = c(1, 3, 4, 6, 8, 12, 24, 48, 72))

usethis::use_data(plate_formats, overwrite = TRUE)
