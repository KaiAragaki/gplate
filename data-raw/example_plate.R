## code to prepare `example_plate` dataset goes here
set.seed(8332)
example_plate <- matrix(data = sample(x = 1:10,
                                      size = 96,
                                      replace = TRUE),
                        nrow = 8,
                        ncol = 12)

usethis::use_data(example_plate, overwrite = TRUE)
