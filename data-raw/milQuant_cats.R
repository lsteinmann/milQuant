## code to prepare `categories` dataset goes here

# conn <- connect_idaifield(project = "milet", pwd = "hallo")
# config <- get_configuration(connection = conn)
# source("data-raw/milQuant_inputTypes.R")

data("milQuant_inputTypes")

finds <- milQuant_inputTypes %>%
  filter(parent == "Find") %>%
  pull(category) %>%
  unique()

quants <- milQuant_inputTypes %>%
  filter(parent == "Quantification") %>%
  pull(category) %>%
  unique()

features <- milQuant_inputTypes %>%
  filter(parent == "Feature") %>%
  pull(category) %>%
  unique()

other <- c("Sample", "Inscription", "Building", "Impression")

milQuant_cats <- list(
  Find =
    factor(finds,
           levels = finds),

  Feature =
    factor(features,
           levels = features),

  Quantification =
    factor(quants,
           levels = quants),

  Other =
    factor(other,
           levels = other)

)


usethis::use_data(milQuant_cats, overwrite = TRUE)
