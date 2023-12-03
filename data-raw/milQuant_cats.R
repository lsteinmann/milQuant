## code to prepare `categories` dataset goes here

# conn <- connect_idaifield(project = "milet", pwd = "hallo")
# config <- get_configuration(connection = conn)

categories <- read.csv(system.file("extdata/milet_categories.csv",
                                   package = "milQuant"))

milQuant_cats <- list(
  Finds = c(categories$category[which(categories$parent == "Find")]),
  Features = c(categories$category[which(categories$parent == "Feature")]),
  Quantifications = c(categories$category[which(categories$parent == "Quantification")])
)



usethis::use_data(milQuant_cats, overwrite = TRUE)
