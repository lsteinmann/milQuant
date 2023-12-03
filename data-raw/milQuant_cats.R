## code to prepare `categories` dataset goes here

# conn <- connect_idaifield(project = "milet", pwd = "hallo")
# config <- get_configuration(connection = conn)

categories <- read.csv(system.file("extdata/milet_categories.csv",
                                   package = "milQuant"))

milQuant_cats <- list(

  Find =
    factor(categories$category[which(categories$parent == "Find")],
           levels = categories$category[which(categories$parent == "Find")]),

  Feature =
    factor(categories$category[which(categories$parent == "Feature")],
           levels = categories$category[which(categories$parent == "Feature")]),

  Quantification =
    factor(categories$category[which(categories$parent == "Quantification")],
           levels = categories$category[which(categories$parent == "Quantification")])

)

milQuant_cats$Find

usethis::use_data(milQuant_cats, overwrite = TRUE)
