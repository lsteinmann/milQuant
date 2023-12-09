## code to prepare `milQuant_inputTypes` dataset goes here

config <- get_configuration(conn)
tmp <- jsonlite::read_json(system.file(package = "milQuant", "extdata/Config-Milet.json"))
config$forms <- append(config$forms, tmp$forms)

tmp <- jsonlite::read_json(system.file(package = "milQuant", "extdata/Categories.json"))
config$forms <- append(config$forms, tmp)

tmp <- jsonlite::read_json(system.file(package = "milQuant", "extdata/Forms.json"))
config$forms <- append(config$forms, tmp)


milQuant_inputTypes <- get_field_inputtypes(config) %>%
  as.data.frame() %>%
  filter(!category == "archaeoDox") %>%
  filter(!field %in% c("constraintIndexed", "valuelistId"))

usethis::use_data(milQuant_inputTypes, overwrite = TRUE)
