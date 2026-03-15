## code to prepare `milQuant_inputTypes` dataset goes here
conn <- connect_idaifield(project = "milet", pwd = "hallo")

config <- get_configuration(conn)
milQuant_inputTypes <- parse_field_inputtypes(config)

milQuant_inputTypes <- milQuant_inputTypes %>%
  mutate(category_label = remove_config_names(category)) %>%
  mutate(category_label = remove_config_names(parent)) %>%
  mutate(category_label = remove_config_names(fieldname))


usethis::use_data(milQuant_inputTypes, overwrite = TRUE)

