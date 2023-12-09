## code to prepare `milQuant_inputTypes` dataset goes here

config <- get_configuration(conn)
milet_new <- get_field_inputtypes(config) %>%
  as.data.frame() %>%
  mutate(source = "config")

tmp <- jsonlite::read_json("https://raw.githubusercontent.com/dainst/idai-field/master/core/config/Config-Milet.json")
milet_old <- get_field_inputtypes(tmp) %>%
  as.data.frame() %>%
  mutate(source = "Config-Milet.json")


tmp <- jsonlite::read_json("https://raw.githubusercontent.com/dainst/idai-field/master/core/config/Library/Categories.json")
tmp <- list(forms = tmp)
def_cats <- get_field_inputtypes(tmp) %>%
  as.data.frame() %>%
  mutate(source = "Categories.json")

tmp <- jsonlite::read_json("https://raw.githubusercontent.com/dainst/idai-field/master/core/config/Library/Forms.json")
tmp <- list(forms = tmp)
def_forms <- get_field_inputtypes(tmp) %>%
  as.data.frame() %>%
  mutate(source = "Forms.json")

milQuant_inputTypes <- rbind(milet_new, milet_old, def_cats, def_forms)

milQuant_inputTypes <- milQuant_inputTypes %>%
  filter(!category == "archaeoDox") %>%
  filter(!field %in% c("constraintIndexed", "references", "subfields", "name", "valuelistId")) %>%
  filter(!inputType %in% c("FALSE")) %>%
  mutate(comb = paste(category, "/", field, sep = ""))

# match the first occurence of each combination of category and inputType
# which should always be in the milet-config
# and then reduce that to unique values to filter out duplicates that
# i don't want
ind <- unique(match(milQuant_inputTypes$comb, milQuant_inputTypes$comb))
milQuant_inputTypes <- milQuant_inputTypes[ind, ]
milQuant_inputTypes$comb <- NULL

usethis::use_data(milQuant_inputTypes, overwrite = TRUE)

table(milQuant_inputTypes$inputType, milQuant_inputTypes$source) %>%
  as.data.frame() %>%
  ggplot(aes(x = fct_reorder(Var1, -Freq), y = Freq, fill = Var2)) +
  geom_col()


test <- get_idaifield_docs(conn) %>%
  simplify_idaifield(spread_fields = FALSE, silent = TRUE)

colnames <- unique(unlist(lapply(test, names)))

test %>%
  idaifield_as_matrix() %>%
  as.data.frame()



milfields <- milQuant_inputTypes %>%
  filter(grepl("onfig", milQuant_inputTypes$source)) %>%
  pull(field)

sort(colnames[!colnames %in% milQuant_inputTypes$field])

