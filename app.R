shiny::reactlogReset()
options(shiny.reactlog = TRUE)
pkgload::load_all()
milQuant::run_milQuant_app()
milQuant:::devel_idf_version()

reactlogShow()

#readRDS("inst/app/www/settings/db_settings.RDS")



# source("data-raw/NEWS.R")
