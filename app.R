shiny::reactlogReset()
options(shiny.reactlog = TRUE)

# RUN before publishing!
source("data-raw/NEWS.R")

pkgload::load_all()
milQuant:::devel_idf_version()
milQuant::run_milQuant_app()

reactlogShow()

#readRDS("inst/app/www/settings/db_settings.RDS")



