## code to prepare `milQuant_periods` dataset goes here

periods <- jsonlite::read_json(system.file("extdata/milet_periods.json",
                                package = "milQuant"))

to_add <- c("multiple", "unbestimmt")
periods <- periods$values

period_values_temp <- c(names(periods), to_add)

new_periods <- list()
new_periods$order <- factor(period_values_temp,
                            levels = period_values_temp,
                            ordered = TRUE)

new_periods$values <- period_values_temp

langs <- lapply(periods, function(x) {
  names(x$labels)
})
langs <- unique(unlist(unname(langs)))

new_periods$languages <- as.list(langs)
names(new_periods$languages) <- new_periods$languages

stop("Only run manually from here on.")

new_periods$languages$de <- lapply(periods, function(x) {
  x$labels$de
})
new_periods$languages$de <- append(new_periods$languages$de, to_add)

new_periods$languages$en <- lapply(periods, function(x) {
  x$labels$en
})
new_periods$languages$en <- append(new_periods$languages$en, c("multiple", "undetermined"))

new_periods$languages$fr <- lapply(periods, function(x) {
  x$labels$fr
})
new_periods$languages$fr <- append(new_periods$languages$fr, c("multiple", "undetermined"))


dating <- read.csv(system.file("extdata/milet_period_dat.csv",
                                package = "milQuant"), row.names = 1)

dating <- dating[-which(dating$period == "unbestimmt"), ]

new_periods$dating <- apply(dating, MARGIN = 1, FUN = function(x) {
  x <- as.list(x)
  list(period = x$period, dating.min = x$from, dating.max = x$to)
})
names(new_periods$dating) <- names(periods)

new_periods$colors <- as.list(c(dating$color, "#cd2436", "#a6a6a6"))
names(new_periods$colors) <- new_periods$values

milQuant_periods <- new_periods

usethis::use_data(milQuant_periods, overwrite = TRUE)
