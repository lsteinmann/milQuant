## code to prepare `NEWS` html include

rmarkdown::render(input = "NEWS.md",
                  output_file = "inst/app/www/html/NEWS.html",
                  output_options = list(theme = NULL,
                                        mathjax = FALSE,
                                        self_contained = FALSE))
files <- list.files("inst/app/www/html/NEWS_files", full.names = TRUE, recursive = TRUE)
file.remove(files)
unlink("inst/app/www/html/NEWS_files", recursive = TRUE)

milQuant_NEWS <- readLines("inst/app/www/html/NEWS.html")

start <- which(milQuant_NEWS == '<body>')

milQuant_NEWS <- milQuant_NEWS[-c(1:(start+1))]

end <- which(milQuant_NEWS == '</body>')

milQuant_NEWS <- milQuant_NEWS[-c(length(milQuant_NEWS):end)]

milQuant_NEWS <- gsub(' class=".*."', '', milQuant_NEWS)

milQuant_NEWS <- milQuant_NEWS[-which(milQuant_NEWS == "")]

writeLines(milQuant_NEWS, "inst/app/www/html/NEWS.html")

# run before publishing
