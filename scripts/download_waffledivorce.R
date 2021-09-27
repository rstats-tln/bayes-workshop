url <- "https://raw.githubusercontent.com/rmcelreath/rethinking/master/data/WaffleDivorce.csv"
download.file(url = url, destfile = here::here("data", tolower(basename(url))))
