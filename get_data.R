library(pageviews)
# import data
wiki <- readRDS("data/wiki.rds")

# list countries wikipedia names
start <- as.Date('2017-01-01')
end <- as.Date("2017-12-31")
mat <- matrix(data = 0, nrow = nrow(wiki), ncol = as.numeric(end-start) + 1,
              dimnames = list(as.character(wiki$ISO2)))

days <- seq(start, end, 1)

for (i in 1:nrow(mat)){
  res <- article_pageviews(project = "en.wikipedia",
                           article = wiki$wikiname[i],
                           granularity = "daily",
                           start = start,
                           end = end,
                           user_type = c("user"),
                           platform = c("all"))
  mat[i,match(as.Date(res$date), days)] <- res$views
  Sys.sleep(.1)
}
#
saveRDS(mat, "data/wikimat.rds")
library(rnaturalearth)
ocean <- ne_download(110, type = "ocean", category = "physical", 
                     returnclass = "sf", destdir = "data")
ctry <- ne_countries(110, type = "countries", returnclass = "sf")
ctry[is.na(ctry$iso_a2),'iso_a2'] <- "XX"
saveRDS(ctry, "data/ctry.rds")
saveRDS(ocean, "data/ocean.rds")