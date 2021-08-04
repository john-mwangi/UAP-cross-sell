pr <- plumber::plumb("fetch_popular_products_api.R")

pr$run(host = "127.0.0.1", port = 6210)