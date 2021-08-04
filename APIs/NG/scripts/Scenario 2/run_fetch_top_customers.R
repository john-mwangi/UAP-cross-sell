pr <- plumber::plumb("fetch_top_customers_api.R")

pr$run(host = "127.0.0.1", port = 6209)