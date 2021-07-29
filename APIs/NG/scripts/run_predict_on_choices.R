pr <- plumber::plumb("predict_on_choices_api.R")

pr$run(host = "127.0.0.1", port = 6211)