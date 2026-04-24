mean_score : Vector Double -> Double
mean_score xs <-
    sum xs / to_double (length xs)

