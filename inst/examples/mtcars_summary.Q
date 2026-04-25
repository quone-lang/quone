type alias Cars <-
    dataframe
        { model : Vector Character
        , mpg   : Vector Double
        , cyl   : Vector Integer
        , hp    : Vector Double
        , wt    : Vector Double
        }

mtcars_demo : Cars -> dataframe { cyl : Vector Integer, n_cars : Vector Integer, avg_mpg : Vector Double, avg_hp : Vector Double }
mtcars_demo cars <-
    cars
        |> filter (mpg > mean mpg)
        |> mutate { power_to_weight = hp / wt }
        |> group_by { cyl }
        |> summarize { n_cars = count model, avg_mpg = mean mpg, avg_hp = mean hp }
        |> arrange (desc avg_mpg)

