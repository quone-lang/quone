mtcars_demo <-
    mtcars
        |> filter (mpg > mean mpg)
        |> mutate { power_to_weight = hp / wt }
        |> group_by { cyl }
        |> summarize
            { n_cars = count mpg
            , avg_mpg = mean mpg
            , avg_hp = mean hp
            }
        |> arrange { desc avg_mpg }
