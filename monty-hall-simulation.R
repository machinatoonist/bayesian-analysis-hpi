# What are you chances of winning in the Monty Hall problem

chance_of_winning <- function(num_doors, num_simulations, change_door = TRUE) {
  count_win <- 0

  sample_vec <- function(x, size, replace = FALSE, prob = NULL) {
    if (length(x) == 1) return(x)
    base::sample(x, size = size, replace = replace, prob = prob)
  }

  for (i in 1:num_simulations) {
    doors <- sample(c(rep("goat", num_doors - 1), "car"))

    initial_choice <- sample.int(num_doors, 1)

    # The host reveals a door with a donkey/not the contestant's initial choice
    intersection <- c(intersect(which(seq_along(doors) != initial_choice[1]),
                        which(doors == "goat")))
    host_reveal <- sample_vec(intersection, size = 1)

    if (change_door) {
      final_choice <- setdiff(seq_along(doors), c(initial_choice, host_reveal))
    } else {
      final_choice <- initial_choice
    }

    if (doors[final_choice[1]] == "car") {
      count_win <- count_win + 1
    }
  }

  prob_win <- count_win / num_simulations

  return(prob_win)
}

# Test the function for the case of 3 doors
chance_of_winning(num_doors = 3, num_simulations = 10000, change_door = FALSE)

chance_of_winning(num_doors = 3, num_simulations = 10000, change_door = TRUE)

# Test the function for the case of 5 doors
chance_of_winning(num_doors = 5, change_door = FALSE, num_simulations = 10000)

chance_of_winning(num_doors = 5, change_door = TRUE, num_simulations = 10000)
