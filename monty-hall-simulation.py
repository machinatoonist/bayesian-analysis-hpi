
import random

def chance_of_winning(num_doors, num_simulations, change_door=True):
    # Initialize the count of winning
    count_win = 0

    # Simulate num_simulations games
    for i in range(num_simulations):
        # Place the prizes behind the doors
        doors = random.sample(["goat"]*(num_doors-1) + ["car"], num_doors)

        # The contestant chooses a door
        initial_choice = random.randint(1, num_doors)

        # The host reveals a door with a donkey/not the contestant's initial choice
        intersection = list(set(range(1, num_doors+1)) - {initial_choice} - \
            set([i+1 for i, x in enumerate(doors) if x == "car"]))
        host_reveal = random.sample(intersection, k=1)

        if change_door:
            # The contestant changes the door
            final_choice = list(set(range(1, num_doors+1)) - {initial_choice} - set(host_reveal))
        else:
            final_choice = [initial_choice]

        # Check if the contestant wins and update the count
        if doors[final_choice[0]-1] == "car":
            count_win += 1

    # Calculate the probability of winning the prize
    prob_win = count_win / num_simulations

    return prob_win

# Test the function for the case of 3 doors 
chance_of_winning(num_doors=3, num_simulations=10000, change_door=False)

chance_of_winning(num_doors=3, num_simulations=10000, change_door=True)

# Improvement in odds when changing doors
perc_3door = chance_of_winning(num_doors=3, num_simulations=10000, change_door=True) - \
    chance_of_winning(num_doors=3, num_simulations=10000, change_door=False)
    
print(f'Improvement in odds when changing doors with 3 doors: {perc_3door*100:.2f}%')

# Test of function for the case of 5 doors
chance_of_winning(num_doors=5, num_simulations=10000, change_door=False)

chance_of_winning(num_doors=5, num_simulations=10000, change_door=True)

# Improvement in odds when changing doors
perc_5door = chance_of_winning(num_doors=5, num_simulations=10000, change_door=True) - \
    chance_of_winning(num_doors=5, num_simulations=10000, change_door=False)
    
print(f'Improvement in odds when changing doors with 5 doors: {perc_5door*100:.2f}%')



