import random
import sys
sys.setrecursionlimit(2000)
import numpy as np

def next_move(current_position):
    """
    return next move from the current position
    """
    dict_move = {
        0: [4, 6],
        1: [6, 8],
        2: [7, 9],
        3: [4, 8],
        4: [0, 3, 9],
        6: [0, 1, 7],
        7: [2, 6],
        8: [1, 3],
        9: [2, 4]
        }
    return random.choice(dict_move[current_position])

##for i in range(5):
##    print(next_move(i))
##for i in range(6,10):
##    print(next_move(i))

def move_sum(total_step):
    """
    return a tuple with sum of move and last move position
    """
    if total_step == 0:
        return (0, 0)
    else:
        last_sum = move_sum(total_step - 1)
        current_position = next_move(last_sum[1]) 
        return (last_sum[0] + current_position, current_position)

def sum_mod_array(total_step, repeat_number, n_mod):
    """
    return a list with length of repeat_number. Elements are sum mod
    n_mod with total_step's jump.
    """
    result = []
    for i in range(repeat_number):
        result.append(move_sum(total_step)[0] % n_mod)
    return result

##print(sum_mod_array(10, 10, 10))
##test = sum_mod_array(10, 1000000, 10)
##print(np.mean(test))
##print(np.std(test))

##print(sum_mod_array(10, 1000000, 5 * 7).count(0))
##print(sum_mod_array(1024, 10000, 23 * 29).count(0))
##    print(move_sum(1024)[0]%1024)

import cProfile
def test():
    test_array = sum_mod_array(10, 10000, 10)
    print(np.mean(test_array))
    print(np.std(test_array))
    return

test()
cProfile.run('sum_mod_array(10, 100000, 10)')
