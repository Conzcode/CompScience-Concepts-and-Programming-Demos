# Example code for a function that uses Newton's method of successive
# approximation to find square roots and cube roots. These simple algorithms 
# demonstrate the principle of recursion. 
# Inspired by 'Structure and Interpretation of Computer Programs'

# square root ---------------------------------------------------------------

# square root function
newtSquare = function(num, guess = num) {
  # error handling
  if(num < 0) {
    stop('Enter positive numbers only')
  }
  # answers for 0
  else if(num == 0) {
    return(0)
  }
  # implement method. If desired level of precision reached, return result
  else if(abs(((guess + (num / guess)) / 2) - guess) > 0.0000001) {
    guess = (guess + (num / guess)) / 2
    # self-call
    newtSquare(num, guess)
  }
  else {return(guess)}
}

# proof it works
round(sqrt(2), 6) == round(newtSquare(2), 6)
round(sqrt(0.000234), 6) == round(newtSquare(0.000234), 6)

# cube root -----------------------------------------------------------------

# create cube root function
newtCube = function(num, guess = num) {
  if(num = 0) {
    return(0)
  }
  # implement method to desired degree of precision
  if(abs(((num / (guess * guess) + 2 * guess) / 3) - guess) > 0.0000001) {
    guess = (num / (guess * guess) + 2 * guess) / 3
    newtCube(num, guess)
  }
  else {return(guess)}
}

# proof it works
round(34758473 ^ (1/3), 6) == round(newtCube(34758473), 6)
round(-10 ^ (1/3), 6) == round(newtCube(-10), 6)

# lexical scoping demonstration ---------------------------------------------

# square root method (for positive numbers only) using block structure
# function contains a function that is locally defined.
newtBlock = function(num, guess = num) {
  iter = function(num, guess) {
    guess = (num / (guess * guess) + 2 * guess) / 3
    newtBlock(num, guess)
  }
  if(abs(((num / (guess * guess) + 2 * guess) / 3) - guess) > 0.0000001) {
    iter(num, guess)
  } else {
    return(guess)
  }
}

newtBlock(5)

# iter function will not work if called outside of newtBlock function
iter(5)

