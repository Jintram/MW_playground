
import pytest

# test-driven development

def fizzbuzz(number):
    pass

def fizzbuzz2(number):
    if number < 1:
        raise ValueError('number must be greater than 0')
    
    if number % 3 == 0 and number % 5 == 0:
        return 'fizzbuzz'
    if number % 3 == 0:
        return 'fizz'
    if number % 5 == 0:
        return 'buzz'
    
    return number
    
def test_fizzbuzz():
    #assert fizzbuzz(1) == 1
    #assert fizzbuzz(3) == 'fizz'
    #assert fizzbuzz(5) == 'buzz'
    #assert fizzbuzz(15) == 'fizzbuzz'
        
    assert fizzbuzz2(1) == 1
    assert fizzbuzz2(3) == 'fizz'
    assert fizzbuzz2(5) == 'buzz'
    assert fizzbuzz2(15) == 'fizzbuzz'
    
    # use pytest.raises to test for exceptions
    # this tests whether a particular error 
    # is raised in the code within the "with" block
    with pytest.raises(ValueError):
        fizzbuzz2(0)
