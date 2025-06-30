# usually tests go in separte folder called tests and files
# run this by 
# pytest code-testing.py


def add(a, b):
    return a - b

def test_add():
    assert add(2,3) == 5
    assert add('space', 'ship') == 'spaceship'
    
    # suggestions by co-pilot
    assert add(1, 2) == 3
    assert add(0, 0) == 0
    assert add(-1, 1) == 0
    assert add(1, -1) == 0
    assert add(-1, -1) == -2
    assert add(1, 0) == 1
    assert add(0, 1) == 1
    assert add(0, -1) == -1
    assert add(-1, 0) == -1




# /Users/m.wehrens/Documents/git_repos/_UVA/2024escience-goodpractice/pytest-example

