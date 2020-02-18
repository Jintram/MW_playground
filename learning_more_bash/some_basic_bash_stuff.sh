#!/bin/bash

# testing some bash stuff

# define param
hoi=1

echo $hoi

# To halt execution of the script and exit it, use "exit 1"
#exit 1

echo $hoi 2

# outputting to files, >> appends
echo list of stuff > mytest.out
echo $hoi $hoi >> mytest.out
echo some other random stuff >> mytest.out

# the and sign seems to execute stuff simultaneously;
# without the wait, processes are ran in the background
# and execution of next commands is continued. (At least, that
# seems to be the behavior I observe from below code.)
echo hoi & sleep 2 &
echo hoi &
wait
