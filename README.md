# harvard-housing
Housing Project 2020

This project takes a look at the Harvard housing system to determine whether or not Harvard's housing process is "fair."

"Fairness" is defined as whether or not Harvard's actual housing assignments are similar enough to what we would expect
from a random process. This requires two steps [1] the creation of a random process and [2] checking whether or not 
Harvard's official housing is "similar enough" (which we also have to define).

Random process:
We collected data from first-years about personal identifiers (financial aid, legacy status, varsity status, etc.) and their
blocking group (group size, group name, group leader, and house assignment). After extensive data cleaning, we assigned
blocking groups to a new house and calculated sample statistics for that house (proportion on financial aid, with legacy status, 
or with varsity status, etc.). This random distribution process took into consideration the size of each house and assigned
houses to each block (in contrast to each person individually)

Determining "fairness":
Comparing a single random assignment to the real data would not be very valuable because it is only one data point. To 
determine whether or not housing is actually fair, we repeated this process 500 times and compared the results. We 
constructed confidence intervals for each community (a community is defined very broadly; it can refer to a neighborhood like
the quad or the river; it can also refer to a single house like Currier or Leverett). The distribution of personal 
identifiers (financial aid, legacy status, varsity status, etc.) from the official housing data was then compared to our
confidence intervals. Our results show that mostly all distributions fell within the confidence intervals.
