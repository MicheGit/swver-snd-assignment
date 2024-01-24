# A static analyzer for the While language over bounded interval domains

You can find an (almost) complete description of the implementation in `doc/report.pdf`.

This analyzer abstracts variable values into integer intervals. The user can decide the bounds of the intervals:
- with upper bound and lower bound set to +Inf or -Inf respectively, the output will be an approximation of an execution with virtually no limits to the size of the integers; 
- the user can set the upper bound of the interval as INT_MAX and the lower bound as INT_MIN of the target machine: the operations that would cause overflows will be highlighted by intervals with +Inf and/or -Inf as highest possible value or lowest respectively;
- with upper bound less than the lower bound, the analysis will consider only constant propagation (useful for automatic optimization).

