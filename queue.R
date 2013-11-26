# REFERENCES:
#
# * http://adv-r.had.co.nz/R5.html [reference classes]
# * http://www.inside-r.org/r-doc/methods/ReferenceClasses [reference classes]
# * http://stackoverflow.com/questions/11789038/does-r-have-a-priority-queue-like-javas-priorityqueue
#
# ---------------------------------------------------------------------------------------------------------------------

library(iterators)
library(itertools)

# BASIC QUEUE ---------------------------------------------------------------------------------------------------------

# Create a generator object
#
Queue <- setRefClass(Class = "Queue",
                     fields = list(
                       name = "character",
                       data = "list"
                     ),
                     methods = list(
                       size = function() {
                         'Returns the number of items in the queue.'
                         return(length(data))
                       },
                       empty = function() {size() == 0},
                       #
                       push = function(item) {
                         'Inserts element at back of the queue.'
                         data[[size()+1]] <<- item
                       },
                       #
                       pop = function() {
                         'Removes and returns head of queue (or raises error if queue is empty).'
                         if (size() == 0) stop("queue is empty!")
                         value <- data[[1]]
                         data[[1]] <<- NULL
                         value
                       },
                       #
                       poll = function() {
                         'Removes and returns head of queue (or NULL if queue is empty).'
                         if (size() == 0) return(NULL)
                         else pop()
                       },
                       #
                       peek = function(pos = c(1)) {
                         'Returns (but does not remove) specified positions in queue (or NULL if any one of them is not available).'
                         if (size() < max(pos)) return(NULL)
                         #
                         if (length(pos) == 1) return(data[[pos]])
                         else return(data[pos])
                       },
                       iterator = function() {
                         return(ihasNext(data))
                       },
                       initialize=function(...) {
                         callSuper(...)
                         #
                         # Initialise fields here (place holder)...
                         #
                         .self
                       }
                     )
)

# PRIORITY QUEUE ------------------------------------------------------------------------------------------------------

# According to Wikipedia:
#
# In computer science/data structure, a priority queue is an abstract data type which is like a regular queue or stack
# data structure, but where additionally each element has a "priority" associated with it. In a priority queue, an
# element with high priority is served before an element with low priority. If two elements have the same priority,
# they are served according to their order in the queue.

# Items are sorted at insertion and, as a result, this operation takes O(n log n).
#
# The call to sort() needs to accomplish two things:
#
# 1. reorder priorities if they differ and
# 2. leave in order of insertion if priorities are the same.
#
# See Wikipedia definition for why this is necessary.
#
PriorityQueue <- setRefClass("PriorityQueue",
                             contains = "Queue",
                             fields = list(
                               priorities = "numeric"
                             ),
                             methods = list(
                               push = function(item, priority) {
                                 'Inserts element into the queue, reordering according to priority.'
                                 callSuper(item)
                                 priorities <<- c(priorities, priority)
                                 #
                                 order = order(priorities, decreasing = TRUE, partial = size():1)
                                 #
                                 data <<- data[order]
                                 priorities <<- priorities[order]
                               },
                               #
                               pop = function() {
                                 'Removes and returns head of queue (or raises error if queue is empty).'
                                 if (size() == 0) stop("queue is empty!")
                                 priorities <<- priorities[-1]
                                 callSuper()
                               })
)