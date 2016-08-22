# REFERENCES:
#
# * http://adv-r.had.co.nz/R5.html [reference classes]
# * http://www.inside-r.org/r-doc/methods/ReferenceClasses [reference classes]
# * http://www.milanor.net/blog/?p=1234 [S4 classes]
# * http://stackoverflow.com/questions/11789038/does-r-have-a-priority-queue-like-javas-priorityqueue
# * http://fishyoperations.com/2016/04/11/queuing-systems.html

# BASIC QUEUE ---------------------------------------------------------------------------------------------------------

#' A Queue reference class
#' @field data Initial data to populate the queue.
#' @import itertools
#' @importFrom methods new
#' @exportClass Queue
#' @export Queue
#' @examples
#' queue <- Queue$new()
#' queue$push("one")
#' queue$push(2)
#' queue$push("three")
#' queue$size()
#' queue$pop()
#' queue$poll()
Queue <- setRefClass(Class = "Queue",
                     fields = list(
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
                       pop = function(N = 1) {
                         'Removes and returns head of queue (or raises error if queue is empty). N is number of items to pop.'
                         if (size() == 0) stop("queue is empty!")
                         if (N == 1) {
                           value <- data[[1]]
                         } else {
                           value <- data[1:N]
                         }
                         data[1:N] <<- NULL
                         return(value)
                       },
                       #
                       poll = function() {
                         'Removes and returns head of queue (or NULL if queue is empty).'
                         if (size() == 0) return(NULL)
                         else pop()
                       },
                       #
                       peek = function(pos = c(1), as.list = FALSE) {
                         'Returns (but does not remove) specified positions in queue (or NULL if any one of them is not available).
                         The as.list argument will cause a list to be returned even if only one element requested.'
                         if (size() < max(pos)) return(NULL)
                         #
                         if (length(pos) == 1 & !as.list) return(data[[pos]])
                         else return(data[pos])
                       },
                       iterator = function() {
                         return(ihasNext(data))
                       },
                       initialize = function(...) {
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

#' A PriorityQueue reference class
#' 
#' Derived from the Queue class.
#' @field data Initial data to populate the queue.
#' @field priorities Numeric queue priorities.
#' @seealso \code{\link{Queue-class}} for information on base class.
#' @importFrom methods new
#' @exportClass PriorityQueue
#' @export PriorityQueue
PriorityQueue <- setRefClass("PriorityQueue",
                             contains = "Queue",
                             fields = list(
                               priorities = "numeric"
                             ),
                             methods = list(
                               push = function(item, priority = 0) {
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
                               pop = function(N = 1) {
                                 'Removes and returns head of queue (or raises error if queue is empty).'
                                 if (size() == 0) stop("queue is empty!")
                                 priorities <<- priorities[-c(1:N)]
                                 callSuper(N)
                               })
)

# STACK ---------------------------------------------------------------------------------------------------------------

#' A Stack reference class
#' 
#' Derived from the Queue class.
#' @field data Initial data to populate the stack.
#' @seealso \code{\link{Queue-class}} for information on base class.
#' @importFrom methods new
#' @exportClass Stack
#' @export Stack
Stack <- setRefClass("Stack",
                     contains = "Queue",
                     methods = list(
                       push = function(item) {
                         'Pushes item onto the stack.'
                         data <<- c(item, data)
                       })
)