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
                         if (N > size()) stop("insufficient items in queue!")
                         #
                         index <- seq_len(N)
                         #
                         value <- data[index]
                         data[index] <<- NULL
                         #
                         if (length(value) == 0) value = NULL
                         if (length(value) == 1) value = value[[1]]
                         #
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
#' @field prioritise Function to calculate priorities from items.
#' @seealso \code{\link{Queue-class}} for information on base class.
#' @importFrom methods new
#' @exportClass PriorityQueue
#' @export PriorityQueue
PriorityQueue <- setRefClass("PriorityQueue",
                             contains = "Queue",
                             fields = list(
                               priorities = "numeric",
                               prioritise = "function"
                             ),
                             methods = list(
                               sort_ = function() {
                                 order = order(priorities, decreasing = TRUE, partial = size():1)
                                 #
                                 data <<- data[order]
                                 priorities <<- priorities[order]
                               },
                               push = function(item, priority = NULL) {
                                 'Inserts element into the queue, reordering according to priority.'
                                 callSuper(item)
                                 #
                                 if (is.null(priority)) priority = prioritise(item)
                                 #
                                 priorities <<- c(priorities, priority)
                                 #
                                 sort_()
                               },
                               pop = function(N = 1) {
                                 # 'Removes and returns head of queue (or raises error if queue is empty).'
                                 if (N > size()) stop("insufficient items in queue!")
                                 priorities <<- priorities[-seq_len(N)]
                                 callSuper(N)
                               },
                               initialize = function(prioritise = NULL, ...) {
                                 'Creates a PriorityQueue object.'
                                 callSuper(...)
                                 #
                                 if (is.null(prioritise)) 
                                   .self$prioritise = function(x) 0
                                 else
                                   .self$prioritise = prioritise
                                 #
                                 .self
                               }
                             )
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