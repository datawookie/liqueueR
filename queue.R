# REFERENCES:
#
# * http://adv-r.had.co.nz/R5.html [reference classes]
# * http://stackoverflow.com/questions/11789038/does-r-have-a-priority-queue-like-javas-priorityqueue
#
# ---------------------------------------------------------------------------------------------------------------------

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
                       peek = function() {
                         'Returns (but does not remove) the head of queue (or NULL if queue is empty).'
                         if(size() == 0) return(NULL)
                         else return(data[[1]])
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

# Check out methods of generator object
Queue$new()
Queue$methods()
Queue$fields()
Queue$help()
Queue$help(push)

q1 <- Queue$new()
q1$name <- "test queue"
q1$name

q1

q2 <- Queue$new(name = "another test")
q2$name

# Set up some accessors for the name field
#
Queue$accessors("name")

q2$getName()
q2$setName("a new name")
q2$getName()

q2$push("item number one")
q2$push(2)
q2$push("third item")
q2$size()
q2$data

q2$pop()
q2$pop()
q2$size()

q2$peek()
q2$size()

q2$poll()
q2$size()
try(q2$pop())
q2$peek()
q2$poll()

# PRIORITY QUEUE ------------------------------------------------------------------------------------------------------

# SOME NOTES:
#   SOME NOTES:
#   SOME NOTES:
#   SOME NOTES:
#   
#   stuff from http://stackoverflow.com/questions/11789038/does-r-have-a-priority-queue-like-javas-priorityqueue
#   
# You could probably create this quite easily yourself, either using classes (Reference classes fit best),
# or using a data.frame with a custom type, combined with some functions that operate on it
# (add_to_queue(element, queue_object, priority), get_item(queue_object)). These functions would be the methods
# in case of the reference class. I like the reference class solution better as it stores both the state and the
# logic in one place.
# 
# You can use the following implementation from Rosetta Code, but beware that insertion takes O(n log n)
# 
# PriorityQueue <- function() {
#   keys <<- values <<- NULL
#   insert <- function(key, value) {
#     temp <- c(keys, key)
#     ord <- order(temp)
#     keys <<- temp[ord]
#     values <<- c(values, list(value))[ord]
#   }
#   pop <- function() {
#     head <- values[[1]]
#     values <<- values[-1]
#     keys <<- keys[-1]
#     return(head)
#   }
#   empty <- function() length(keys) == 0
#   list(insert = insert, pop = pop, empty = empty)
# }

PriorityQueue <- setRefClass("PriorityQueue", contains = "Queue")