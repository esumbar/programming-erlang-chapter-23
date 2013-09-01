# Chapter 23: Making a System with OTP

This is one possible implementation of the fault-tolerant `prime_tester_server` system outlined in the exercises at the end of Chapter 23 in the book _Programming Erlang_ 2/ed by Joe Armstrong (Pragmatic, 2013).

Before tackling the exercises, the `sellaprime` application, which is the subject of Chapter 23, was coded and tested.

The first exercise was trivial. The second introduced the subject of process pools, for which no background is given in the book. Pools are covered in Chapter 18 of _Learn You Some Erlang for Great Good!_ by Fred H&eacute;bert (No Starch Press, 2013). Although the details do not quite match the specifications of the exercise, I used the book to become acquainted with the issues. Note that all of the client functions are asynchronous.

Furthermore, I assumed that the queue server should consume the prime tests performed by the tester servers. I suppose the results could have been sent back to the shell, but having the queue server print them seemed more convenient for testing purposes.

I believe exercise 3 includes a _trick specification_. Having the prime tester servers maintain their own queue of requests does not require updating a queue object in the server state. The server message queue serves this purpose (unless I am missing something).

Another aspect of exercise 3 is not clear, namely, "Write a load balancer that keeps track of the work being done and requests to be done by the prime server testers." I assume it means to keep a list of the integers being tested by each tester server. The load on the server is just the length of this list. However, in this solution, the load will be updated explicitly rather than getting the length of the list.

To begin exercise 4, I decided to follow the advice given on page 232 of _Erlang Programming_ by Cesarini and Thompson (O'Reilly, 2009) and organize all the database table functions in one module, called `state_db.erl`.
