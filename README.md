# Chapter 23: Making a System with OTP

This is one possible implementation of the fault-tolerant `prime_tester_server` system outlined in the exercises at the end of Chapter 23 in the book _Programming Erlang_ 2/ed by Joe Armstrong (Pragmatic, 2013).

Before tackling the exercises, the `sellaprime` application, which is the subject of Chapter 23, was coded and tested.

The first exercise was trivial. The second introduced the subject of process pools, for which no background is given in the book. Pools are covered in Chapter 18 of _Learn You Some Erlang for Great Good!_ by Fred H&eacute;bert (No Starch Press, 2013). Although the details do not quite match the specifications of the exercise, I used the book to become acquainted with the issues. Note that all of the client functions are asynchronous.

Furthermore, I assumed that the queue server should consume the prime tests performed by the tester servers. I suppose the results could have been sent back to the shell, but having the queue server print them seemed more convenient for testing purposes.

I believe exercise 3 includes a _trick specification_. Having the prime tester servers maintain their own queue of requests does not require updating a queue object in the server state. The server message queue serves this purpose (unless I am missing something).

Another aspect of exercise 3 is not clear, namely, "Write a load balancer that keeps track of the work being done and requests to be done by the prime server testers." I assume it means to keep a list of the integers being tested by each tester server. The load on the server is just the length of this list. However, in this solution, the load will be updated explicitly rather than getting the length of the list.

To begin exercise 4, I decided to follow the advice given on page 232 of _Erlang Programming_ by Cesarini and Thompson (O'Reilly, 2009) and organize all the database table functions in one module, called `lib_tester_db`.

To test that the fault-tolerance code is working, first comment out the line `lib_tester_db:decrement_load(Name, K)` in the `load_balancer` module. Then, execute `load_balancer:dispatch_test_async(K)` numerous time. View the contents of the `ets` table with `tv:start()`. Make a note of the requests processed by one of the testers, for example, `prime_tester7`. When you kill that tester with `exit(whereis(prime_tester7), kill)`, the restart logic should cause the tester to recalculate its original requests. Killing the `load_balancer` will cause all the testers to recalculate their original requests.

To achieve replication for exercise 5, the `ets` and `dets` tables were replaced with `mnesia`. Mnesia is covered in Chapter 20 of _Programming Erlang_, Chapter 13 of _Erlang Programming_, and Chapter 29 of _Learn You Some Erlang for Great Good!_.

I created two nodes, short names `main@gilda` and `backup@gilda`, both running from the same directory. To connect the nodes, I executed `net_adm:ping(backup@gilda)` from `main@gilda`. The one-time schema setup and table creation were done outside the scope of the `sellaprime` application. From `main@gilda`

	(main@gilda)3> mnesia:create_schema([node()|nodes()]).
	ok
	(main@gilda)4> rr(lib_tester_db).
	[state]
	(main@gilda)5> rpc:multicall([node()|nodes()], application,start,[mnesia]).
	{[ok,ok],[]}
	(main@gilda)6> mnesia:create_table(state, [{disc_copies, [node()]}, {disc_only_copies, nodes()}, {attributes, record_info(fields, state)}]).
	{atomic,ok}

Afterwards, the `init_tables/1` function in the `lib_tester_db` module was revised to just call `mnesia:wait_for_tables/2` and the others were converted to _dirty_ Mnesia operations. These operations could be distributed among the `prime_tester_server` and `load_balancer` modules using transactions, but that was not done.

In exercise 6, I converted the table copy type on node `backup@gilda` from `disc_only_copies` to `disc_copies`.

	(main@gilda)57> mnesia:change_table_copy_type(state, backup@gilda, disc_copies).
	{atomic,ok}

With this setup, I was able to halt one of the nodes and resume on the other. Commenting-out the call to `lib_tester_db:decrement_load/2` in the `load_balancer` module, as before, helps to demonstrate proper operation. When the app is manually restarted on the other node, all the requests performed on the halted node are rerun. As takeover and failover (and distributed OTP in general) are not covered in the text, I did not try to fulfill the problem specification in that way.
