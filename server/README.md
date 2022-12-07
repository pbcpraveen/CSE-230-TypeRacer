# Backend

The logic of the backend goes like this:
1. First, it waits for a specified amount of user to join. Currently, it's hardcoded to 2 for testing but we can make it a cmd arg.
2. After the specified number of users have joined, we send everyone text to type.
3. While the clients are typing, they send their progress to the backend. Here we collect all the clients' progess in a dictionary and boadcast the progress every once in a while.
4. The collection works like this: for each client, we wait a certain amount of time to receive their progress (0.1 second). If nothing is received, we don't update their progress. Otherwise, we collect their progress. This gets around the blocking receive without the need for multithreading.
5. Boardcasting is done every cycle of the game loop. Currently each cycle takes a bit over 1 second depending on how many clients join and how fast they type. The worst case scenario is that none of the clients are typing, in which case we have to wait for 1+0.1*number of clients for each broadcast to run.

Try the test client in `test/test_client.py`. The interface is not pretty but once you start 2 clients (and the server), you will receive the text to type. After that, you can type in a real number between 0 and 1 to update your typing progress.


# Front-end

The Following steps summarize what happens in the front-end.

1. Get the server-IP to connect to.

2. Wait for other clients to join the game. 

3. Get the corpus to type. 

4. Start the game. 

5. Game exits when all clients have finished. 

6. Get final report.
