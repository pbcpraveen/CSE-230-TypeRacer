# TypeRacer

A terminal-based minimal clone of [TypeRacer](https://play.typeracer.com/) built with [brick](https://github.com/jtdaugherty/brick) in Haskell.

## Proposal

### Background

TypeRacer is a popular game where a group of people in a virtual room trying to type the same body of text as fast as possible. As they type, progress bars corresponding to the percentage of text correctly typed are shown in the form of cars on a race track. Additionally, speed (words per minute) and accuracy information will be shown when a user finishes typing. ![image](https://user-images.githubusercontent.com/13091869/200763468-9f7d7c81-91e5-4363-9e5b-f1d4402fc08b.png)

### Goal

We want to recreate this online game in the terminal with the help of brick. Due to time constraints, we will obviously not try to roll out a huge server supporting hundreds of concurrent games. Instead, the game will be accessible on the local area network. The following are the details of the goal broken up into sections.

#### UI

The application will have two main pages. One is the lobby where users can create/join a room and the other is the game interface. The game interface will consist of the text each competitor is about to type as well as progress bars showing how much each competitor has typed.

#### Game Play

Users will either create or enter a room number to start a race. The instance creating a room will be the host of the room and subsequent instances joining the room will make HTTP requests to the host to 1) get the corpus that is to be typed, 2) post their percentage of text typed, and 3) get the progress of other instances. A post request will be made with each finished word.

The game starts when each joined user readies and ends when all users have finished typing or a preset timeout occurs (for example 3 minutes).

## Building and Running

tldr `make`

The above invokes `stack build` and `stack exec` if you want to do just one action.

## Overview
### Backend

The logic of the backend goes like this:
1. First, it waits for a specified amount of user to join. `stack --verbosity error exec TypeRacer-exe <numOfPlayers> <numOfWordsToType>`
2. After the specified number of users have joined, we send everyone randomized text to type.
3. While the clients are typing, they send their progress to the backend. Here we collect all the clients' progess in a dictionary and boadcast the progress every once in a while.
4. The collection works like this: for each client, we wait a certain amount of time to receive their progress (0.01 second). If nothing is received, we don't update their progress. Otherwise, we collect their progress. This gets around the blocking receive without the need for multithreading.
5. Boardcasting is done every cycle of the game loop. Currently each cycle takes a bit over 1 second depending on how many clients join and how fast they type. The worst case scenario is that none of the clients are typing, in which case we have to wait for 0.1+0.01*number of clients for each broadcast to run.

Try the test client in `test/test_client.py`. The interface is not pretty but once you start 2 clients (and the server), you will receive the text to type. After that, you can type in a real number between 0 and 1 to update your typing progress.


### Front-end

The Following steps summarize what happens in the front-end.

1. Get the server-IP to connect to.

2. Wait for other clients to join the game. 

3. Get the corpus to type. 

4. Start the game. 

5. Game exits when all clients have finished. 

6. Get final report.


## Milestone 3

1. What is the architecture of your application (the key components)? 

   There are two main parts of the application: the client and the server.

   The client is responsible for displaying the game interface and sending requests to the server. The server is responsible for managing the game state and sending updates to the clients.
   
   In particular, the server has a game loop that runs every 100ms. It multiplexes among all the connected clients and checks on each client if they have typed anything. If it does not receive a ping from a client in 10ms it moves on to the next. This is how we are able to get around concurency and the need to sync the state of the game across multiple worker threads. The server is also responsible for generating text in the beginning and sending updates to the clients every cycle.

   The client is run on brick and has 2 events that it listens to: keyboard events and a custom timer interrupt event. The keyboard events are used to update what the user has typed and display the diff text on the screen. The timer interrupt event is used to receive updates periodically from the server and update the progress bars on the screen.

2. What challenges (if any) did you have so far and how did you solve them?

   - Initially, the biggest challenge was to figure out the architecture of the program. Since it has multiple clients, we had to figure out how to collect each client's state and sync their data on the server side to share the same game state with all the clients. We settled on multiplexing in the end.
   - Another hurdle was to figure out how to make the client respond to both keyboard events but also network events. This is important because suppose we only listen to keyboard events, then the client will not be able to receive updates from the server. When it is not typing. We solved this by using a custom timer interrupt event that is triggered every 100ms. When the timer fires, we check on the server for information with a timeout.
   - One of the team members (github name: Thomasmyh) was never able to install WSL and could therefore not contribute to the repo with his account. Since it is one of the requirements for the project that everyone contributed we solved this by Praveen and Thomas working together through his computer.


3. Do you expect to meet your goals until the deadline?

   Yes. It is done now haha : )

4. If not, how will you modify your goals?

   N/A
