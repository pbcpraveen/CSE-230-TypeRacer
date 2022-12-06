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

## Milestone 3

1. What is the architecture of your application (the key components)? There are two main parts of the application: the client and the server.

   The client is responsible for displaying the game interface and sending requests to the server. The server is responsible for managing the game state and sending updates to the clients.
   
   In particular, the server has a game loop that runs every 100ms. It multiplexes among all the connected clients and checks on each client if they have typed anything. If it does not receive a ping from a client in 10ms it moves on to the next. This is how we are able to get around concurency and syncing the state of the game across multiple worker threads. The server is also responsible for generating text in the beginning and sending updates to the clients every cycle.

   The client is run on brick and has 2 events that it listens to: keyboard events and a custom timer interrupt event. The keyboard events are used to update what the user has typed and display the diff text on the screen. The timer interrupt event is used to receive updates periodically from the server and update the progress bars on the screen.

2. What challenges (if any) did you have so far and how did you solve them?

   - Initially, the biggest challenge was to figure out the architecture of the program. Since it has multiple clients, we had to figure out how to collect each client's state and sync their data on the server side to share the same game state with all the clients. We settled on multiplexing in the end.
   - Another hurdle was to figure out how to make the client respond to both keyboard events but also network events. This is important because suppose we only listen to keyboard events, then the client will not be able to receive updates from the server. When it is not typing. We solved this by using a custom timer interrupt event that is triggered every 100ms. When the timer fires, we check on the server for information with a timeout.
   -

3. Do you expect to meet your goals until the deadline?

   Yes. It is done now haha : )

4. If not, how will you modify your goals?

   N/A
