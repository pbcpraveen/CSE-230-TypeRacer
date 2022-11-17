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
