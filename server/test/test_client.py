#!/usr/bin/env python3

import socket
import sys, select

HOST = "100.81.39.60"  # The server's hostname or IP address
PORT = 1234         # The port used by the server

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
  s.connect((HOST, PORT))
  welcome_msg = s.recv(1024)
  print(welcome_msg.decode())
  corpus = s.recv(1024)
  print(corpus.decode())
  while True:
    msg_sent = False
    i, o, e = select.select([sys.stdin], [], [], 0.05)
    if i:
      msg = sys.stdin.readline()
      if len(msg) != 0:
        s.sendall(msg.encode())
        msg_sent = True
    data = s.recv(1024).decode()
    if data == "Game Over":
      break
    elif msg_sent:
      print(f"Received {data}")
