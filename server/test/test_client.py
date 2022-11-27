#!/usr/bin/env python3

import socket

HOST = "127.0.0.1"  # The server's hostname or IP address
PORT = 1234         # The port used by the server

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
    s.connect((HOST, PORT))
    welcome_msg = s.recv(1024)
    print(welcome_msg)
    corpus = s.recv(1024)
    print(corpus)
    for i in range(5):
        msg = input(f"${i} ")
        s.sendall(msg.encode("ascii"))
        data = s.recv(1024)
        print(f"Received {data!r}")
