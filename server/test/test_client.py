#!/usr/bin/env python3

import socket
import sys, select

HOST = "127.0.0.1"  # The server's hostname or IP address
PORT = 1234         # The port used by the server

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
    s.connect((HOST, PORT))
    welcome_msg = s.recv(1024)
    print(welcome_msg)
    corpus = s.recv(1024)
    print(corpus)
    while True:
        i, o, e = select.select([sys.stdin], [], [], 5)
        if i:
            msg = sys.stdin.readline()
            if len(msg) != 0:
                s.sendall(msg.encode())
        data = s.recv(1024)
        print(f"Received {data!r}")
