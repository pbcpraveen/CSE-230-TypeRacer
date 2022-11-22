#!/usr/bin/env python3

import socket

HOST = "127.0.0.1"  # The server's hostname or IP address
PORT = 1234         # The port used by the server

for i in range(5):
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        msg = input(f"${i} ")
        s.connect((HOST, PORT))
        s.sendall(msg.encode("ascii"))
        data = s.recv(1024)
        print(f"Received {data!r}")