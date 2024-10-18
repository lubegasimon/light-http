.PHONY: build clean client server

build:
	dune build

clean:
	dune clean

client:
	dune exec client/bin/app.exe 

server:
	dune exec server/server.exe 