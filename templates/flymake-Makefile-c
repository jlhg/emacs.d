CC = gcc
CFLAGS = -g -lm -Wall -Wextra
SRC = src/
SRCF = example.c
OUTEXE = example

all:
	$(CC) -o $(OUTEXE) $(foreach f,$(SRCF),$(SRC)$(f)) $(CFLAGS)

check-syntax:
	$(CC) $(CFLAGS) -Wall -Wextra -fsyntax-only -S ${CHK_SOURCES}
