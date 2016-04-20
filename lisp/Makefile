CC = gcc
CFLAGS = -Wall -c -g
LDFLAGS = 
target = book_code

all: $(target)

.PHONY: clean
clean:
	$(RM) *.o $(target)

$(target): *.o
	$(CC) -o$@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) $<
