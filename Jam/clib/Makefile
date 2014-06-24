CC=clang
CFLAGS=-c -Wall
LDFLAGS=-dynamiclib -luv
SOURCES=clib.c
OBJECTS=$(SOURCES:.c=.o)
TARGET=clib.dylib


all: $(TARGET)

$(OBJECTS): $(SOURCES)
	$(CC) $(CFLAGS) $< -c -o $@

$(TARGET): $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@

clean: 
	rm *.o $(TARGET)

