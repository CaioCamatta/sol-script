TARGET = delta
CC = gcc -std=c17
CFLAGS = -Wall
OBJ = obj
SRC = src
SOURCES := $(wildcard $(SRC)/*.c)
OBJECTS := $(patsubst $(SRC)/%.c, $(OBJ)/%.o, $(wildcard $(SRC)/*.c)) 

all: $(TARGET)

debug: CFLAGS += -pg
debug: $(TARGET)

$(TARGET): $(SRC) $(OBJ) $(OBJECTS)
	$(CC) $(OBJECTS) -o $@

$(OBJ):
	mkdir -p $(OBJ)

$(OBJ)/%.o:	$(SRC)/%.c
	$(CC) $(DEPFLAGS) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

clean:
	$(RM) $(OBJECTS)
	$(RM) $(TARGET)