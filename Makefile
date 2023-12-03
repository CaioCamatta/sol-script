# Compiler and flags
CC := gcc
CFLAGS := -std=c17 -Wall -Isrc -Isrc/util

# Directories
SRC_DIR := src
UTIL_DIR := $(SRC_DIR)/util
OBJ_DIR := obj
TEST_DIR := test
UNIT_TEST_DIR := $(TEST_DIR)/unit

# Source files
SRC_FILES := $(wildcard $(SRC_DIR)/*.c)
UTIL_FILES := $(wildcard $(UTIL_DIR)/*.c)
SRC_OBJ_FILES := $(patsubst $(SRC_DIR)/%.c, $(OBJ_DIR)/%.o, $(SRC_FILES)) 
SRC_UTIL_FILES := $(patsubst $(UTIL_DIR)/%.c, $(OBJ_DIR)/%.o, $(UTIL_FILES))
OBJ_FILES := $(SRC_OBJ_FILES) $(SRC_UTIL_FILES)

# Main executable
MAIN_EXEC := delta

# Unit test files
UNIT_TEST_SRC_FILES := $(wildcard $(UNIT_TEST_DIR)/*.c)
UNIT_TEST_EXEC := $(UNIT_TEST_DIR)/unit_test

# Targets
.PHONY: all clean test

all: $(MAIN_EXEC)

# For debugging, add a couple flags
debug: CFLAGS += -g -p
debug: $(MAIN_EXEC)

$(MAIN_EXEC): $(OBJ_FILES)
	$(CC) $(CFLAGS) $(OBJ_FILES) -o $@

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJ_DIR)/%.o: $(UTIL_DIR)/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

test: $(UNIT_TEST_EXEC)
	$(UNIT_TEST_EXEC)

$(UNIT_TEST_EXEC): $(filter-out $(SRC_DIR)/main.c, $(SRC_FILES)) $(UNIT_TEST_SRC_FILES) $(SRC_UTIL_FILES) # Everything but src/main.c
	$(CC) $(CFLAGS) -g -p $(filter-out $(SRC_DIR)/main.c, $(SRC_FILES)) $(UNIT_TEST_SRC_FILES) $(SRC_UTIL_FILES) -o $@

clean:
	rm -rf $(OBJ_DIR) $(MAIN_EXEC) $(UNIT_TEST_EXEC)
