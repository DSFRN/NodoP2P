MAKEFLAGS += --no-print-directory
SRC_DIR = ./src/
SRC = $(wildcard $(SRC_DIR)*.erl)
BEAM_DIR = ./beams/
DUMP_DIR = ./dumps/
START_SERVER = -run nodo start_server
NAME = -sname nodo_18
 
.PHONY: all compile move_dumps

all:
	@ ./bin/run.sh

compile:
	@ mkdir -p $(BEAM_DIR) ;\
		erlc -o $(BEAM_DIR) $(SRC)

move_dumps:
	@ mkdir -p $(DUMP_DIR) ;\
		find . -name '*.dump' ! -path "$(DUMP_DIR)*" -exec mv {} $(DUMP_DIR) \;

clean:
	@ rm -f $(DUMP_DIR)*.dump
	@ rm -f $(BEAM_DIR)*.beam
