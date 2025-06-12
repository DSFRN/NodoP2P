MAKEFLAGS += --no-print-directory
SRC_DIR = ./src/
SRC = $(wildcard $(SRC_DIR)*.erl)
BEAM_DIR = ./beams/
DUMP_DIR = ./dumps/
START_SERVER = -run nodo start_server
NAME = -sname nodo_18

all:
	@ ./bin/run.sh

compile:
	@ mkdir -p $(BEAM_DIR) ;\
	  erlc -o $(BEAM_DIR) $(SRC)

move_dumps:
	@ mkdir -p $(DUMP_DIR) ;\
		find . -name '*.dump' ! -path "$(DUMP_DIR)*" -exec mv {} $(DUMP_DIR) \;
