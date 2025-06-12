#!/bin/bash
set -e
ulimit -c 0

BEAM_DIR="./beams"
START_SERVER="-run nodo start_server"
NAME="-sname nodo_18"

make compile
trap 'make move_dumps' EXIT
{
  erl -pa $BEAM_DIR $NAME $START_SERVER
}
