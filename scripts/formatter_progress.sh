#!/bin/bash

set -ex

target=$(git rev-parse --show-toplevel)/target
dir="$target/progress_projects"
mkdir -p "$dir"

# small util library
if [ ! -d "$dir/build" ]; then git clone https://github.com/pypa/build "$dir/build"; fi
# web framework that implements a lot of magic
if [ ! -d "$dir/django" ]; then git clone https://github.com/django/django "$dir/django"; fi
# an ML project
if [ ! -d "$dir/transformers" ]; then git clone https://github.com/huggingface/transformers "$dir/transformers"; fi
# type annotations
if [ ! -d "$dir/typeshed" ]; then git clone https://github.com/python/typeshed "$dir/typeshed"; fi
# python 3.11, typing and 100% test coverage
if [ ! -d "$dir/warehouse" ]; then git clone https://github.com/pypi/warehouse "$dir/warehouse"; fi
# django project
if [ ! -d "$dir/zulip" ]; then git clone https://github.com/zulip/zulip "$dir/zulip"; fi

for i in "$dir"/*/; do (cd $i && echo "# $(basename $i) $(git rev-parse HEAD)"); done
# build 5800521541e5e749d4429617420d1ef8cdb40b46
# django 0016a4299569a8f09ff24053ff2b8224f7fa4113
# transformers 5bb4430edc7df9f9950d412d98bbe505cc4d328b
# typeshed 57c435cd7e964290005d0df0d9b5daf5bd2cbcb1
# warehouse e72cca94e7ac0dbe095db5c2942ad9f2f51b30cc
# zulip 1cd587d24be1d668fcf6d136172bfec69e35cb75

cargo run --bin ruff_dev -- format-dev --multi-project "$dir" > "$target/progress_projects_report.txt"
grep "similarity index" target/progress_projects_report.txt | sort
