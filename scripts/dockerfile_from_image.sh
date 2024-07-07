#!/usr/bin/env bash
docker history --format {{.CreatedBy}} --no-trunc=true $1 |sed "s?/bin/sh\ -c\ \#(nop)\ ??g"|sed "s?/bin/sh\ -c?RUN?g" | tac
