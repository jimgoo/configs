#!/usr/bin/env bash

ENV_FILE=${1}

# Show env vars
grep -v '^#' ${ENV_FILE}

# Export env vars
export $(grep -v '^#' ${ENV_FILE} | xargs)

