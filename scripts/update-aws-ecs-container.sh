#!/usr/bin/env bash

# ############################################################ #
#   ____  _          _ _    ___        _   _                   #
#  / ___|| |__   ___| | |  / _ \ _ __ | |_(_) ___  _ __  ___   #
#  \___ \| '_ \ / _ \ | | | | | | '_ \| __| |/ _ \| '_ \/ __|  #
#   ___) | | | |  __/ | | | |_| | |_) | |_| | (_) | | | \__ \  #
#  |____/|_| |_|\___|_|_|  \___/| .__/ \__|_|\___/|_| |_|___/  #
#                               |_|                            #
# ############################################################ #

set -o pipefail  # trace ERR through pipes
set -o nounset   # set -u : exit the script if you try to use an uninitialised variable
set -o errexit   # set -e : exit the script if any statement returns a non-true return value

nom build .#packages.aarch64-linux.haskbike-docker --builders "${NIX_AARCH64_BUILDER}" --fallback
IMAGE="$(docker load --quiet --input ./result | grep -oP '(?<=Loaded image: ).*')"
docker tag "${IMAGE}" "${DOCKER_TAG}"
docker push "${DOCKER_TAG}"
