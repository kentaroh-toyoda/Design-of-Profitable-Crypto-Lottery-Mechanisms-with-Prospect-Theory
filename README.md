# Overview

This contains R programs and docker files to reproduce the results of our paper "Web3 Meets Behavioral Economics: An Example of Profitable Crypto Lottery Mechanism Design" submitted to IEEE BRAINS 2022.

# Prerequisites

- `docker`
- `docker-compose`

# Usage

- Specify an R program (e.g. `utility_comparison.R`) in the `command` of `docker-compose.yml`.
- Run `docker-compose up --build`, and graphs will be stored in `./plots` which is accessible from the host side.
