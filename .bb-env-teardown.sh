#!/usr/bin/env bash
# Stops this worktree's test database containers (mise.toml). docker runs them, outside the worktree, so bb's sweep of
# the worktree's processes doesn't reach them. The daemons are namespaced per worktree, so this stops no other
# checkout's databases; the containers run with --rm, so stopping also removes them.

mise daemons stop
