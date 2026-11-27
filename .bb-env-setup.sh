#!/usr/bin/env bash
# mise ignores a mise.toml in a directory it hasn't been told to trust, and every new worktree is a new directory.
# Non-fatal: without mise the worktree still works, just without its own test databases.

mise trust || true
