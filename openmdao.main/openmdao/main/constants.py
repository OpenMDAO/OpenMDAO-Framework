"""
Various constants/enumerations.
"""

# Run completion status.

RUN_UNKNOWN     = -1
RUN_OK          = 0
RUN_FAILED      = 1
RUN_STOPPED     = 2
RUN_INTERRUPTED = 3

# Execution states.
STATE_UNKNOWN = -1
STATE_IDLE    = 0
STATE_RUNNING = 1
STATE_WAITING = 2

# Save formats.
SAVE_YAML    = 1
SAVE_LIBYAML = 2
SAVE_PICKLE  = 3
SAVE_CPICKLE = 4

