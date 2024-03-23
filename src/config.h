#ifndef sol_script_config_h
#define sol_script_config_h

// We skip printing additional logs when running unit tests. This is because some of the unit tests \
// capture standard output to confirm things are being printed correctly.
#ifndef ENV_TEST

#define DEBUG_COMPILER 0
#define DEBUG_VM 1

#else

#define DEBUG_COMPILER 0
#define DEBUG_VM 0

#endif

#endif