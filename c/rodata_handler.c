#define _GNU_SOURCE
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "rodata_handler.h"

#include "util/kvec.h"

_Noreturn void scm_runtime_error0(char *msg);

typedef struct {
  void *start;
  void *end;
} range;

kvec_t(range) ranges;

#ifndef __APPLE__
static void find_rodata_range() {
  kv_init(ranges);

  FILE *maps = fopen("/proc/self/maps", "r");
  if (!maps) {
    perror("fopen");
    exit(1);
  }

  char line[256];
  void *start;
  void *end;
  char perms[5];

  while (fgets(line, sizeof(line), maps)) {
    sscanf(line, "%p-%p %4s %*s %*s %*s %*s", &start, &end, perms);

    // Looking for read-only, non-executable sections
    if (strcmp(perms, "r--p") == 0) {

      auto r = (range){start, end};
      kv_push(ranges, r);
    }
  }
  fclose(maps);

  if (kv_size(ranges) == 0) {
    fprintf(stderr, "Failed to find .rodata section\n");
    exit(1);
  }
}

static void sigsegv_handler(int sig, siginfo_t *info, void *context) {
  void *fault_addr = info->si_addr;

  for (size_t i = 0; i < kv_size(ranges); i++) {
    auto r = kv_A(ranges, i);
    if (fault_addr >= r.start && fault_addr < r.end) {
      // Reset the handler so it can run again after scm_runtime_error
      sigset_t unblocked;
      sigemptyset(&unblocked);
      sigaddset(&unblocked, SIGSEGV);
      sigprocmask(SIG_UNBLOCK, &unblocked, nullptr);

      scm_runtime_error0("Caught write to read-only data");
    }
  }
  printf("Continue\n");

  // Not a .rodata write, restore default handler and re-raise signal
  signal(SIGSEGV, SIG_DFL);
  raise(SIGSEGV);
}
#endif
void setup_sigsegv_handler() {
#ifdef __APPLE__
  // Disable SIGSEGV handler on macOS
  return;
#else
  find_rodata_range();

  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = sigsegv_handler;
  sigemptyset(&sa.sa_mask);
  if (sigaction(SIGSEGV, &sa, nullptr) == -1) {
    perror("sigaction");
    exit(1);
  }
#endif
}
