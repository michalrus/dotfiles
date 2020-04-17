#include <errno.h>
#include <fcntl.h>
#include <linux/vt.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

void handle_event(char *exe, unsigned int oldev, unsigned int newev);

// Using <https://github.com/torvalds/linux/blob/c0cc271173b2e1c2d8d0ceaef14e4dfa79eefc0d/include/uapi/linux/vt.h#L66-L78> to listen for VT switches.

#define DEVICE "/dev/console"

int main (int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr,
            "Usage: %s <prog>\n" \
            "\n" \
            "  <prog> - path to an executable to call when VT switching occurs.\n" \
            "           It will get the old VT number as $1, and a new one as $2.\n",
            argv[0]);
    return -1;
  }

  // Silently reap children.
  signal(SIGCHLD, SIG_IGN);

  int fd;
  if ((fd = open(DEVICE, O_NOCTTY)) < 0) {
    perror("open(\"" DEVICE "\")");
    return -2;
  }

  for (;;) {
    struct vt_event ev;
    int rv;
    memset(&ev, 0, sizeof(ev));
    ev.event = VT_EVENT_SWITCH;

    // Ignore EINTR after resuming from sleep.
    while ((rv = ioctl(fd, VT_WAITEVENT, &ev)) < 0 && errno == EINTR)
      continue;

    if (rv < 0) {
      perror("ioctl(VT_WAITEVENT, VT_EVENT_SWITCH)");
      return -3;
    }
    handle_event(argv[1], ev.oldev, ev.newev);
  }

  return 0;
}

extern char **environ;

void handle_event(char *exe, unsigned int oldev, unsigned int newev) {
  pid_t pid = fork();

  if (pid == -1) {
    perror("fork");
    _exit(-4);
  } else if (pid > 0) {
    // In parent, just return to `main()`, as `waitpid` is unnecessary, because we ignore `SIGCHLD`.
  } else {
    // In child:
    char old_str[16], new_str[16];
    snprintf(old_str, sizeof(old_str), "%d", oldev);
    snprintf(new_str, sizeof(new_str), "%d", newev);

    char *argp[] = { exe, old_str, new_str, NULL };

    if (execve(exe, argp, environ) < 0) {
      perror("execve");
      _exit(-5);
    }
  }
}
