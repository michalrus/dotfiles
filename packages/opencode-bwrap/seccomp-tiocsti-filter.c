#include <errno.h>
#include <asm/termbits.h>  // TIOCSTI
#include <sys/ioctl.h>     // ioctl()
#include <seccomp.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
  scmp_filter_ctx ctx = seccomp_init(SCMP_ACT_ALLOW);
  if (!ctx) {
    perror("seccomp_init");
    return 1;
  }

  // ioctl(request) is arg1 (0-based: arg0=fd, arg1=request, arg2=argp)
  //
  // Masked compare avoids the classic 64-bit bypass where high bits are set
  // but the kernel ignores them for ioctl request decoding.
  int rc = seccomp_rule_add(
    ctx,
    SCMP_ACT_ERRNO(EPERM),
    SCMP_SYS(ioctl),
    1,
    SCMP_A1(SCMP_CMP_MASKED_EQ, 0xffffffffu, (unsigned)TIOCSTI)
  );
  if (rc < 0) {
    errno = -rc;
    perror("seccomp_rule_add");
    seccomp_release(ctx);
    return 1;
  }

  rc = seccomp_export_bpf(ctx, fileno(stdout));
  if (rc < 0) {
    errno = -rc;
    perror("seccomp_export_bpf");
    seccomp_release(ctx);
    return 1;
  }

  seccomp_release(ctx);
  return 0;
}
