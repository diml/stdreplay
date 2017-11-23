#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/alloc.h>

#if defined(__APPLE__)
#include <util.h>
#else
#include <pty.h>
#endif

#include <unistd.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <errno.h>

static void copy_winsz(int fd1, int fd2)
{
  struct winsize size;

  if (ioctl(fd1, TIOCGWINSZ, &size) < 0)
    uerror("ioctl(TIOCGWINSZ)", Nothing);

  if (ioctl(fd1, TIOCSWINSZ, &size) < 0)
    uerror("ioctl(TIOCSWINSZ)", Nothing);
}

CAMLprim value stdreplay_forkpty()
{
  int master;
  pid_t pid;
  struct winsize size;
  struct termios term;

  if (tcgetattr(0, &term) < 0)
    uerror("tcgetattr", Nothing);

  if (ioctl(0, TIOCGWINSZ, &size) < 0)
    uerror("ioctl(TIOCGWINSZ)", Nothing);

  pid = forkpty(&master, NULL, &term, &size);

  if (pid == (pid_t)-1)
    uerror("forkpty", Nothing);
  else if (pid == (pid_t)0)
    return Val_int(0);
  else {
    value res;
    res = caml_alloc_tuple(2);
    Field(res, 0) = Val_long(pid);
    Field(res, 1) = Val_int(master);
    return res;
  }
}
