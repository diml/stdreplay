#include <util.h>

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/alloc.h>

CAMLprim value stdreplay_forkpty()
{
  int master;
  pid_t pid;
  value res;

  pid = forkpty(&master, NULL, NULL, NULL);

  if (pid == (pid_t)-1)
    uerror("forkpty", Nothing);
  else if (pid == (pid_t)0)
    return Val_int(0);
  else {
    res = caml_alloc_tuple(2);
    Field(res, 0) = Val_long(pid);
    Field(res, 1) = Val_int(master);
    return res;
  }
}
