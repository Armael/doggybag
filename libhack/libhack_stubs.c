#define _GNU_SOURCE
#define CAML_NAME_SPACE
#include <sys/mman.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/misc.h>

value caml_memfd_create(value unit) {
  int fd = memfd_create("doggybag" /* decorative */, MFD_CLOEXEC);
  return (Val_int(fd));
}

/* from ocaml otherlibs/unix/channels.c */
extern value caml_ml_open_descriptor_out(value fd); /* from runtime/io.c */
value caml_outchannel_of_file_descr(value fd) {
  return caml_ml_open_descriptor_out(fd);
}


value caml_execv_memfd(value fd, value argv) {
  CAMLparam2(fd, argv);
  char pathname[60]; /* /proc/<PID>/fd/<FD> */
  char **argv_c;

  pid_t mypid = getpid();
  snprintf(pathname, 60, "/proc/%d/fd/%d", mypid, Int_val(fd));

  int argv_size = Wosize_val(argv);
  argv_c = calloc(argv_size + 1, sizeof(char*));
  for (int i = 0; i < argv_size; i++) {
    value arg = Field(argv, i);
    argv_c[i] = String_val(arg);
  }

  execv(pathname, argv_c);
  CAMLreturn(Val_unit); /* dummy */
}

value caml_my_execv(value filename, value argv) {
  CAMLparam2(filename, argv);
  char **argv_c;

  int argv_size = Wosize_val(argv);
  argv_c = calloc(argv_size + 1, sizeof(char*));
  for (int i = 0; i < argv_size; i++) {
    value arg = Field(argv, i);
    argv_c[i] = String_val(arg);
  }

  execv(String_val(filename), argv_c);
  CAMLreturn(Val_unit); /* dummy */
}

value caml_my_putenv(value name, value val) {
  CAMLparam2(name, val);
  char* s;
  char* p;
  s = caml_stat_strconcat(3, name, "=", val);
  p = caml_stat_strdup_to_os(s);
  caml_stat_free(s);
  putenv(p);
  CAMLreturn(Val_unit);
}
